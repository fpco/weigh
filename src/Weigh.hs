{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

-- | Framework for seeing how much a function allocates.

module Weigh
  (-- * Main entry point.
   mainWith
   -- * Types
  ,Weigh
  ,Weight(..)
  -- * Simple combinators
  ,action
  ,func
  ,value
  -- * Validating combinators
  ,validateAction
  ,validateFunc
  -- * Validators
  ,maxAllocs
  -- * Handy utilities
  ,commas)
  where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Writer
import Data.List
import Data.List.Split
import Data.Maybe
import GHC.HeapView (getClosureRaw)
import GHC.Int
import GHC.Stats
import Prelude
import System.Environment
import System.Exit
import System.Mem
import System.Process
import Text.Printf
import Unsafe.Coerce

--------------------------------------------------------------------------------
-- Types

-- | Weigh specification monad.
newtype Weigh a =
  Weigh {runWeigh :: Writer [(String,Action)] a}
  deriving (Monad,Functor,Applicative)

-- | How much a computation weighed in at.
data Weight =
  Weight {weightLabel :: !String
         ,weightAllocatedBytes :: !Int64
         ,weightGCs :: !Int64}
  deriving (Read,Show)

-- | An action to run.
data Action =
  forall a b. (NFData a) =>
  Action {_actionRun :: !(b -> IO a)
         ,_actionArg :: !b
         ,actionCheck :: Weight -> Maybe String}

--------------------------------------------------------------------------------
-- Main-runners

-- | Just run the measuring and print a report.
mainWith :: Weigh a -> IO ()
mainWith m =
  do args <- getArgs
     let cases = execWriter (runWeigh m)
     result <- weigh args cases
     case result of
       Nothing -> return ()
       Just weights ->
         do let results =
                  map (\w ->
                         case lookup (weightLabel w) cases of
                           Nothing -> (w,Nothing)
                           Just a -> (w,actionCheck a w))
                      weights
            putStrLn ""
            putStrLn (report results)
            case mapMaybe (\(w,r) ->
                             do msg <- r
                                return (w,msg))
                          results of
              [] -> return ()
              errors ->
                do putStrLn "\nCheck problems:"
                   mapM_ (\(w,r) ->
                            putStrLn ("  " ++ weightLabel w ++ "\n    " ++ r))
                         errors
                   exitWith (ExitFailure (-1))

--------------------------------------------------------------------------------
-- User DSL

-- | Weigh a function applied to an argument.
--
-- Implemented in terms of 'validateFunc'.
func :: (NFData a) => String -> (b -> a) -> b -> Weigh ()
func name !f !x = validateFunc name f x (const Nothing)

-- | Weigh a value.
--
-- Implemented in terms of 'action'.
value :: NFData a => String -> a -> Weigh ()
value name !v = action name (return v)

-- | Weigh an IO action.
--
-- Implemented in terms of 'validateAction'.
action :: NFData a
       => String -> IO a -> Weigh ()
action name !m =
  validateAction name
                 (const m)
                 ()
                 (const Nothing)

-- | Make a validator that set sthe maximum allocations.
maxAllocs :: Int64 -> (Weight -> Maybe String)
maxAllocs n =
  \w ->
    if weightAllocatedBytes w > n
       then Just ("Allocated bytes exceeds " ++
                  commas n ++ ": " ++ commas (weightAllocatedBytes w))
       else Nothing

-- | Weigh an IO action, validating the result.
validateAction
  :: (NFData a)
  => String -> (b -> IO a) -> b -> (Weight -> Maybe String) -> Weigh ()
validateAction name !m !arg !validate =
  Weigh (tell [(name,Action m arg validate)])

-- | Weigh a function, validating the result
validateFunc
  :: (NFData a)
  => String -> (b -> a) -> b -> (Weight -> Maybe String) -> Weigh ()
validateFunc name !f !x !validate =
  Weigh (tell [(name,Action (return . f) x validate)])

--------------------------------------------------------------------------------
-- Internal measuring actions

-- | Weigh a set of actions. The value of the actions are forced
-- completely to ensure they are fully allocated.
weigh :: [String] -> [(String,Action)] -> IO (Maybe [Weight])
weigh args cases =
  case args of
    ("--case":label:_) ->
      case lookup label (deepseq (map fst cases) cases) of
        Nothing -> error "No such case!"
        Just act ->
          do case act of
               Action !run arg _ ->
                 do (bytes,gcs) <- weighAction run arg
                    print (Weight {weightLabel = label
                                  ,weightAllocatedBytes = bytes
                                  ,weightGCs = gcs})
             return Nothing
    _ -> fmap Just (mapM (fork . fst) cases)

-- | Fork a case and run it.
fork :: String -> IO Weight
fork label =
  do me <- getExecutablePath
     (exit,out,err) <-
       readProcessWithExitCode me
                               ["--case",label,"+RTS","-T","-RTS"]
                               ""
     case exit of
       ExitFailure{} -> error ("Error in case (" ++ show label ++ "):\n  " ++ err)
       ExitSuccess ->
         let !r = read out
         in return r

-- | Weigh an action.
weighAction :: (NFData a) => (b -> IO a) -> b -> IO (Int64, Int64)
weighAction !run !arg =
  do performGC
     !bootupStats <- getGCStats
     !_ <- fmap force (run arg)
     performGC
     !actionStats <- getGCStats
     reflectionBytes <- ghcStatsSize bootupStats
     let reflectionGCs = 2
         actionBytes =
           bytesAllocated actionStats - bytesAllocated bootupStats -
           reflectionBytes
         actionGCs = numGcs actionStats - reflectionGCs
         overheadBytes = 24
     return (max 0 (actionBytes - overheadBytes),actionGCs)

--------------------------------------------------------------------------------
-- Formatting functions

-- | Make a report of the weights.
report :: [(Weight,Maybe String)] -> String
report =
  tablize .
  ([(True,"Case"),(False,"Bytes"),(False,"GCs"),(True,"Check")] :) . map toRow
  where toRow (w,err) =
          [(True,weightLabel w)
          ,(False,commas (weightAllocatedBytes w))
          ,(False,commas (weightGCs w))
          ,(True
           ,case err of
              Nothing -> "OK"
              Just{} -> "INVALID")]

-- | Make a table out of a list of rows.
tablize :: [[(Bool,String)]] -> String
tablize xs =
  intercalate "\n"
              (map (intercalate "  " . map fill . zip [0 ..]) xs)
  where fill (x',(left,text')) = printf ("%" ++ direction ++ show width ++ "s") text'
          where direction = if left
                               then "-"
                               else ""
                width = maximum (map (length . snd . (!! x')) xs)

-- | Formatting an integral number to 1,000,000, etc.
commas :: (Num a,Integral a,Show a) => a -> String
commas = reverse . intercalate "," . chunksOf 3 . reverse . show

--------------------------------------------------------------------------------
-- Memory utilities

-- This used to be available via GHC.Constants
#include "MachDeps.h"
wORD_SIZE :: Int
wORD_SIZE = SIZEOF_HSWORD

-- | Calculate size of GHC objects in Bytes.
ghcStatsSize :: GCStats -> IO Int64
ghcStatsSize !x =
  do (_,y,_) <- getClosureRaw x
     return . fromIntegral $ length y * wORD_SIZE
