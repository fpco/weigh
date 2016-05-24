{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

-- | Framework for seeing how much a function allocates.

module Weigh
  (Weigh
  ,Weight(..)
  ,action
  ,check
  ,mainWith)
  where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Writer
import Data.List
import Data.List.Split
import GHC.HeapView (getClosureRaw)
import GHC.Int
import GHC.Stats
import Prelude
import System.Environment
import System.Exit
import System.Mem
import System.Process
import Text.Printf

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
  forall a. NFData a =>
  Action {_actionRun :: IO a
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

--------------------------------------------------------------------------------
-- User DSL

-- | Write an action out.
action :: NFData a => String -> IO a -> Weigh ()
action name m = Weigh (tell [(name,Action m (const Nothing))])

-- | Write an action out.
check :: NFData a => String -> (Weight -> Maybe String) -> IO a -> Weigh ()
check name validate m = Weigh (tell [(name,Action m validate)])

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
               Action !run _ ->
                 do (bytes,gcs) <- weighAction run
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
weighAction :: NFData a => IO a -> IO (Int64, Int64)
weighAction run =
  do performGC
     !bootupStats <- getGCStats
     !_ <- fmap force run
     performGC
     !actionStats <- getGCStats
     reflectionBytes <- ghcStatsSize bootupStats
     let reflectionGCs = 2
         actionBytes =
           bytesAllocated actionStats - bytesAllocated bootupStats -
           reflectionBytes
         actionGCs = numGcs actionStats - reflectionGCs
     return (actionBytes,actionGCs)

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
