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

import           Control.DeepSeq
import           Control.Monad.Writer
import           Data.List
import           Formatting
import           GHC.Int
import           GHC.Stats
import           System.Environment
import           System.Exit
import           System.Process

-- | Weigh specification monad.
newtype Weigh a =
  Weigh {runWeigh :: Writer [(String,Action)] a}
  deriving (Monad,Functor,Applicative)

-- | Write an action out.
action :: NFData a => String -> IO a -> Weigh ()
action name m = Weigh (tell [(name,Action m (const Nothing))])

-- | Write an action out.
check
  :: NFData a
  => String -> (Weight -> Maybe String) -> IO a -> Weigh ()
check name validate m = Weigh (tell [(name,Action m validate)])

-- | A Weight of we
data Weight =
  Weight {weightLabel :: !String
         ,weightAllocatedBytes :: !Int64
         ,weightGCs :: !Int64}
  deriving (Read,Show)

-- | An action to run.
data Action =
  forall a. NFData a => Action {_actionRun :: IO a
                               ,actionCheck :: Weight -> Maybe String}

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
            putStrLn (report results)

-- | Make a report of the weights.
report :: [(Weight,Maybe String)] -> String
report = tablize . (["Case","Bytes","GCs","Check"] :) . map toRow
  where toRow (w,err) =
          [weightLabel w
          ,formatToString commas
                          (weightAllocatedBytes w)
          ,show (weightGCs w)
          ,case err of
             Nothing -> "OK"
             Just {} -> "INVALID"]

-- | Make a table out of a list of rows.
tablize :: [[String]] -> String
tablize xs =
  intercalate "\n"
              (map (intercalate "  " . map fill . zip [0 ..]) xs)
  where fill (x',text') = take width (text' ++ repeat ' ')
          where width = maximum (map (length . (!! x')) xs)

-- | Weigh a set of actions. The value of the actions are forced
-- completely to ensure they are fully allocated.
weigh :: [String] -> [(String,Action)] -> IO (Maybe [Weight])
weigh args cases =
  case args of
    ("--case":label:_) ->
      case lookup label cases of
        Nothing -> error "No such case!"
        Just act ->
          do case act of
               Action run _ ->
                 do !_ <- fmap force run
                    stats <- getGCStats
                    print (Weight {weightLabel = label
                                  ,weightAllocatedBytes = bytesAllocated stats
                                  ,weightGCs = numGcs stats})
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
