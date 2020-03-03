{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

-- | Framework for seeing how much a function allocates.
--
-- WARNING: weigh is incompatible with profiling. It reports much more
-- allocations with profiling turned on.
--
-- Example:
--
-- @
-- import Weigh
-- main =
--   mainWith (do func "integers count 0" count 0
--                func "integers count 1" count 1
--                func "integers count 2" count 2
--                func "integers count 3" count 3
--                func "integers count 10" count 10
--                func "integers count 100" count 100)
--   where count :: Integer -> ()
--         count 0 = ()
--         count a = count (a - 1)
-- @
--
-- Use 'wgroup' to group sets of tests.

module Weigh
  (-- * Main entry points
   mainWith
  ,weighResults
  -- * Configuration
  ,setColumns
  ,Column(..)
  ,setFormat
  ,Format (..)
  ,setConfig
  ,Config (..)
  ,defaultConfig
  -- * Simple combinators
  ,func
  ,func'
  ,io
  ,value
  ,action
  ,wgroup
  -- * Validating combinators
  ,validateAction
  ,validateFunc
  -- * Validators
  ,maxAllocs
  -- * Types
  ,Weigh
  ,Weight(..)
  -- * Handy utilities
  ,commas
  ,reportGroup
  -- * Internals
  ,weighDispatch
  ,weighFunc
  ,weighFuncResult
  ,weighAction
  ,weighActionResult
  ,Grouped(..)
  )
  where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Monad.State
import Criterion.Measurement
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.List.Split
import Data.Maybe
import qualified Data.Traversable as Traversable
import Data.Word
import GHC.Generics
import Prelude
import System.Environment
import System.Exit
import System.IO
import System.IO.Temp
import System.Mem
import System.Process
import Text.Printf
import qualified Weigh.GHCStats as GHCStats
import qualified Weigh.OsStats as OsStats

--------------------------------------------------------------------------------
-- Types

-- | Table column.
data Column
  = Case      -- ^ Case name for the column
  | Allocated -- ^ Total bytes allocated
  | GCs       -- ^ Total number of GCs
  | Live      -- ^ Total amount of live data in the heap
  | Check     -- ^ Table column indicating about the test status
  | Max       -- ^ Maximum residency memory in use
  | MaxOS     -- ^ Maximum memory in use by the RTS. Valid only for
              -- GHC >= 8.2.2. For unsupported GHC, this is reported
              -- as 0.
  | MaxRss    -- ^ Maximum residency memory in use (via OS)
  | WallTime  -- ^ Rough execution time. For general indication, not a benchmark tool.
  deriving (Show, Eq, Enum)

-- | Weigh configuration.
data Config = Config
  { configColumns :: [Column]
  , configPrefix :: String
  , configFormat :: !Format
  } deriving (Show)

data Format = Plain | Markdown
  deriving (Show)

-- | Weigh specification monad.
newtype Weigh a =
  Weigh {runWeigh :: State (Config, [Grouped Action]) a}
  deriving (Monad,Functor,Applicative)

-- | How much a computation weighed in at.
data Weight =
  Weight {weightLabel :: !String
         ,weightAllocatedBytes :: !Word64
         ,weightGCs :: !Word32
         ,weightLiveBytes :: !Word64
         ,weightMaxBytes :: !Word64
         ,weightMaxOSBytes :: !Word64
         ,weightMaxRssBytes :: !Word64
         ,weightWallTime :: !Double
         }
  deriving (Read,Show)

-- | Some grouped thing.
data Grouped a
  = Grouped String [Grouped a]
  | Singleton a
  deriving (Eq, Show, Functor, Traversable.Traversable, Foldable.Foldable, Generic)
instance NFData a => NFData (Grouped a)

-- | An action to run.
data Action =
  forall a b. (NFData a) =>
  Action {_actionRun :: !(Either (b -> IO a) (b -> a))
         ,_actionArg :: !b
         ,actionName :: !String
         ,actionCheck :: Weight -> Maybe String}
instance NFData Action where rnf _ = ()

--------------------------------------------------------------------------------
-- Main-runners

-- | Just run the measuring and print a report. Uses 'weighResults'.
mainWith :: Weigh a -> IO ()
mainWith m = do
  (results, config) <- weighResults m
  unless
    (null results)
    (do putStrLn ""
        putStrLn (report config results))
  case mapMaybe
         (\(w, r) -> do
            msg <- r
            return (w, msg))
         (concatMap Foldable.toList (Foldable.toList results)) of
    [] -> return ()
    errors -> do
      putStrLn "\nCheck problems:"
      mapM_
        (\(w, r) -> putStrLn ("  " ++ weightLabel w ++ "\n    " ++ r))
        errors
      exitWith (ExitFailure (-1))

-- | Run the measuring and return all the results, each one may have
-- an error.
weighResults
  :: Weigh a -> IO ([Grouped (Weight,Maybe String)], Config)
weighResults m = do
  args <- getArgs
  weighEnv <- lookupEnv "WEIGH_CASE"
  let (config, cases) = execState (runWeigh m) (defaultConfig, [])
  result <- weighDispatch weighEnv cases
  case result of
    Nothing -> return ([], config)
    Just weights ->
      return
        ( fmap
            (fmap
               (\w ->
                  case glookup (weightLabel w) cases of
                    Nothing -> (w, Nothing)
                    Just a -> (w, actionCheck a w)))
            weights
        , config
          { configFormat =
              if any (== "--markdown") args
                then Markdown
                else configFormat config
          })

--------------------------------------------------------------------------------
-- User DSL

-- | Default columns to display.
defaultColumns :: [Column]
defaultColumns = [Case, Allocated, GCs]

-- | Default config.
defaultConfig :: Config
defaultConfig =
  Config
  {configColumns = defaultColumns, configPrefix = "", configFormat = Plain}

-- | Set the columns to display in the config
setColumns :: [Column] -> Weigh ()
setColumns cs = Weigh (modify (first (\c -> c {configColumns = cs})))

-- | Set the output format in the config
setFormat :: Format -> Weigh ()
setFormat fm = Weigh (modify (first (\c -> c {configFormat = fm})))

-- | Set the config. Default is: 'defaultConfig'.
setConfig :: Config -> Weigh ()
setConfig = Weigh . modify . first . const

-- | Weigh a function applied to an argument.
--
-- Implemented in terms of 'validateFunc'.
func :: (NFData a)
     => String   -- ^ Name of the case.
     -> (b -> a) -- ^ Function that does some action to measure.
     -> b        -- ^ Argument to that function.
     -> Weigh ()
func name !f !x = validateFunc name f x (const Nothing)

-- | Weigh a function applied to an argument. Unlike 'func', the argument
-- is evaluated to normal form before the function is applied.
func' :: (NFData a, NFData b)
      => String
      -> (b -> a)
      -> b
      -> Weigh ()
func' name !f (force -> !x) = validateFunc name f x (const Nothing)

-- | Weigh an action applied to an argument.
--
-- Implemented in terms of 'validateAction'.
io :: (NFData a)
   => String      -- ^ Name of the case.
   -> (b -> IO a) -- ^ Aciton that does some IO to measure.
   -> b           -- ^ Argument to that function.
   -> Weigh ()
io name !f !x = validateAction name f x (const Nothing)

-- | Weigh a value.
--
-- Implemented in terms of 'action'.
value :: NFData a
      => String -- ^ Name for the value.
      -> a      -- ^ The value to measure.
      -> Weigh ()
value name !v = func name id v

-- | Weigh an IO action.
--
-- Implemented in terms of 'validateAction'.
action :: NFData a
       => String -- ^ Name for the value.
       -> IO a   -- ^ The action to measure.
       -> Weigh ()
action name !m = io name (const m) ()

-- | Make a validator that set sthe maximum allocations.
maxAllocs :: Word64 -- ^ The upper bound.
          -> (Weight -> Maybe String)
maxAllocs n =
  \w ->
    if weightAllocatedBytes w > n
       then Just ("Allocated bytes exceeds " ++
                  commas n ++ ": " ++ commas (weightAllocatedBytes w))
       else Nothing

-- | Weigh an IO action, validating the result.
validateAction :: (NFData a)
               => String -- ^ Name of the action.
               -> (b -> IO a) -- ^ The function which performs some IO.
               -> b -- ^ Argument to the function. Doesn't have to be forced.
               -> (Weight -> Maybe String) -- ^ A validating function, returns maybe an error.
               -> Weigh ()
validateAction name !m !arg !validate =
  tellAction name $ flip (Action (Left m) arg) validate

-- | Weigh a function, validating the result
validateFunc :: (NFData a)
             => String -- ^ Name of the function.
             -> (b -> a) -- ^ The function which calculates something.
             -> b -- ^ Argument to the function. Doesn't have to be forced.
             -> (Weight -> Maybe String) -- ^ A validating function, returns maybe an error.
             -> Weigh ()
validateFunc name !f !x !validate =
  tellAction name $ flip (Action (Right f) x)  validate

-- | Write out an action.
tellAction :: String -> (String -> Action) -> Weigh ()
tellAction name act =
  Weigh (do prefix <- gets (configPrefix . fst)
            modify (second (\x -> x ++ [Singleton $ act (prefix ++ "/" ++ name)])))

-- | Make a grouping of tests.
wgroup :: String -> Weigh () -> Weigh ()
wgroup str wei = do
  (orig, start) <- Weigh get
  let startL = length $ start
  Weigh (modify (first (\c -> c {configPrefix = configPrefix orig ++ "/" ++ str})))
  wei
  Weigh $ do
    modify $ second $ \x -> take startL x ++ [Grouped str $ drop startL x]
    modify (first (\c -> c {configPrefix = configPrefix orig}))

--------------------------------------------------------------------------------
-- Internal measuring actions

-- | Weigh a set of actions. The value of the actions are forced
-- completely to ensure they are fully allocated.
weighDispatch :: Maybe String -- ^ The content of then env variable WEIGH_CASE.
              -> [Grouped Action] -- ^ Weigh name:action mapping.
              -> IO (Maybe [(Grouped Weight)])
weighDispatch args cases =
  case args of
    Just var -> do
      let (label:fp:_) = read var
      let !_ = force fp
      case glookup label (force cases) of
        Nothing -> error "No such case!"
        Just act -> do
          case act of
            Action !run arg _ _ -> do
              initializeTime
              start <- getTime
              (bytes, gcs, liveBytes, maxByte, maxOSBytes, maxRssBytes) <-
                case run of
                  Right f -> weighFunc f arg
                  Left m -> weighAction m arg
              end <- getTime
              writeFile
                fp
                (show
                   (Weight
                    { weightLabel = label
                    , weightAllocatedBytes = bytes
                    , weightGCs = gcs
                    , weightLiveBytes = liveBytes
                    , weightMaxBytes = maxByte
                    , weightMaxOSBytes = maxOSBytes
                    , weightMaxRssBytes = maxRssBytes
                    , weightWallTime = end - start
                    }))
          return Nothing
    _ -> fmap Just (Traversable.traverse (Traversable.traverse fork) cases)

-- | Lookup an action.
glookup :: String -> [Grouped Action] -> Maybe Action
glookup label =
  Foldable.find ((== label) . actionName) .
  concat . map Foldable.toList . Foldable.toList

-- | Fork a case and run it.
fork :: Action -- ^ Label for the case.
     -> IO Weight
fork act =
  withSystemTempFile
    "weigh"
    (\fp h -> do
       hClose h
       setEnv "WEIGH_CASE" $ show $ [actionName act,fp]
       me <- getExecutablePath
       args <- getArgs
       (exit, _, err) <-
         readProcessWithExitCode
           me
           (args ++ ["+RTS", "-T", "-RTS"])
           ""
       case exit of
         ExitFailure {} ->
           error
             ("Error in case (" ++ show (actionName act) ++ "):\n  " ++ err)
         ExitSuccess -> do
           out <- readFile fp
           case reads out of
             [(!r, _)] -> return r
             _ ->
               error
                 (concat
                    [ "Malformed output from subprocess. Weigh"
                    , " (currently) communicates with its sub-"
                    , "processes via a temporary file."
                    ]))

-- | Weigh a pure function. This function is built on top of `weighFuncResult`,
--   which is heavily documented inside
weighFunc
  :: (NFData a)
  => (b -> a)         -- ^ A function whose memory use we want to measure.
  -> b                -- ^ Argument to the function. Doesn't have to be forced.
  -> IO (Word64,Word32,Word64,Word64,Word64,Word64) -- ^ Bytes allocated and garbage collections.
weighFunc run !arg = snd <$> weighFuncResult run arg

-- | Weigh a pure function and return the result. This function is heavily
--   documented inside.
weighFuncResult
  :: (NFData a)
  => (b -> a)         -- ^ A function whose memory use we want to measure.
  -> b                -- ^ Argument to the function. Doesn't have to be forced.
  -> IO (a, (Word64,Word32,Word64,Word64,Word64,Word64)) -- ^ Result, Bytes allocated, GCs.
weighFuncResult run !arg = do
  ghcStatsSizeInBytes <- GHCStats.getGhcStatsSizeInBytes
  performGC
     -- The above forces getStats data to be generated NOW.
  !bootupStats <- GHCStats.getStats
  !bootupTotalRssInBytes <- OsStats.getVmRss
     -- We need the above to subtract "program startup" overhead. This
     -- operation itself adds n bytes for the size of GCStats, but we
     -- subtract again that later.
  let !result = force (run arg)
  performGC
     -- The above forces getStats data to be generated NOW.
  !actionStats <- GHCStats.getStats
  !actionTotalRssInBytes <- OsStats.getVmRss
  let reflectionGCs = 1 -- We performed an additional GC.
      actionBytes =
        (GHCStats.totalBytesAllocated actionStats `subtracting`
         GHCStats.totalBytesAllocated bootupStats) `subtracting`
           -- We subtract the size of "bootupStats", which will be
           -- included after we did the performGC.
        fromIntegral ghcStatsSizeInBytes
      actionGCs =
        GHCStats.gcCount actionStats `subtracting` GHCStats.gcCount bootupStats `subtracting`
        reflectionGCs
         -- If overheadBytes is too large, we conservatively just
         -- return zero. It's not perfect, but this library is for
         -- measuring large quantities anyway.
      actualBytes = max 0 actionBytes
      liveBytes =
        (GHCStats.liveBytes actionStats `subtracting`
         GHCStats.liveBytes bootupStats)
      maxBytes =
        (GHCStats.maxBytesInUse actionStats `subtracting`
         GHCStats.maxBytesInUse bootupStats)
      maxOSBytes =
        (GHCStats.maxOSBytes actionStats `subtracting`
            GHCStats.maxOSBytes bootupStats)
      maxRssBytes = actionTotalRssInBytes `subtracting` bootupTotalRssInBytes
  return (result, (actualBytes, actionGCs, liveBytes, maxBytes, maxOSBytes, maxRssBytes))

subtracting :: (Ord p, Num p) => p -> p -> p
subtracting x y =
  if x > y
    then x - y
    else 0

-- | Weigh an IO action. This function is based on `weighActionResult`, which is
--   heavily documented inside.
weighAction
  :: (NFData a)
  => (b -> IO a)      -- ^ A function whose memory use we want to measure.
  -> b                -- ^ Argument to the function. Doesn't have to be forced.
  -> IO (Word64,Word32,Word64,Word64,Word64,Word64) -- ^ Bytes allocated and garbage collections.
weighAction run !arg = snd <$> weighActionResult run arg

-- | Weigh an IO action, and return the result. This function is heavily
--   documented inside.
weighActionResult
  :: (NFData a)
  => (b -> IO a)      -- ^ A function whose memory use we want to measure.
  -> b                -- ^ Argument to the function. Doesn't have to be forced.
  -> IO (a, (Word64,Word32,Word64,Word64,Word64,Word64)) -- ^ Result, Bytes allocated and GCs.
weighActionResult run !arg = do
  ghcStatsSizeInBytes <- GHCStats.getGhcStatsSizeInBytes
  performGC
     -- The above forces getStats data to be generated NOW.
  !bootupStats <- GHCStats.getStats
  !bootupTotalRssInBytes <- OsStats.getVmRss
     -- We need the above to subtract "program startup" overhead. This
     -- operation itself adds n bytes for the size of GCStats, but we
     -- subtract again that later.
  !result <- fmap force (run arg)
  performGC
     -- The above forces getStats data to be generated NOW.
  !actionStats <- GHCStats.getStats
  !actionTotalRssInBytes <- OsStats.getVmRss
  let reflectionGCs = 1 -- We performed an additional GC.
      actionBytes =
        (GHCStats.totalBytesAllocated actionStats `subtracting`
         GHCStats.totalBytesAllocated bootupStats) `subtracting`
           -- We subtract the size of "bootupStats", which will be
           -- included after we did the performGC.
        fromIntegral ghcStatsSizeInBytes
      actionGCs =
        GHCStats.gcCount actionStats `subtracting` GHCStats.gcCount bootupStats `subtracting`
        reflectionGCs
         -- If overheadBytes is too large, we conservatively just
         -- return zero. It's not perfect, but this library is for
         -- measuring large quantities anyway.
      actualBytes = max 0 actionBytes
      liveBytes =
        max 0 (GHCStats.liveBytes actionStats `subtracting` GHCStats.liveBytes bootupStats)
      maxBytes =
        max
          0
          (GHCStats.maxBytesInUse actionStats `subtracting`
           GHCStats.maxBytesInUse bootupStats)
      maxOSBytes =
        max
          0
          (GHCStats.maxOSBytes actionStats `subtracting`
           GHCStats.maxOSBytes bootupStats)
      maxRssBytes = actionTotalRssInBytes `subtracting` bootupTotalRssInBytes
  return (result,
    (  actualBytes
    ,  actionGCs
    ,  liveBytes
    ,  maxBytes
    ,  maxOSBytes
    ,  maxRssBytes
    ))

--------------------------------------------------------------------------------
-- Formatting functions

report :: Config -> [Grouped (Weight,Maybe String)] -> String
report config gs =
  List.intercalate
    "\n\n"
    (filter
       (not . null)
       [ if null singletons
           then []
           else reportTabular config singletons
       , List.intercalate "\n\n" (map (uncurry (reportGroup config)) groups)
       ])
  where
    singletons =
      mapMaybe
        (\case
           Singleton v -> Just v
           _ -> Nothing)
        gs
    groups =
      mapMaybe
        (\case
           Grouped title vs -> Just (title, vs)
           _ -> Nothing)
        gs

reportGroup :: Config -> [Char] -> [Grouped (Weight, Maybe String)] -> [Char]
reportGroup config title gs =
  case configFormat config of
    Plain -> title ++ "\n\n" ++ indent (report config gs)
    Markdown -> "#" ++ title ++ "\n\n" ++ report config gs

-- | Make a report of the weights.
reportTabular :: Config -> [(Weight,Maybe String)] -> String
reportTabular config = tabled
  where
    tabled =
      (case configFormat config of
         Plain -> tablize
         Markdown -> mdtable) .
      (select headings :) . map (select . toRow)
    select row = mapMaybe (\name -> lookup name row) (configColumns config)
    headings =
      [ (Case, (True, "Case"))
      , (Allocated, (False, "Allocated"))
      , (GCs, (False, "GCs"))
      , (Live, (False, "Live"))
      , (Check, (True, "Check"))
      , (Max, (False, "Max"))
      , (MaxOS, (False, "MaxOS"))
      , (MaxRss, (False, "MaxRss"))
      , (WallTime, (False, "Wall Time"))
      ]
    toRow (w, err) =
      [ (Case, (True, takeLastAfterBk $ weightLabel w))
      , (Allocated, (False, commas (weightAllocatedBytes w)))
      , (GCs, (False, commas (weightGCs w)))
      , (Live, (False, commas (weightLiveBytes w)))
      , (Max, (False, commas (weightMaxBytes w)))
      , (MaxOS, (False, commas (weightMaxOSBytes w)))
      , (MaxRss, (False, commas (weightMaxRssBytes w)))
      , (WallTime, (False, printf "%.3fs" (weightWallTime w)))
      , ( Check
        , ( True
          , case err of
              Nothing -> "OK"
              Just {} -> "INVALID"))
      ]
    takeLastAfterBk w = case List.elemIndices '/' w of
                       [] -> w
                       x  -> drop (1+last x) w

-- | Make a markdown table.
mdtable ::[[(Bool,String)]] -> String
mdtable rows = List.intercalate "\n" [heading, align, body]
  where
    heading = columns (map (\(_, str) -> str) (fromMaybe [] (listToMaybe rows)))
    align =
      columns
        (map
           (\(shouldAlignLeft, _) ->
              if shouldAlignLeft
                then ":---"
                else "---:")
           (fromMaybe [] (listToMaybe rows)))
    body =
      List.intercalate "\n" (map (\row -> columns (map snd row)) (drop 1 rows))
    columns xs = "|" ++ List.intercalate "|" xs ++ "|"

-- | Make a table out of a list of rows.
tablize :: [[(Bool,String)]] -> String
tablize xs =
  List.intercalate "\n" (map (List.intercalate "  " . map fill . zip [0 ..]) xs)
  where
    fill (x', (left', text')) =
      printf ("%" ++ direction ++ show width ++ "s") text'
      where
        direction =
          if left'
            then "-"
            else ""
        width = maximum (map (length . snd . (!! x')) xs)

-- | Formatting an integral number to 1,000,000, etc.
commas :: (Num a,Integral a,Show a) => a -> String
commas = reverse . List.intercalate "," . chunksOf 3 . reverse . show

-- | Indent all lines in a string.
indent :: [Char] -> [Char]
indent = List.intercalate "\n" . map (replicate 2 ' '++) . lines
