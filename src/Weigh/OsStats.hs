module Weigh.OsStats
  ( getVmRssWithError
  , getVmRss
  )
  where


import Text.Read
import Data.List
import Data.Word


getVmRss :: IO Word64
getVmRss = either (const 0) id <$> getVmRssWithError


-- | Get 'VmRSS' (resident set size) from file "/proc/self/status".
-- Returns either error message or memory size in bytes.
--
getVmRssWithError :: IO (Either String Word64)
getVmRssWithError = do
  stat <- readFile "/proc/self/status"
  return $
    case filter (isPrefixOf "VmRSS:") $ lines stat of
      [] -> Left "No VmRSS line in /proc/self/status"
      (line:_) ->
        case words line of
          "VmRSS:":sz:"kB":[] -> (* kb) <$> readEither sz
          _                   -> Left $ "Can't parse \"" ++ line ++ "\""
  where
    kb = 1024

