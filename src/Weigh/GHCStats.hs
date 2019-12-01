{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

-- | Calculate the size of GHC.Stats statically.

module Weigh.GHCStats
  (getGhcStatsSizeInBytes
  ,getStats
  ,gcCount
  ,totalBytesAllocated
  ,liveBytes
  ,maxBytesInUse
  ,maxOSBytes
  )
  where

import Data.Word
import GHC.Stats
import System.Mem

-- | Get GHC's statistics.
getStats :: IO RTSStats
getStats = getRTSStats

gcCount :: RTSStats -> Word32
gcCount = gcs

totalBytesAllocated :: RTSStats -> Word64
totalBytesAllocated = allocated_bytes

liveBytes :: RTSStats -> Word64
liveBytes = gcdetails_live_bytes . gc

maxBytesInUse :: RTSStats -> Word64
maxBytesInUse = max_live_bytes

maxOSBytes :: RTSStats -> Word64
maxOSBytes = max_mem_in_use_bytes

-- | Get the size of a 'RTSStats' object in bytes.
getGhcStatsSizeInBytes :: IO Word64
getGhcStatsSizeInBytes = do
  s1 <- oneGetStats
  s2 <- twoGetStats
  return (fromIntegral (totalBytesAllocated s2 - totalBytesAllocated s1))
  where
    oneGetStats = do
      performGC
      !s <- getStats
      return s
    twoGetStats = do
      performGC
      !_ <- getStats
      !s <- getStats
      return s
