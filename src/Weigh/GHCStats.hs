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
  ,maxBytesInUse)
  where

#if __GLASGOW_HASKELL__ < 802
import Data.Int
#else
import Data.Int
import Data.Word
#endif
import GHC.Stats
import System.Mem

#if __GLASGOW_HASKELL__ < 802
-- | Get GHC's statistics.
getStats :: IO GCStats
getStats = getGCStats

gcCount :: GCStats -> Int64
gcCount = numGcs

totalBytesAllocated :: GCStats -> Int64
totalBytesAllocated = bytesAllocated

liveBytes :: GCStats -> Int64
liveBytes = currentBytesUsed

maxBytesInUse :: GCStats -> Int64
maxBytesInUse = maxBytesUsed

#else
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
maxBytesInUse = max_mem_in_use_bytes
#endif

-- | Get the size of a 'RTSStats' object in bytes.
getGhcStatsSizeInBytes :: IO Int64
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
