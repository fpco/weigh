-- | Example uses of Weigh which should work.

module Main where

import Weigh

-- | Weigh integers.
main :: IO ()
main =
  mainWith (do integers
               ints)

-- | Just counting integers.
integers :: Weigh ()
integers =
  do action "integers count 0" (return (count 0))
     action "integers count 1" (return (count 1))
     action "integers count 10" (return (count 10))
     action "integers count 100" (return (count 100))
  where count :: Integer -> ()
        count 0 = ()
        count a = count (a - 1)

-- | We count ints and ensure that the allocations are optimized away
-- to only two 64-bit Ints (16 bytes).
ints :: Weigh ()
ints =
  do allocs "ints count 1" 16 (return (count 1))
     allocs "ints count 10" 16 (return (count 10))
     allocs "ints count 1000000" 16 (return (count 1000000))
  where count :: Int -> ()
        count 0 = ()
        count a = count (a - 1)
