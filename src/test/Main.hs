{-# LANGUAGE DeriveGeneric #-}

-- | Example uses of Weigh which should work.

module Main where

import Control.DeepSeq
import Weigh
import GHC.Generics

-- | Weigh integers.
main :: IO ()
main =
  mainWith (do integers
               ints
               struct)

-- | Just counting integers.
integers :: Weigh ()
integers =
  do func "integers count 0" count 0
     func "integers count 1" count 1
     func "integers count 2" count 2
     func "integers count 3" count 3
     func "integers count 10" count 10
     func "integers count 100" count 100
  where count :: Integer -> ()
        count 0 = ()
        count a = count (a - 1)

-- | We count ints and ensure that the allocations are optimized away
-- to only two 64-bit Ints (16 bytes).
ints :: Weigh ()
ints =
  do validateFunc "ints count 1" count 1 (maxAllocs 24)
     validateFunc "ints count 10" count 10 (maxAllocs 24)
     validateFunc "ints count 1000000" count 1000000 (maxAllocs 24)
  where count :: Int -> ()
        count 0 = ()
        count a = count (a - 1)

-- | Some simple data structure of two ints.
data IntegerStruct = IntegerStruct !Integer !Integer
  deriving (Generic)
instance NFData IntegerStruct

-- | Weigh allocating a user-defined structure.
struct :: Weigh ()
struct = do func "alloc struct" (\x -> IntegerStruct x x) 5
            func "alloc struct2" (\x -> (IntegerStruct x x,IntegerStruct x x)) 5
