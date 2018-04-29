{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Example uses of Weigh which should work.

module Main where

import Control.DeepSeq
import Weigh
import GHC.Generics

-- | Weigh integers.
main :: IO ()
main =
  mainWith
    (do wgroup "Integers" integers
        wgroup "IO actions" ioactions
        wgroup "Ints" ints
        wgroup "Structs" struct
        wgroup "Packing" packing)

-- | Weigh IO actions.
ioactions :: Weigh ()
ioactions =
  do action "integers count IO CAF 0" (return (count 0))
     io "integers count IO func 0" (return . count) 0
     action "integers count IO CAF 1" (return (count 1))
     io "integers count IO func 1" (return . count) 1
  where count :: Integer -> ()
        count 0 = ()
        count a = count (a - 1)

-- | Just counting integers.
integers :: Weigh ()
integers = do
  func "integers count 0" count 0
  func "integers count 1" count 1
  func "integers count 2" count 2
  func "integers count 3" count 3
  func "integers count 10" count 10
  func "integers count 100" count 100
  where
    count :: Integer -> ()
    count 0 = ()
    count a = count (a - 1)

-- | We count ints and ensure that the allocations are optimized away
-- to only two 64-bit Ints (16 bytes).
ints :: Weigh ()
ints =
  do validateFunc "ints count 1" count 1 (maxAllocs 0)
     validateFunc "ints count 10" count 10 (maxAllocs 0)
     validateFunc "ints count 1000000" count 1000000 (maxAllocs 0)
  where count :: Int -> ()
        count 0 = ()
        count a = count (a - 1)

-- | Some simple data structure of two ints.
data IntegerStruct = IntegerStruct !Integer !Integer
  deriving (Generic)
instance NFData IntegerStruct

-- | Weigh allocating a user-defined structure.
struct :: Weigh ()
struct =
  do func "\\_ -> IntegerStruct 0 0" (\_ -> IntegerStruct 0 0) (5 :: Integer)
     validateFunc "\\x -> IntegerStruct x 0" (\x -> IntegerStruct x 0) 5 (const (Just "Boo!"))
     func "\\x -> IntegerStruct x x" (\x -> IntegerStruct x x) 5
     func "\\x -> IntegerStruct (x+1) x" (\x -> IntegerStruct (x+1) x) 5
     func "\\x -> IntegerStruct (x+1) (x+1)" (\x -> IntegerStruct (x+1) (x+1)) 5
     func "\\x -> IntegerStruct (x+1) (x+2)" (\x -> IntegerStruct (x+1) (x+2)) 5

-- | A simple structure with an Int in it.
data HasInt = HasInt !Int
  deriving (Generic)
instance NFData HasInt

-- | A simple structure with an Int in it.
data HasPacked = HasPacked HasInt
  deriving (Generic)
instance NFData HasPacked

-- | A simple structure with an Int in it.
data HasUnpacked = HasUnpacked {-# UNPACK #-} !HasInt
  deriving (Generic)
instance NFData HasUnpacked

-- | Weigh: packing vs no packing.
packing :: Weigh ()
packing =
  do func "\\x -> HasInt x" (\x -> HasInt x) 5
     func "\\x -> HasUnpacked (HasInt x)" (\x -> HasUnpacked (HasInt x)) 5
     func "\\x -> HasPacked (HasInt x)" (\x -> HasPacked (HasInt x)) 5
