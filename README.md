# weigh [![Build Status](https://travis-ci.org/fpco/weigh.png)](https://travis-ci.org/fpco/weigh)

Measures the memory usage of a Haskell value or function

## Example use

``` haskell
import Weigh

-- | Weigh integers.
main :: IO ()
main =
  mainWith (do integers
               ints)

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
```

Output results:

```
Case                Bytes  GCs  Check
integers count 0        0    0  OK
integers count 1       32    0  OK
integers count 2       64    0  OK
integers count 3       96    0  OK
integers count 10     320    0  OK
integers count 100  3,200    0  OK
ints count 1            0    0  OK
ints count 10           0    0  OK
ints count 1000000      0    0  OK
```

You can try this out with `stack test` in the `weight` directory.

## Real-world use-case

This is the output for the Store package to measure allocation of
three data structure types, and encoding them to binary with the Store
package vs the Cereal package.

We can see that the Cereal package is inefficient with allocations. It
generates 3GB, 6GB and 6.5GB for 10 million integers in `[Int]`,
`Vector Int` and `Storable.Vector Int` respectively.

```
Case                                                      Bytes     GCs  Check
   1,000,000 [Int]                Allocate           47,999,928      92  OK
   1,000,000 [Int]                Encode: Store      56,000,096      92  OK
   1,000,000 [Int]                Encode: Cereal    368,229,992     679  OK
   2,000,000 [Int]                Allocate           95,999,928     184  OK
   2,000,000 [Int]                Encode: Store     112,000,096     184  OK
   2,000,000 [Int]                Encode: Cereal    736,425,192   1,357  OK
  10,000,000 [Int]                Allocate          479,999,928     920  OK
  10,000,000 [Int]                Encode: Store     560,000,096     920  OK
  10,000,000 [Int]                Encode: Cereal  3,682,021,160   6,783  OK
   1,000,000 Boxed Vector Int     Allocate            8,007,888       1  OK
   1,000,000 Boxed Vector Int     Encode: Store      16,008,120       2  OK
   1,000,000 Boxed Vector Int     Encode: Cereal    600,238,152   1,118  OK
   2,000,000 Boxed Vector Int     Allocate           16,015,704       1  OK
   2,000,000 Boxed Vector Int     Encode: Store      32,015,936       2  OK
   2,000,000 Boxed Vector Int     Encode: Cereal  1,200,441,168   2,234  OK
  10,000,000 Boxed Vector Int     Allocate           80,078,200       1  OK
  10,000,000 Boxed Vector Int     Encode: Store     160,078,432       2  OK
  10,000,000 Boxed Vector Int     Encode: Cereal  6,002,099,632  11,168  OK
   1,000,000 Storable Vector Int  Allocate            8,000,072       1  OK
   1,000,000 Storable Vector Int  Encode: Store      16,000,224       2  OK
   1,000,000 Storable Vector Int  Encode: Cereal    656,230,304   1,222  OK
   2,000,000 Storable Vector Int  Allocate           16,000,072       1  OK
   2,000,000 Storable Vector Int  Encode: Store      32,000,224       2  OK
   2,000,000 Storable Vector Int  Encode: Cereal  1,312,425,504   2,443  OK
  10,000,000 Storable Vector Int  Allocate           80,000,072       1  OK
  10,000,000 Storable Vector Int  Encode: Store     160,000,224       2  OK
  10,000,000 Storable Vector Int  Encode: Cereal  6,562,021,472  12,215  OK
````
