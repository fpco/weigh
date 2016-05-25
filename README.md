# weigh [![Build Status](https://travis-ci.org/fpco/weigh.png)](https://travis-ci.org/fpco/weigh)

Measures the memory usage of a Haskell value or function

## Example use

```
import Weigh

-- | Weigh integers.
main :: IO ()
main =
  mainWith (do integers
               ints)

-- | Just counting integers.
integers :: Weigh ()
integers =
  do action "integers count 1" (return (count 1))
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
```

Output results:

```
Case                Bytes  GCs  Check
integers count 1       48    0  OK
integers count 10     336    0  OK
integers count 100  3,216    0  OK
ints count 1           16    0  OK
ints count 10          16    0  OK
ints count 1000000     16    0  OK
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
   1,000,000 [Int]                Allocate           47,999,976      92  OK
   1,000,000 [Int]                Encode: Store      56,000,144      92  OK
   1,000,000 [Int]                Encode: Cereal    368,230,040     679  OK
   2,000,000 [Int]                Allocate           95,999,976     184  OK
   2,000,000 [Int]                Encode: Store     112,000,144     184  OK
   2,000,000 [Int]                Encode: Cereal    736,425,240   1,357  OK
  10,000,000 [Int]                Allocate          479,999,976     920  OK
  10,000,000 [Int]                Encode: Store     560,000,144     920  OK
  10,000,000 [Int]                Encode: Cereal  3,682,021,208   6,783  OK
   1,000,000 Boxed Vector Int     Allocate            8,007,936       1  OK
   1,000,000 Boxed Vector Int     Encode: Store      16,008,168       2  OK
   1,000,000 Boxed Vector Int     Encode: Cereal    600,238,200   1,118  OK
   2,000,000 Boxed Vector Int     Allocate           16,015,752       1  OK
   2,000,000 Boxed Vector Int     Encode: Store      32,015,984       2  OK
   2,000,000 Boxed Vector Int     Encode: Cereal  1,200,441,216   2,234  OK
  10,000,000 Boxed Vector Int     Allocate           80,078,248       1  OK
  10,000,000 Boxed Vector Int     Encode: Store     160,078,480       2  OK
  10,000,000 Boxed Vector Int     Encode: Cereal  6,002,099,680  11,168  OK
   1,000,000 Storable Vector Int  Allocate            8,000,120       1  OK
   1,000,000 Storable Vector Int  Encode: Store      16,000,272       2  OK
   1,000,000 Storable Vector Int  Encode: Cereal    656,230,352   1,222  OK
   2,000,000 Storable Vector Int  Allocate           16,000,120       1  OK
   2,000,000 Storable Vector Int  Encode: Store      32,000,272       2  OK
   2,000,000 Storable Vector Int  Encode: Cereal  1,312,425,552   2,443  OK
  10,000,000 Storable Vector Int  Allocate           80,000,120       1  OK
  10,000,000 Storable Vector Int  Encode: Store     160,000,272       2  OK
  10,000,000 Storable Vector Int  Encode: Cereal  6,562,021,520  12,215  OK
````
