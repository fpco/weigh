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

To try out other examples, try:

* `stack test :weigh-maps --flag weigh:weigh-maps`
