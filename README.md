# weigh [![Build Status](https://travis-ci.org/fpco/weigh.svg)](https://travis-ci.org/fpco/weigh)

Measures the memory usage of a Haskell value or function

## Example use

``` haskell
import Weigh

main :: IO ()
main =
  mainWith
    (do func "integers count 0" count 0
        func "integers count 1" count 1
        func "integers count 10" count 10
        func "integers count 100" count 100)
  where
    count :: Integer -> ()
    count 0 = ()
    count a = count (a - 1)
```

Output results:

```
Case                Allocated  GCs
integers count 0           16    0
integers count 1           88    0
integers count 2          160    0
integers count 3          232    0
integers count 10         736    0
integers count 100      7,216    0
```
