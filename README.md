# weigh [![Tests](https://github.com/fpco/weigh/actions/workflows/tests.yml/badge.svg)](https://github.com/fpco/weigh/actions/workflows/tests.yml)

Measures the memory usage of a Haskell value or function

# Limitations

*  :warning: Turn off the `-threaded` flag, otherwise it will cause inconsistent results.

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

|Case|Allocated|GCs|
|:---|---:|---:|
|integers count 0|16|0|
|integers count 1|88|0|
|integers count 10|736|0|
|integers count 100|7,216|0|

Output by default is plain text table; pass `--markdown` to get a
markdown output like the above.
