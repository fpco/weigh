# weigh [![Build Status](https://travis-ci.org/fpco/weigh.png)](https://travis-ci.org/fpco/weigh)

Measures the memory usage of a Haskell value or function

## Example output

This is the output for the Store package to measure allocation of
three data structure types, and encoding them to binary with the Store
package vs the Cereal package.

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
