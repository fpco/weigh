{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Example uses of comparing map-like data structures.

module Main where

import           Control.DeepSeq
import qualified Data.HashMap.Lazy
import qualified Data.HashMap.Strict
import qualified Data.IntMap.Lazy
import qualified Data.IntMap.Strict
import qualified Data.Map.Lazy
import qualified Data.Map.Strict
import           System.Random
import           Weigh

-- | Weigh maps.
main :: IO ()
main =
  mainWith (do inserts
               fromlists)

inserts :: Weigh ()
inserts = do func "Data.Map.Strict.insert mempty"
                  (\(k,v) -> Data.Map.Strict.insert k v mempty)
                  (1 :: Int,1 :: Int)
             func "Data.Map.Lazy.insert mempty"
                  (\(k,v) -> Data.Map.Lazy.insert k v mempty)
                  (1 :: Int,1 :: Int)
             func "Data.HashMap.Strict.insert mempty"
                  (\(k,v) -> Data.HashMap.Strict.insert k v mempty)
                  (1 :: Int,1 :: Int)
             func "Data.HashMap.Lazy.insert mempty"
                  (\(k,v) -> Data.HashMap.Lazy.insert k v mempty)
                  (1 :: Int,1 :: Int)

fromlists :: Weigh ()
fromlists =
  do let !elems =
           force (zip (randoms (mkStdGen 0) :: [Int])
                      [1 :: Int .. 1000000])
     func "Data.Map.Strict.fromList     (1 million)" Data.Map.Strict.fromList elems
     func "Data.Map.Lazy.fromList       (1 million)" Data.Map.Lazy.fromList elems
     func "Data.IntMap.Strict.fromList  (1 million)" Data.IntMap.Strict.fromList elems
     func "Data.IntMap.Lazy.fromList    (1 million)" Data.IntMap.Lazy.fromList elems
     func "Data.HashMap.Strict.fromList (1 million)" Data.HashMap.Strict.fromList elems
     func "Data.HashMap.Lazy.fromList   (1 million)" Data.HashMap.Lazy.fromList elems
