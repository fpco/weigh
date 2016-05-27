{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

-- | Calculate the size of GHC.Stats statically.

module Weigh.GHCStats
  (ghcStatsSizeInBytes)
  where

import GHC.Stats
import GHC.Int
import Language.Haskell.TH
import Data.List

-- | Get the size of a 'GHCStats' object in bytes.
ghcStatsSizeInBytes :: Int64
ghcStatsSizeInBytes =
  $(do info <- reify ''GHC.Stats.GCStats
       case info of
#if __GLASGOW_HASKELL__ >= 800
         TyConI (DataD _ _ _ _ [RecC _ fields] _) ->
#else
         TyConI (DataD _ _ _ [RecC _ fields] _) ->
#endif
           do total <-
                fmap (foldl' (+) headerSize)
                     (mapM fieldSize fields)
              litE (IntegerL (fromIntegral total))
           where headerSize = 8
                 fieldSize
                   :: (name,strict,Type) -> Q Int64
                 fieldSize (_,_,typ) =
                   case typ of
                     ConT typeName ->
                       case lookup typeName knownTypes of
                         Nothing ->
                           fail ("Unknown size for type " ++
                                 show typeName ++
                                 ". Please report this as a bug, the codebase needs updating.")
                         Just size -> return size
                     _ ->
                       fail ("Unexpected type shape: " ++
                             show typ ++
                             ". Please report this as a bug, the codebase needs updating.")
                 knownTypes :: [(Name,Int64)]
                 knownTypes =
                   map (,8)
                       [''Int64,''Int32,''Int8,''Int16,''Double]
         _ ->
           fail ("Unexpected shape of GCStats data type. " ++
                 "Please report this as a bug, this function " ++
                 "needs to be updated to the newer GCStats type."))
