{-# LANGUAGE RankNTypes, KindSignatures #-}
module SAT.IPASIR.LiteralCache where

import qualified Data.Map as Map

class LiteralCache (a :: * -> *) where
    numVars  :: forall l. a l -> Int
    intToVar :: forall l. a l -> Int -> l
    varToInt :: forall l. a l -> l -> Int
