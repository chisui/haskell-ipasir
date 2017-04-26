{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module SAT.IPASIR.Solver where

import qualified Data.Map as Map

import SAT.IPASIR.CSolver
import SAT.IPASIR.LiteralCache
import SAT.IPASIR.Literals


type Val = Maybe Bool

class (Solver s, Ord (ClausesLabel c)) => Clauses s c where
    type ClausesLabel c
    toClauses :: forall s. Clauses s c => c -> [[ELit (ClausesLabel c)]]

instance (Ord l, Solver s) => Clauses s [[Lit l]] where
    type ClausesLabel [[Lit l]] = l
    toClauses = map (map (Right <$>))

class Solver (s :: * -> *) where
    new :: forall l. IO (s l)
    addClauses :: (Clauses s c, l ~ ClausesLabel c) => s l -> c -> IO (s l)
    solve :: s l -> IO (s l, Either (Map.Map l Val) (Map.Map l Val))

type LitCache l = Map.Map l Word
data CIpasir s c = CIpasir s (LitCache (ClausesLabel c))
