{-# LANGUAGE RankNTypes, KindSignatures, TypeFamilies, MultiParamTypeClasses, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances #-}
module SAT.IPASIR.Solver where

import qualified Data.Map as Map

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
    solve :: s l -> IO (s l, Maybe (Map.Map l Val))
