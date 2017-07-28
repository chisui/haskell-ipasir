{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module SAT.IPASIR.Minicard
{-    ( Minicard
    , minicard
    ) -} where

import Data.Proxy
import Data.Functor
import qualified Data.Set as Set

import Control.Comonad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import SAT.IPASIR
import SAT.IPASIR.Api
import SAT.IPASIR.Minicard.C

minicard :: Proxy (IpasirSolver Minicard s)
minicard = Proxy

data MinisatCardinality v = MinisatAtMostK  [Lit (Var v)] Word
                          | MinisatAtLeastK [Lit (Var v)] Word

lits (MinisatAtMostK  l _) = l
lits (MinisatAtLeastK l _) = l
k    (MinisatAtMostK  _ x) = x
k    (MinisatAtLeastK _ x) = x

toWords :: Ord v => IpasirSolver Minicard v -> MinisatCardinality v -> [Lit Word]
toWords (IpasirSolver _ vc) (MinisatAtMostK lits k) = map (varToInt vc <$>) lits

minisatAtMostK :: Ord v =>  [Lit v] -> Word -> MinisatCardinality v
minisatAtMostK lits k = MinisatAtMostK lits' k
    where
        lits' = map (Right <$>) lits

minisatAtLeastK :: Ord v =>  [Lit v] -> Word -> MinisatCardinality v
minisatAtLeastK lits k = MinisatAtLeastK lits' k
    where
        lits' = map (Right <$>) lits

toAtMostK :: MinisatCardinality v -> MinisatCardinality v
toAtMostK (MinisatAtMostK  l k) = MinisatAtMostK l k
toAtMostK (MinisatAtLeastK l k) = MinisatAtMostK l' k'
    where
        k' = toEnum $ max 0 $ length l - (fromEnum k)
        l' = map neg l

instance Ord v => HasVariables (MinisatCardinality v) where
    type VariableType (MinisatCardinality v) = v
    getAllVariables ls _ = let MinisatAtMostK lits _ = ls in map extract lits
    getAllHelpers _ _    = []
    getHelpers    _ _    = Set.empty

instance Ord v => Clauses (IpasirSolver Minicard) (MinisatCardinality v) where
    addClauses conf = do
        solvers <- get
        newSolver <- lift $ mapM (addCard (toAtMostK conf)) solvers
        put newSolver
        return ()
        where
            addCard :: MinisatCardinality v -> IpasirSolver Minicard v -> IO (IpasirSolver Minicard v)
            addCard conf solver@(IpasirSolver cSolver _) = do
                let iLits = toWords solver conf
                addAtMostK cSolver iLits $ k conf
                return solver

instance Ord v => Clauses (IpasirSolver Minicard) (Formula v) where
    addClauses f = do
        solvers <- get
        newSolver <- lift $ mapM (addClauses' f) solvers
        put newSolver
        return ()
        where
            addClauses' :: Formula v -> IpasirSolver Minicard v -> IO (IpasirSolver Minicard v)
            addClauses' f (IpasirSolver cSolver vc) = do
                ipasirAddClauses    cSolver intCNF
                return (IpasirSolver cSolver vc')
                where
                    (vc', cnf) = formulaToCNF vc f
                    intCNF  = clausesToInt vc' cnf



