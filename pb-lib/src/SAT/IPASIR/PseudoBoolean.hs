{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SAT.IPASIR.PseudoBoolean
    ( module PB
    ) where

import qualified Data.Map as Map

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import SAT.IPASIR

import SAT.PseudoBoolean
import qualified SAT.PseudoBoolean.C as C
import SAT.IPASIR.PseudoBoolean.State as PB


instance (C.CardinalityMethod c, Ord v) => HasVariables (PBConstraint c v) where
    type VariableType (PBConstraint c v) = v
    getAllVariables c _ = Map.keys $ PB.vars c

instance (C.CardinalityMethod c, Ord v, Ipasir i) => Clauses (IpasirSolver i) (PBConstraint c v) where
    addClauses c = do
        solvers <- get
        newSolvers <- lift $ mapM addClauses' solvers
        put newSolvers
        return ()
        where
            addClauses' (IpasirSolver solver cache) = do
                rawClauses <- evalEncoder (pbConfig c) weightedLits (comp c) (cn $ lower c) (cn $ upper c) nVars getClauses
                let (intClauses, cache') = toClauses cache rawClauses
                ipasirAddClauses intClauses solver
                return (IpasirSolver solver cache')
                where
                    cn = toEnum . fromEnum
                    toClauses = undefined
                    nVars = length weightedLits
                    weightedLits = asLit <$> Map.toList (PB.vars c)
                    asLit (v, i) = varToInt cache v $-$ fromInteger i
    
