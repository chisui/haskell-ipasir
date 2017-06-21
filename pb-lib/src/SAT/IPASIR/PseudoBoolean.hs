{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SAT.IPASIR.PseudoBoolean
    ( module PB
    ) where

import qualified Data.Map as Map

import Foreign.Ptr (nullPtr)
import Foreign.ForeignPtr (newForeignPtr_)

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

minimizingOverVars :: (MSolver s, Ord v, C.CardinalityMethod c, Clauses s [[Lit (Var v)]]) => PBConstraint c v -> StateT (m (s v)) IO (m (Conflict v, [Solution v]))
minimizingOverVars constraint = do
    (clauses, encoder) <- lift $ runStateT (PB.newPBEncoder constraint) (newForeighnPtr_ nullPtr)
    addClauses clauses
    sol <- mSolve
    let importantVars = keys $ vars constraint :: [Vars v]
    minimizingOverVars' sol
    where
        minimizingOverVars' (Left confict)   = return (conflict, [])
        minimizingOverVars' (Right solution) = do
            let count = length $ filter (`Map.elem` solution) importantVars
            newClauses <- lift $ evalStateT (pushUpperBound (count-1)) encoder
            addClause newClauses
            sol <- mSolve
            (con, s') <- minimizingOverVars' sol
            return (con, s'++[sol])

