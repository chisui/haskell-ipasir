{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SAT.IPASIR.PseudoBoolean
    ( module PB
    ) where


import qualified Data.Map as Map

import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import SAT.IPASIR

import SAT.PseudoBoolean
import qualified SAT.PseudoBoolean.C as C
import SAT.IPASIR.PseudoBoolean.State as PB


instance (C.CardinalityMethod c, Ord v) => HasVariables (PBConstraint c v) where
    type VariableType (PBConstraint c v) = v
    getAllVariables c _ = map Right $ Map.keys $ PB.vars c

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
                    asLit (v, i) = varToInt cache (Right v) $-$ fromInteger i

minimizingOverVars :: forall s v c m. (MSolver s, Ord v, C.CardinalityMethod c, Clauses s [[Lit v]], VariableType [[Lit v]] ~ v, Monad m, Traversable m) => PBConstraint c v -> StateT (m (s v)) IO (m (Conflict v, [Solution v]))
minimizingOverVars constraint = do
    (clauses, encoder) <- lift $ runStateT (PB.newPBEncoder constraint) Nothing
    addClauses clauses
    sol <- mSolve
    idToM $ mapM (minimizingOverVars' encoder) sol
    where
        idToM :: StateT (Identity (s v)) IO (m (Conflict v, [Solution v])) -> StateT (m (s v)) IO (m (Conflict v, [Solution v]))
        idToM body = do
            state <- get
            r <- lift $ mapM (runStateT body . Identity) state
            put $ (runIdentity . snd) <$> r
            return $ fst =<< r
        importantVars :: [v]
        importantVars = Map.keys $ PB.vars constraint
        minimizingOverVars' :: Maybe (Enc c v) -> ESolution v -> StateT (Identity (s v)) IO (Conflict v, [Solution v])
        minimizingOverVars' _ (Left conflict)   = return (conflict, [])
        minimizingOverVars' encoder (Right solution) = do
            let count = toEnum $ length $ filter ((`Map.member` solution) . Right) importantVars
            newClauses :: [[Lit v]] <- lift $ evalStateT (PB.pushUpperBound (count-1)) encoder
            addClauses newClauses
            (Identity sol) <- mSolve
            (con, s') <- minimizingOverVars' encoder sol
            return (con, s'++[solution])

