module SAT.IPASIR.PseudoBoolean
    ( module Export
    ) where

import qualified Data.Map as Map

import SAT.PseudoBoolean
import SAT.IPASIR.PseudoBoolean.State as Export

instance Ord v => HasVariables (PBConstraint v) where
    type VariableType (PBConstraint v) = v
    getVars = Map.keys . vars

instance (Ord v, Ipasir i) => Clauses (MIpasirSolver i) (PBConstraint v) where
    addClauses c = do
        solvers <- getVars
        newSolvers <- lift $ mapM addClauses' solvers
        put newSolvers
        return ()
        where
            addClauses' (MIpasirSolver solver cache) = do
                clauses <- runEncoder (config c) weightedLits (comp c) (cn $ lower c) (cn $ upper c) nVars getClauses
                return ()
            cn = toEnum . fromEnum
            weightedLits = (\(v, i) -> v $-$ fromInteger i) <$> Map.toList $ vars c
