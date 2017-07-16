{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SAT.IPASIR.PseudoBoolean
    ( module PB
    , minimizeOverVars
    , cardinalitySolving
    ) where

import Control.Comonad
import qualified Data.Map as Map
import Data.Maybe

import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import SAT.IPASIR
import SAT.IPASIR.Api

import SAT.PseudoBoolean
import qualified SAT.PseudoBoolean.C as C
import SAT.IPASIR.PseudoBoolean.State as PB

import Debug.Trace


instance (C.CardinalityMethod c, Ord v) => HasVariables (PBConstraint c v) where
    type VariableType (PBConstraint c v) = v
    getAllVariables c _ = map (Right . extract) $ Map.keys $ PB.vars c

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
                    asLit :: (Lit v, Integer) -> WeightedLit
                    asLit (v, i) = lit $-$ fromInteger i
                        where
                            lit = fromEnum $ varToInt cache . Right <$> v


cardinalitySolving :: forall s v c m. (Show v, MSolver s, Ord v, C.CardinalityMethod c, Clauses s [[Lit v]], VariableType [[Lit v]] ~ v, Monad m, Traversable m) => 
                    (Solution v -> IO ()) -> ((Int, Int) -> Int -> (Int, Int)) -> PBConstraint c v -> StateT (m (s v)) IO (m (Conflict v, [Solution v]))
cardinalitySolving handleSolution borderFunction constraint = do
    (clauses, encoder) <- lift $ runStateT (PB.newPBEncoder constraint) Nothing
    addClauses clauses
    sol <- mSolve
    idToM $ mapM (minimizeOverVars' encoder (startLower,startUpper)) sol
    where
        weights = Map.elems $ PB.vars constraint
        startLower = fromEnum $ sum $ filter (<0) weights
        startUpper = fromEnum $ sum $ filter (>0) weights

        idToM :: StateT (Identity (s v)) IO (m (Conflict v, [Solution v])) -> StateT (m (s v)) IO (m (Conflict v, [Solution v]))
        idToM body = do
            state <- get
            r <- lift $ mapM (runStateT body . Identity) state
            put $ (runIdentity . snd) <$> r
            return $ fst =<< r

        importantVars :: [v]
        importantVars = map extract $ Map.keys $ PB.vars constraint

        minimizeOverVars' :: Maybe (Enc c v) -> (Int, Int) -> ESolution v -> StateT (Identity (s v)) IO (Conflict v, [Solution v])
        minimizeOverVars' _ _ (Left conflict)   = return (conflict, [])
        minimizeOverVars' encoder (lower, upper) (Right solution) = do
            lift $ handleSolution solution
            let count = length $ filter (fromMaybe False . (solution Map.!) . Right) importantVars
            let (newLower, newUpper) = borderFunction (lower, upper) count

            newClauses <- if newLower <= lower && newUpper >= upper 
                then error $ "You have to decrease the border-size! Old Border: " ++ show (lower, upper) ++ ", new border: " ++ show (lower, upper)
                else do
                    lowerClauses <- if newUpper < upper
                        then lift $ evalStateT (PB.pushUpperBound newUpper) encoder
                        else return []
                    upperClauses <- if newLower > lower
                        then lift $ evalStateT (PB.pushLowerBound newLower) encoder
                        else return []
                    return $ lowerClauses ++ upperClauses

            addClauses newClauses
            (Identity sol) <- mSolve
            (con, s') <- minimizeOverVars' encoder (newLower, newUpper) sol
            return (con, s'++[solution])


minimizeOverVars :: forall s v c m. (Show v, MSolver s, Ord v, C.CardinalityMethod c, Clauses s [[Lit v]], VariableType [[Lit v]] ~ v, Monad m, Traversable m) => 
                    PBConstraint c v -> StateT (m (s v)) IO (m (Conflict v, [Solution v]))
minimizeOverVars constraint = do
    (clauses, encoder) <- lift $ runStateT (PB.newPBEncoder constraint) Nothing
    addClauses clauses
    sol <- mSolve
    idToM $ mapM (minimizeOverVars' encoder) sol
    where
        idToM :: StateT (Identity (s v)) IO (m (Conflict v, [Solution v])) -> StateT (m (s v)) IO (m (Conflict v, [Solution v]))
        idToM body = do
            state <- get
            r <- lift $ mapM (runStateT body . Identity) state
            put $ (runIdentity . snd) <$> r
            return $ fst =<< r
        importantVars :: [v]
        importantVars = map extract $ Map.keys $ PB.vars constraint
        minimizeOverVars' :: Maybe (Enc c v) -> ESolution v -> StateT (Identity (s v)) IO (Conflict v, [Solution v])
        minimizeOverVars' _ (Left conflict)   = return (conflict, [])
        minimizeOverVars' encoder (Right solution) = do
            let count = length $ filter (fromMaybe False . (solution Map.!) . Right) importantVars
            newClauses :: [[Lit v]] <- lift $ evalStateT (PB.pushUpperBound (count-1)) encoder
            traceM $ show count
            addClauses newClauses
            (Identity sol) <- mSolve
            (con, s') <- minimizeOverVars' encoder sol
            return (con, s'++[solution])

