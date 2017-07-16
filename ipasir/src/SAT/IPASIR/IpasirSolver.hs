{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

-- |This module implements the solve functions for IPASIR-solver (See "SAT.IPASIR.Api"). 
module SAT.IPASIR.IpasirSolver where

import Control.Comonad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

import Data.List (nub)
import Data.Monoid
import Data.Maybe
import Data.Bifunctor
import qualified Data.Set as Set
import qualified Data.Map as Map

import SAT.IPASIR.Api
import SAT.IPASIR.Solver
import SAT.IPASIR.Literals
import SAT.IPASIR.VarCache

{- |Stands for a solver, which is implementing the IPASIR Api. See "SAT.IPASIR.Api". 
    
    @i@ is the solver, and @v@ the variable type.
-}
data IpasirSolver i v = IpasirSolver i (VarCache v)

instance Ipasir i => MSolver (IpasirSolver i) where

    -- newMSolver :: (Applicative m, Monoid (m (s v)), Ord v) => marker -> StateT (m (s v)) IO ()
    newMSolver _ = do
        oldSolvers <- get
        newSolver <- lift $ do
            s <- ipasirInit
            return $ IpasirSolver s emptyCache
        put $ oldSolvers <> pure newSolver
        return ()

    -- forall m l. (Traversable m, Ord l) => StateT (m (s l)) IO (m (ESolution l))
    mSolve = do
        solvers <- get
        lift $ mapM mSolve' solvers
        where
            mSolve' :: (Ord v, Ipasir i) => IpasirSolver i v -> IO (ESolution v)
            mSolve' solver@(IpasirSolver i vc) = bimap (Set.map (intToVar vc)) (mapLits vc) <$> mSolveInt solver

    mSolveAllForVars :: forall v m. (Ord v, Traversable m) => [Var v] -> StateT (m (IpasirSolver i v)) IO (m (Conflict v, [Solution v]))
    mSolveAllForVars ls = do
        solvers <- get
        lift $ mapM solve solvers
        where
            solve :: (Ord v, Ipasir i) => IpasirSolver i v -> IO (Conflict v, [Solution v])
            solve s@(IpasirSolver i vc) = do
                (conflict, sols) :: (IConflict Word, [ISolution Word]) <- solve' =<< mSolveInt s
                return (Set.map (intToVar vc) conflict, mapLits vc <$> sols)
                where
                    ints :: [Word]
                    ints = map (varToInt vc) ls
                    solve' :: IESolution Word -> IO (IConflict Word, [ISolution Word])
                    solve' (Left conflict) = return (conflict, [])
                    solve' (Right sol) = do
                        let clause = mapMaybe (extract sol) ints
                        ipasirAddClause i clause
                        (conflict, sols) <- solve' =<< mSolveInt s
                        return (conflict, sol:sols)
                    extract :: ISolution Word -> Word -> Maybe (Lit Word)
                    extract sol i = neg . (`lit` i) <$> val
                        where
                            val = sol Map.! i
                    sign' True = -1
                    sign' False = 1

<<<<<<< HEAD
mSolveAllForVars' :: forall v m i. (Ord v, Traversable m, Ipasir i) => [Var v] -> StateT (m (IpasirSolver i v)) IO (m (Conflict v, [Solution v]))
mSolveAllForVars' ls = do
    solvers <- get
    lift $ mapM solve solvers
    where
        solve :: (Ord v, Ipasir i) => IpasirSolver i v -> IO (Conflict v, [Solution v])
        solve s@(IpasirSolver i vc) = do
            (conflict, sols) :: (IConflict Word, [ISolution Word]) <- solve' =<< mSolveInt s
            return (Set.map (intToVar vc) conflict, mapLits vc <$> sols)
            where
                ints :: [Word]
                ints = map (varToInt vc) ls
                solve' :: IESolution Word -> IO (IConflict Word, [ISolution Word])
                solve' (Left conflict) = return (conflict, [])
                solve' (Right sol) = do
                    let clause = mapMaybe (extract sol) ints
                    ipasirAddClause i clause
                    (conflict, sols) <- solve' =<< mSolveInt s
                    return (conflict, sol:sols)
                extract :: ISolution Word -> Word -> Maybe (Lit Word)
                extract sol i = neg . (`lit` i) <$> val
                    where
                        val = sol Map.! i
                sign' True = -1
                sign' False = 1

=======
>>>>>>> 91231f9bce77073b6ca8fa2b43730b2bc121f71a
instance Ipasir i => Solver (IpasirSolver i) where

instance Ord v => HasVariables [[Lit v]] where
    type VariableType [[Lit v]] = v
    getAllVariables ls _ = map (Right . extract) $ concat ls
    getAllHelpers _ _ = []
    getHelpers _ _ = Set.empty

instance (Ord v, Ipasir i) => Clauses (IpasirSolver i) [[Lit v]] where
    addClauses rawClauses = do
        solvers <- get
        newSolver <- lift $ mapM addClauses' solvers
        put newSolver
        return ()
        where
            addClauses' (IpasirSolver solver cache) = do
                ipasirAddClauses solver intClauses
                return $ IpasirSolver solver cache'
                where
                    intClauses :: [[Lit Word]]
                    intClauses = (varToInt cache' . Right) 💩 rawClauses
                    vars = extract <$> concat rawClauses
                    cache' = snd $ newVars cache vars
                    (💩)=(<$>).(<$>).(<$>)

-- |Transforms the solution of a solver (which is working on int) to the clauses type. 
--  The 'VarCache' contains the definitions of the variables.
mapLits :: Ord v => VarCache v -> Map.Map Word a -> Map.Map (Var v) a
mapLits vc = Map.mapKeys (intToVar vc)

-- |ISolution is comparable to 'Solution' but without helper variabes. 
type ISolution v = Map.Map v Val
-- |IConflict is comparable to 'Conflict' but without helper variabes. 
type IConflict v = Set.Set v
-- |IESolution is comparable to 'ESolution' but without helper variabes. 
type IESolution v = Either (IConflict v) (ISolution v)

-- | Same as 'mSolve' but without transforming the solution back into the variable type. 
--   You get the 'Words' instead, which are the same as the variables of the solver.
mSolveInt :: Ipasir i => IpasirSolver i v -> IO (IESolution Word)
mSolveInt (IpasirSolver s vc) = do
    sol <- ipasirSolve s
    case sol of
        Nothing -> error "solving interrupted"
        (Just True) -> Right <$> makeSolution ( ((sign <$>) <$> ) . ipasirVal s)
        (Just False) -> Left <$> makeConflict (ipasirFailed s)
    where
        makeConflict :: (Word -> IO Bool) -> IO (Set.Set Word)
        makeConflict ioOp = Map.keysSet . Map.filter id <$> makeSolution ioOp
        makeSolution :: (Word -> IO a) -> IO (Map.Map Word a)
        makeSolution ioOp = Map.fromList <$> mapM (\i -> (i,) <$> ioOp i) [1..numVars vc]
