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
module SAT.IPASIR.IpasirSolver
( IpasirSolver(..)
, MIpasirSolver(..)
) where

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

data IpasirSolver i = IpasirSolver
data MIpasirSolver i v = MIpasirSolver i (VarCache v)

instance Ipasir i => MSolver (MIpasirSolver i) where
    type Marker (MIpasirSolver i) = IpasirSolver i

    -- newMSolver :: (Applicative m, Monoid (m (s v)), Ord v) => marker -> StateT (m (s v)) IO ()
    newMSolver _ = do
        oldSolvers <- get
        newSolver <- lift $ do
            s <- ipasirInit
            return $ MIpasirSolver s emptyCache
        put $ oldSolvers <> pure newSolver
        return ()

    -- forall m l. (Traversable m, Ord l) => StateT (m (s l)) IO (m (ESolution l))
    mSolve = do
        solvers <- get
        lift $ mapM mSolve' solvers
        where
            mSolve' :: (Ord v, Ipasir i) => MIpasirSolver i v -> IO (ESolution v)
            mSolve' solver@(MIpasirSolver i vc) = bimap (mapLits vc) (mapLits vc) <$> mSolveInt solver

    mSolveAllForVars :: forall v m. (Ord v, Traversable m) => [Var v] -> StateT (m (MIpasirSolver i v)) IO (m (Conflict v, [Solution v]))
    mSolveAllForVars ls = do
        solvers <- get
        lift $ mapM solve solvers
        where
            solve :: (Ord v, Ipasir i) => MIpasirSolver i v -> IO (Conflict v, [Solution v])
            solve s@(MIpasirSolver i vc) = do
                (conflict, sols) :: (IConflict Word, [ISolution Word]) <- solve' =<< mSolveInt s
                return (mapLits vc conflict, mapLits vc <$> sols)
                where
                    ints :: [Word]
                    ints = map (varToInt vc) ls
                    solve' :: IESolution Word -> IO (IConflict Word, [ISolution Word])
                    solve' (Left conflict) = return (conflict, [])
                    solve' (Right sol) = do
                        let clause = mapMaybe (extract sol) ints
                        ipasirAddClause clause i
                        (conflict, sols) <- solve' =<< mSolveInt s
                        return (conflict, sol:sols)
                    extract :: ISolution Word -> Word -> Maybe (Lit Word)
                    extract sol i = neg . (`lit` i) <$> val
                        where
                            val = sol Map.! i
                    sign' True = -1
                    sign' False = 1

instance Ipasir i => Solver (MIpasirSolver i) where

instance Ord v => HasVariables [[Lit v]] where
    type VariableType [[Lit v]] = v
    getAllVariables ls _ = map extract $ concat ls
    getAllHelpers _ _ = []
    getHelpers _ _ = Set.empty

instance (Ord v, Ipasir i) => Clauses (MIpasirSolver i) [[Lit v]] where
    addClauses rawClauses = do
        solvers <- get
        newSolver <- lift $ mapM addClauses' solvers
        put newSolver
        return ()
        where
            addClauses' (MIpasirSolver solver cache) = do
                ipasirAddClauses intClauses solver
                return $ MIpasirSolver solver cache'
                where
                    intClauses :: [[Lit Word]]
                    intClauses = (varToInt cache' . Right) 💩 rawClauses
                    vars = extract <$> concat rawClauses
                    cache' = snd $ newVars cache vars
                    (💩)=(<$>).(<$>).(<$>)

mapLits :: Ord v => VarCache v -> Map.Map Word a -> Map.Map (Var v) a
mapLits vc = Map.mapKeys (intToVar vc)

type ISolution v = Map.Map v Val
type IConflict v = Map.Map v Bool
type IESolution v = Either (IConflict v) (ISolution v)

mSolveInt :: Ipasir i => MIpasirSolver i v -> IO (IESolution Word)
mSolveInt (MIpasirSolver s vc) = do
    sol <- ipasirSolve s
    case sol of
        Nothing -> error "solving interrupted"
        (Just True) -> Right <$> makeSolution ( ((sign <$>) <$> ) . (`ipasirVal` s))
        (Just False) -> Left <$> makeSolution (`ipasirFailed` s)
    where
        makeSolution :: (Word -> IO a) -> IO (Map.Map Word a)
        makeSolution ioOp = Map.fromList <$> mapM (\i -> (i,) <$> ioOp i) [1..numVars vc]