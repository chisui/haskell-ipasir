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
import qualified Data.Map as Map

import SAT.IPASIR.Api
import SAT.IPASIR.Solver
import SAT.IPASIR.Literals
import SAT.IPASIR.LiteralCache

data IpasirSolver (i :: *) (lc :: * -> *) = IpasirSolver
data MIpasirSolver (i :: *) (lc :: * -> *) v = MIpasirSolver i (lc v)

-- type CryptoMiniSat = MIpasirSolver CryptominisatSolver
-- type CryptoMiniSatMarker = IpasirSolver CryptominisatSolver

instance (Ipasir i, LiteralCache lc, Ord v) => MSolver (MIpasirSolver i) lc v where
    type Marker (MIpasirSolver i) lc = IpasirSolver i lc

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
            mSolve' :: (LiteralCache lc, Ipasir i) => MIpasirSolver i lc v -> IO (ESolution v)
            mSolve' solver@(MIpasirSolver i lc) = bimap (mapLits lc) (mapLits lc) <$> mSolveInt solver

    -- forall m l. (Traversable m, Ord l) => [l] -> StateT (m (s l)) IO (m ([Solution l], Conflict l))
    mSolveAllForVars ls = do
        solvers <- get
        lift $ mapM solve solvers
        where
            solve :: MIpasirSolver i lc v -> IO ([Solution v], Conflict v)
            solve s@(MIpasirSolver i lc) = do
                (sols, conflict) :: ([Solution Word], Conflict Word) <- solve' =<< mSolveInt s
                return (mapLits lc <$> sols, mapLits lc conflict)
                where
                    ints = map (varToInt lc) ls
                    solve' :: ESolution Word -> IO ([Solution Word], Conflict Word)
                    solve' (Right conflict) = return ([], conflict)
                    solve' (Left sol) = do
                        let clause = mapMaybe (extract sol) ints
                        ipasirAddClause clause i
                        (sols, conflict) <- solve' =<< mSolveInt s
                        return (sol:sols, conflict)
                    extract :: Solution Word -> Word -> Maybe (Lit Word)
                    extract sol i = neg . (`lit` i)  <$> val
                        where
                            val = sol Map.! i
                    sign' True = -1
                    sign' False = 1

instance Ord v => HasVariables [[Lit v]] where
    type VariableType [[Lit v]] = v
    getVars = map extract . nub . concat

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
                    intClauses = varToInt cache' <$$$> rawClauses
                    vars = extract <$> concat rawClauses
                    cache' = cache `insertVars` vars
                    (<$$$>)=(<$>).(<$>).(<$>)

mapLits :: (Enum e, Ord v, LiteralCache lc) => lc v -> Map.Map e a -> Map.Map v a
mapLits lc = Map.mapKeys (intToVar lc)

mSolveInt :: (LiteralCache lc, Ipasir i) => MIpasirSolver i lc v -> IO (ESolution Word)
mSolveInt (MIpasirSolver s lc) = do
    sol <- ipasirSolve s
    case sol of
        Nothing -> error "solving interrupted"
        (Just True) -> Left <$> makeSolution ( ((sign <$>) <$> ) . (`ipasirVal` s))
        (Just False) -> Right <$> makeSolution (`ipasirFailed` s)
    where
        makeSolution :: (Word -> IO a) -> IO (Map.Map Word a)
        makeSolution ioOp = Map.fromList <$> mapM (\i -> (i,) <$> ioOp i) [1..numVars lc]