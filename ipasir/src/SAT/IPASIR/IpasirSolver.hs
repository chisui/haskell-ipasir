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
) where

import Control.Comonad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

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
            mSolve' :: MIpasirSolver s lc v -> IO (ESolution v)
            mSolve' solver@(MIpasirSolver i lc) = bimap (mapLits lc) (mapLits lc) <$> mSolveInt solver

    -- forall m l. (Traversable m, Ord l) => [l] -> StateT (m (s l)) IO (m ([Solution l], Conflict l))
    mSolveAllForVars ls = do
        solvers <- get
        lift $ mapM solve solvers
        where
            solve s@(MIpasirSolver i lc) = do
                let ints = map (varToInt lc) ls
                (sols, conflict) :: ([Solution Word], Conflict Word) <- solve' <$> mSolveInt s
                return (mapLits' <$> sols, mapLits conflict)
                where
                    mapLits' = mapLits lc
                    solve' (Right conflict) = return (sols, conflict)
                    solve' (Left sol) = do
                        let clause = mapMaybe (extract sol) (zip ls ints)
                        ipasirAddClause clause
                        (sols, conflict) <- solve' <$> mSolveInt s
                        return (sol:sols, conflict)
                    extract sol (lit, i) = (i *) . sign' <$> (sol Map.! i)
                    sign' True = -1
                    sign' False = 1

instance Ord v => HasVariables [[Lit v]] where
    type VariableType [[Lit v]] = v
    getVars = map extract . nub . concat

instance (Ord v, Ipasir i) => Clauses (MIpasirSolver i) [[Lit v]] where
    addClauses :: (Ord l) => IpasirSolver i lc -> [[Lit v]] -> IO (IpasirSolver i lc v)
    addClauses (IpasirSolver solver cache) rawClauses = do
        ipasirAddClauses intClauses solver
        return $ CIpasir solver cache'
        where
            intClauses :: [[Lit Word]]
            intClauses = varToInt cache' <$$$> clauses
            clauses = Right <$$$> rawClauses
            vars :: [Ext l]
            vars = extract <$> concat clauses
            cache' = cache insertVars vars
            (<$$$>)=(<$>).(<$>).(<$>)

mapLits lc = Map.mapKeys (intToVar lc)

mSolve' :: MIpasirSolver s lc v -> IO (ESolution v)
mSolve' s@(MIpasirSolver _ lc) = mapLits lc $ mSolveInt s

mSolveInt :: MIpasirSolver s lc v -> IO (ESolution Int)
mSolveInt (MIpasirSolver s lc) = do
    sol <- ipasirSolve s
    case sol of
        Nothing -> error "solving interrupted"
        (Just True) -> Left <$> makeSolution ( ((sign <$>) <$> ) . ipasirVal)
        (Just False) -> Right <$> makeSolution ipasirFailed
    where
        makeSolution ioOp = Map.fromList <$> mapM (\i -> (i,) <$> ioOp i) [1..numVars lc]