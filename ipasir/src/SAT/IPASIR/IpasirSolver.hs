{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module SAT.IPASIR.IpasirSolver
    ( IpasirSolver(..)
    ) where

import Control.Comonad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

import Data.Maybe
import qualified Data.Map as Map

import SAT.IPASIR.Api
import SAT.IPASIR.Solver
import SAT.IPASIR.Literals
import SAT.IPASIR.LiteralCache

data IpasirSolver s lc = IpasirSolver
data MIpasirSolver s lc v = MIpasirSolver s lc

data MiniSat = MiniSat
miniSat :: IpasirSolver MiniSat LitCache
miniSat = undefined

instance (Ipasir s, LiteralCache lc) => MSolver (IpasirSolver s (v -> lc v)) where
    type MSolver' (IpasirSolver s lc) = MIpasirSolver s lc

    -- (Applicative m, Monoid m, Ord l) => MSolverMarker s -> StateT (m (s l)) IO ()
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
    
    -- forall m l. (Traversable m, Ord l) => [l] -> StateT (m (s l)) IO (m ([Solution l], Conflict l))
    mSolveAllForVars ls = do
        solvers <- get
        lift $ mapM solve solvers
            where
                solve s@(IpasirSolver i lc) = do
                    let ints = map (varToInt lc) ls
                    (sols, conflict) <- solve' <$> mSolveInt s
                    return (mapLits <$> sols, mapLits conflict)
                    where
                        mapLits = mapKeys $ intToVar lc
                        solve' (Right conflict) = return (sols, conflict)
                        solve' (Left sol) = do
                            let clause = mapMaybe (extract sol) (zip ls ints)
                            ipasirAddClause clause
                            (sols, conflict) <- solve' <$> mSolveInt s
                            return (sol:sols, conflict)
                        extract sol (lit, i) = (i *) . sign' <$> (sol Map.! i) 
                        sign' True  = -1
                        sign' False =  1

instance Ord v => HasVariables [[Lit v]] where
    type VariableType [[Lit v]] = v
    getVars = map extract . nub . concat

instance (Ord v) => Clauses (IpasirSolver s lc v) [[Lit v]] where
    addClauses :: (Ord l) => IpasirSolver s lc v -> [[Lit v]] -> IO (IpasirSolver s lc v)
    addClauses (IpasirSolver solver cache) rawClauses = do
        ipasirAddClauses intClauses solver
        return $ CIpasir solver cache'
        where
            intClauses :: [[Lit Word]]
            intClauses = varToInt cache' <$$$> clauses
            clauses = Right <$$$> rawClauses
            vars :: [Ext l]
            vars = extract <$> concat clauses
            cache' = cache `insertVars` vars
            (<$$$>)=(<$>).(<$>).(<$>)


mSolveInt :: IpasirSolver s lc v -> IO (ESolution Int)
mSolveInt (IpasirSolver s lc) = do
    sol <- ipasirSolve s
    case sol of
        Nothing      -> error "solving interrupted"
        (Just True)  -> Left  <$> makeSolution ( ((sign <$>) <$> ) . ipasirVal)
        (Just False) -> Right <$> makeSolution ipasirFailed
    where
        makeSolution ioOp = Map.fromList <$> mapM (\i -> (i,) <$> ioOp i) [1..numVars lc]