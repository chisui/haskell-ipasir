{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module SAT.IPASIR.Solver where

import Control.Monad
import Control.Monad.Loops
import Control.Comonad

import Data.Either
import Data.Maybe
import qualified Data.Map as Map

import SAT.IPASIR.CSolver
import SAT.IPASIR.LiteralCache
import SAT.IPASIR.Literals


type Val = Maybe Bool

type Solution v = M.Map v Var
type Conflict v = M.Map v Bool
type ESolution v = Either (Solution v) (Conflict v)


class (Ord (ClausesLabel c), Solver s) => Clauses s c where
    type ClausesLabel c
    addClauses :: (ClausesLabel c ~ l) => s l -> c -> IO (s l)

instance (Ord l, CSolver s, LiteralCache c) => Clauses (CIpasir s c) [[Lit l]] where
    type ClausesLabel [[Lit l]] = l
    addClauses :: (Ord l, LiteralCache c) => CIpasir s c l -> [[Lit l]] -> IO (CIpasir s c l)
    addClauses (CIpasir solver cache) rawClauses = do
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

class Solver (s :: * -> *) where
    type SolverMarker s
    solve :: (Clauses s c, ClausesLabel c ~ l) => SolverMarker s -> c -> ESolution
    solveAllForVars :: (Clauses s c, ClausesLabel c ~ l) => SolverMarker s -> c -> [l] -> [Solution l]
    solveAllForVars = fst . solveAllForVars'
    solveAllForVars' :: (Clauses s c, ClausesLabel c ~ l) => SolverMarker s -> c -> [l] -> ([Solution l], Conflict l)
    solveAll :: (Clauses s c, ClausesLabel c ~ l) => SolverMarker s -> c -> [Solution l]
    solveAll c = solveAllForVars c (extract `map` getLits c)


data CIpasir s c l = CIpasir s (c (Ext l))

instance (CSolver s, LiteralCache c) => Solver (CIpasir s c) where
    new = do
        solver <- ipasirInit
        return $ CIpasir solver emptyCache

    solve :: forall c l. (Ord l, LiteralCache c) => 
                CIpasir s c l -> IO (CIpasir s c l, ESolution l)
    solve c@(CIpasir solver cache) = do
        (Just sat) <- ipasirSolve solver
        ret <- readSolution c sat
        return (c, ret)

    solveAllOverVars :: forall c l. (Ord l, LiteralCache c) => 
                CIpasir s c l -> [l] -> IO (CIpasir s c l, [Solution l] )
    solveAllOverVars c@(CIpasir solver cache) iterateVars = do
        solutions <- whileM (fromJust <$> ipasirSolve solver) $ do
            addClauseOfSolution
            (Left sol) <- readSolution c True
            return sol

        return (c, solutions)
        where
            intLits :: [Int]
            intLits = traceShowId $ map (varToInt cache . Right) iterateVars
            addClauseOfSolution :: IO ()
            addClauseOfSolution = do
                solution <- mapM (readVal solver) intLits
                let newClause = clause solution
                print newClause
                ipasirAddClause newClause solver
            clause :: (Enum e) => [Val] -> [Lit e]
            clause solution = do
                (int, sol) <- zip intLits solution
                guard $ isJust sol
                if fromJust sol
                    then return $ Neg $ toEnum int
                    else return $ Pos $ toEnum int


readSolution :: forall s c l. (Ord l, CSolver s, LiteralCache c) => CIpasir s c l -> Bool -> IO (ESolution l)
readSolution (CIpasir solver cache) sat = if sat
    then Left  <$> toMap (readVal solver)
    else Right <$> toMap (readFail solver)
        where
            toMap :: (Int -> IO a) -> IO (Map.Map l a)
            toMap readRes = do
                pairs <- zipWithM readPair labels vars
                return $ Map.fromList [ (v, r) | (Right v, r) <- pairs]
                where
                    readPair v i = (v,) <$> readRes i
            labels = intToVar cache `map` vars
            vars = [1..numVars cache]

readVal :: CSolver s => s -> Int -> IO Val
readVal solver = ((sign <$>) <$>) . (`ipasirVal` solver) . toEnum
readFail :: CSolver s => s -> Int -> IO Bool
readFail solver = (`ipasirFailed` solver) . toEnum
