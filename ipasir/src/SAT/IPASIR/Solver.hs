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

import Data.Either
import Data.Maybe
import qualified Data.Map as Map

import SAT.IPASIR.CSolver
import SAT.IPASIR.LiteralCache
import SAT.IPASIR.Literals

type Val = Maybe Bool

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
            vars = ordinal <$> concat clauses
            cache' = cache `insertVars` vars
            (<$$$>)=(<$>).(<$>).(<$>)

class Solver (s :: * -> *) where
    new :: forall l. IO (s l)
    solve :: (Ord l) => s l -> IO ( s l, Either (Map.Map l Val) (Map.Map l Bool) )
    solveAllOverVars :: (Ord l) => s l -> [l] -> IO ( s l, [Map.Map l Val] )


data CIpasir s c l = CIpasir s (c (Ext l))

instance (CSolver s, LiteralCache c) => Solver (CIpasir s c) where
    new = do
        solver <- ipasirInit
        return $ CIpasir solver emptyCache

    solve :: forall c l. (Ord l, LiteralCache c) => 
                CIpasir s c l -> IO (CIpasir s c l, Either (Map.Map l Val) (Map.Map l Bool) )
    solve c@(CIpasir solver cache) = do
        (Just sat) <- ipasirSolve solver
        ret <- if sat
            then Left  <$> toMap readVal
            else Right <$> toMap readFail
        return (c, ret)
        where
            toMap :: (Int -> IO a) -> IO (Map.Map l a)
            toMap f = do
                pairs <- zipWithM g labels vars
                return $ Map.fromList pairs
                where
                    g v i = (v,) <$> f i
            labels = rights $ intToVar cache `map` vars
            vars = [0..numVars cache - 1]
            readVal :: Int -> IO Val
            readVal = ((sign <$>) <$>) . (`ipasirVal` solver) . toEnum
            readFail :: Int -> IO Bool
            readFail = (`ipasirFailed` solver) . toEnum


    solveAllOverVars :: forall c l. (Ord l, LiteralCache c) => 
                CIpasir s c l -> [l] -> IO (CIpasir s c l, [Map.Map l Val] )
    solveAllOverVars c@(CIpasir solver cache) iterateVars = do
        solutions <- whileM (fromJust <$> ipasirSolve solver) $ do
            addClauseOfSolution
            toMap readVal

        return (c, solutions)
        where
            intLits :: [Int]
            intLits = map (varToInt cache . Right) iterateVars
            toMap :: (Int -> IO a) -> IO (Map.Map l a)
            toMap f = do
                pairs <- zipWithM g labels vars
                return $ Map.fromList pairs
                where
                    g v i = (v,) <$> f i
            addClauseOfSolution :: IO ()
            addClauseOfSolution = do
                solution <- mapM readVal intLits
                ipasirAddClause (clause solution) solver
            clause :: (Enum e) => [Val] -> [Lit e]
            clause solution = do
                (int, sol) <- zip intLits solution
                guard $ isNothing sol
                if fromJust sol
                    then return $ Neg $ toEnum int
                    else return $ Pos $ toEnum int
            labels = rights $ intToVar cache `map` vars
            vars = [0..numVars cache - 1]
            readVal :: Int -> IO Val
            readVal = ((sign <$>) <$>) . (`ipasirVal` solver) . toEnum
            readFail :: Int -> IO Bool
            readFail = (`ipasirFailed` solver) . toEnum
