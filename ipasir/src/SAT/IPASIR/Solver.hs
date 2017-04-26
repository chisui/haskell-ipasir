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

import Data.Either
import qualified Data.Map as Map

import SAT.IPASIR.CSolver
import SAT.IPASIR.LiteralCache
import SAT.IPASIR.Literals

import Debug.Trace


type Val = Maybe Bool

class (Ord (ClausesLabel c)) => Clauses c where
    type ClausesLabel c
    toClauses :: Clauses c => c -> [[ELit (ClausesLabel c)]]

instance (Ord l) => Clauses [[Lit l]] where
    type ClausesLabel [[Lit l]] = l
    toClauses = map (map (Right <$>))

class Solver (s :: * -> *) where
    new :: forall l. IO (s l)
    addClauses :: (Clauses c, l ~ ClausesLabel c) => s l -> c -> IO (s l)
    solve :: (Ord l) => s l -> IO ( s l, Either (Map.Map l Val) (Map.Map l Bool) )



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
    
    addClauses :: forall c l x. (Ord l, LiteralCache c, Clauses x, l ~ ClausesLabel x) => CIpasir s c l -> x -> IO (CIpasir s c l)
    addClauses (CIpasir solver cache) rawClauses = do
        ipasirAddClauses intClauses solver
        traceShowM intClauses
        return $ CIpasir solver cache'
        where
            clauses :: [[ELit l]]
            clauses = toClauses rawClauses
            intClauses :: [[Lit Word]]
            intClauses = varToInt cache' <$$$> clauses
            vars :: [Ext l]
            vars = ordinal <$> concat clauses
            cache' = cache `insertVars` vars
            (<$$$>)=(<$>).(<$>).(<$>)
        


