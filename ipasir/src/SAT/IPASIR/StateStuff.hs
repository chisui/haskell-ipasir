{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module SAT.IPASIR.StateStuff where

import Data.Traversable
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Primitive
import Control.Lens

import Data.Monoid

import qualified Data.Map as M


type Var = Maybe Bool

type Solution v = M.Map v Var
type Conflict v = M.Map v Bool
type ESolution v = Either (Solution v) (Conflict v)

data CryptoMiniSat = CryptoMiniSat
data MiniSat = MiniSat

class (Ord (ClausesLabel c), Solver s) => IsClause s (c :: *) where
    type ClausesLabel c
    addClauses :: (ClausesLabel c ~ l) => s -> c -> IO s

class Solver s where
    solveAssuming   :: (IsClause s c) => s -> c -> [v] -> ESolution v
    
    solve           :: (IsClause s c) => s -> c -> ESolution v
    solve       s c = solveAssuming s c []


class (Ord (StClausesLabel c), MSolver s) => IsMClause s (c :: *) where
    type StClausesLabel c
    mAddClauses :: (Ord l) => c -> s l -> IO (s l)

    stAddClauses :: (Traversable m, Ord l, IsMClause s c) => c -> StateT (m (s l)) IO ()
    stAddClauses clauses = do
        solver  <- get
        solver' <- lift $ mapM (mAddClauses clauses) solver
        put solver'

class MSolver (s :: * -> *) where
    newSolver :: (Ord l) => IO (s l)
    mSolve    :: (Ord l) => s l -> IO (ESolution l, s l)
    
    stNewSolver :: (Applicative m, Ord l) => StateT (m (s l)) IO ()
    stNewSolver = put . pure =<< lift newSolver
    
    stSolve :: forall m l. (Traversable m, Applicative m, Ord l) => StateT (m (s l)) IO (m (ESolution l))
    stSolve = do
        solver <- get
        do 
            r <- lift $ mapM mSolve solver :: StateT (m (s l)) IO (m (ESolution l, s l))
            mapM (put . pure . snd) r
            mapM (return . fst) r

data CCryptominiSat = CCryptominiSat
newtype MCryptoMiniSat l = MCryptoMiniSat CCryptominiSat

newCryptominiSat :: (Applicative m, Monoid (m (MCryptoMiniSat l)), Ord l) => l -> StateT (m (MCryptoMiniSat l)) IO ()
newCryptominiSat _ = stNewSolver

instance MSolver MCryptoMiniSat where
    newSolver = undefined
    mSolve = undefined

data Lit a
    = Pos a
    | Neg a
        deriving (Eq, Ord)

instance (Ord l) => IsMClause MCryptoMiniSat [[Lit l]] where
    type StClausesLabel [[Lit l]] = l
    mAddClauses = undefined

runSolver :: (Monoid a, Monad m) => StateT a m b -> m b
runSolver s = evalStateT s mempty 

main :: IO ()
main = runSolver operation


operation :: StateT (Last (MCryptoMiniSat String), Last (MCryptoMiniSat String)) IO ()
operation = do
    _2 =.= newCryptominiSat (undefined :: String)
    _1 =.= stAddClauses [[Pos ""]]
    solution <- _2 =.= stSolve
    lift $ print solution
    return ()

-- type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
(=.=) :: (Monad m) => Lens' s a -> StateT a m b -> StateT s m b
ln =.= st = do
    s <- get
    (b, s') <- lift $ runStateT st $ s ^. ln
    put $ set ln s' s
    return b
