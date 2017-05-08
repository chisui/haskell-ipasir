{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module SAT.IPASIR.StateStuff where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Primitive
import Control.Lens

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

    stAddClauses :: (Ord l, IsMClause s c) => c -> StateT (Eventual (s l)) IO ()
    stAddClauses clauses = do
        (Present solver) <- get
        solver' <- lift $ mAddClauses clauses solver
        put $ Present solver'

class MSolver (s :: * -> *) where
    newSolver :: (Ord l) => IO (s l)
    mSolve    :: (Ord l) => s l -> IO (ESolution l, s l)
    
    stNewSolver :: (Ord l) => StateT (Eventual (s l)) IO ()
    stNewSolver = do
        solver <- lift newSolver
        put $ Present solver
    
    stSolve :: (Ord l) => StateT (Eventual (s l)) IO (ESolution l)
    stSolve = do
        (Present solver) <- get
        (solution, solver') <- lift $ mSolve solver
        put $ Present solver'
        return solution
        

data CCryptominiSat = CCryptominiSat
newtype MCryptoMiniSat l = MCryptoMiniSat CCryptominiSat

newCryptominiSat :: (Ord l) => l -> StateT (Eventual (MCryptoMiniSat l)) IO ()
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

data Eventual a 
    = NotYet
    | Present a

instance Monoid (Eventual a) where
    mempty = NotYet
    mappend _ e@(Present _) = e
    mappend e _ = e

runSolver :: (Monoid a, Monad m) => StateT a m b -> m b
runSolver s = evalStateT s mempty 

main :: IO ()
main = runSolver operation


operation :: StateT (Eventual (MCryptoMiniSat String), Eventual (MCryptoMiniSat String)) IO ()
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
