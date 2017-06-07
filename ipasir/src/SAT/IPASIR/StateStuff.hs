{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module SAT.IPASIR.StateStuff where

import Data.Traversable
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Primitive
import Control.Lens

import Data.Monoid
import SAT.IPASIR.Literals

import qualified Data.Map as M


type Var = Maybe Bool

type Solution v = M.Map v Var
type Conflict v = M.Map v Bool
type ESolution v = Either (Solution v) (Conflict v)

data CryptoMiniSat = CryptoMiniSat
data MiniSat = MiniSat

class (Ord (ClausesLabel c), MSolver s) => MClauses s (c :: *) where
    type MClausesLabel c
    addClauses :: (Traversable m, ClausesLabel c ~ l, Clauses s c) => c -> StateT (m (s l)) IO ()

class MSolver (s :: * -> *) where
    type MSolverMarker s
    newMSolver :: (Applicative m, Ord l) => StateT (m (s l)) IO ()
    
    mSolve :: forall m l. (Traversable m, Ord l) => StateT (m (s l)) IO (m (ESolution l))

data CCryptominiSat = CCryptominiSat
newtype MCryptoMiniSat l = MCryptoMiniSat CCryptominiSat

runSolver' :: (Monoid a, Monad m) => StateT a m b -> m b
runSolver' s = evalStateT s mempty 

runSolver :: (MSolver s, Ord l) => StateT (Identity (s l)) IO a -> IO a
runSolver s = do
    (Last (Just solver)) <- runSolver' newMSolver
    evalStateT (Identity solver) s

main :: IO ()
main = runSolver' operation


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
