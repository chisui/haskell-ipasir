{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SAT.IPASIR.Solver where

import Control.Monad
import Data.Functor.Identity
import Control.Monad.Loops
import Control.Comonad
import System.IO.Unsafe
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Data.Monoid
import Control.Lens
import Data.Kind

import Data.Either
import Data.Maybe
import qualified Data.Map as Map

import SAT.IPASIR.Api
import SAT.IPASIR.Literals
import SAT.IPASIR.VarCache

type Val = Maybe Bool
type Solution v = Map.Map (Var v) Val
type Conflict v = Map.Map (Var v) Bool
type ESolution v = Either (Conflict v) (Solution v)

class (Ord (VariableType c)) => HasVariables c where
    type VariableType c
    getVars :: c -> VarCache (VariableType c) -> [Var (VariableType c)]

class (HasVariables c) => Clauses s c where
    addClauses :: (MSolver s, Traversable m, Clauses s c) => c -> StateT (m (s (VariableType c))) IO ()

class MSolver (s :: * -> *) where

    type family Marker s = marker | marker -> s

    newMSolver :: (Ord v, Applicative m, Monoid (m (s v))) => Marker s -> StateT (m (s v)) IO ()
    mSolve :: (Ord v, Traversable m) => StateT (m (s v)) IO (m (ESolution v))
    mSolveAllForVars :: (Ord v, Traversable m) => [Var v] -> StateT (m (s v)) IO (m (Conflict v, [Solution v]))

class (MSolver s) => Solver s where
    
    solve :: (Clauses s c, v ~ VariableType c, Ord v) => Marker s -> c -> ESolution v
    solve m c = runIdentity $ unsafePerformIO $ runSolver m $ do
        addClauses c
        mSolve

    solveAllForVars' :: (Ord v, Clauses s c, VariableType c ~ v) => Marker s -> c -> [Var v] -> (Conflict v, [Solution v])
    solveAllForVars' m c ls = runIdentity $ unsafePerformIO $ runSolver m $ do
        addClauses c
        mSolveAllForVars ls

    solveAllForVars :: (Clauses s c, VariableType c ~ v) => Marker s -> c -> [Var v] -> [Solution v]
    solveAllForVars m c ls = snd $ solveAllForVars' m c ls
    solveAll :: (Clauses s c, VariableType c ~ v) => Marker s -> c -> [Solution v]
    solveAll m c = solveAllForVars m c $ getVars c emptyCache


runSolver' :: (Monoid a, Monad m) => StateT a m b -> m b
runSolver' s = evalStateT s mempty

runSolver :: (MSolver s, Ord v) => Marker s -> StateT (Identity (s v)) IO a -> IO a
runSolver m s = do
    (Just solver) <- getLast <$> execStateT (newMSolver m) mempty
    evalStateT s (Identity solver)

-- type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
(=.=) :: (Monad m) => Lens' s a -> StateT a m b -> StateT s m b
ln =.= st = do
    s <- get
    (b, s') <- lift $ runStateT st $ s ^. ln
    put $ set ln s' s
    return b
