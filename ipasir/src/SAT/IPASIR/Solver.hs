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
import SAT.IPASIR.LiteralCache
import SAT.IPASIR.Literals

type Val = Maybe Bool
type Solution v = Map.Map v Val
type Conflict v = Map.Map v Bool
type ESolution v = Either (Conflict v) (Solution v)

class (Ord (VariableType c)) => HasVariables c where
    type VariableType c
    getVars :: c -> [VariableType c]

class (HasVariables c) => Clauses s c where
    addClauses :: (LiteralCache lc (VariableType c), MSolver s lc (VariableType c), Traversable m, Clauses s c) => c -> StateT (m (s lc (VariableType c))) IO ()

class (LiteralCache lc v, Ord v) => MSolver s lc v where

    type family Marker s lc = marker | marker -> s lc

    newMSolver :: (Applicative m, Monoid (m (s lc v))) => Marker s lc -> StateT (m (s lc v)) IO ()
    mSolve :: Traversable m => StateT (m (s lc v)) IO (m (ESolution v))
    mSolveAllForVars :: Traversable m => [v] -> StateT (m (s lc v)) IO (m (Conflict v, [Solution v]))

class (MSolver s lc v) => Solver s lc v where
    solve :: (Clauses s c, v ~ VariableType c) => Marker s lc -> c -> ESolution v
    solve m c = runIdentity $ unsafePerformIO $ runSolver m $ do
        addClauses c
        mSolve

    solveAllForVars' :: (Clauses s c, VariableType c ~ v) => Marker s lc -> c -> [v] -> (Conflict v, [Solution v])
    solveAllForVars' m c ls = runIdentity $ unsafePerformIO $ runSolver m $ do
        addClauses c
        mSolveAllForVars ls

    solveAllForVars :: (Clauses s c, VariableType c ~ v) => Marker s lc -> c -> [v] -> [Solution v]
    solveAllForVars m c ls = snd $ solveAllForVars' m c ls
    solveAll :: (Clauses s c, VariableType c ~ v) => Marker s lc -> c -> [Solution v]
    solveAll m c = solveAllForVars m c $ getVars c


runSolver' :: (Monoid a, Monad m) => StateT a m b -> m b
runSolver' s = evalStateT s mempty

runSolver :: (MSolver s lc v, Ord v) => Marker s lc -> StateT (Identity (s lc v)) IO a -> IO a
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