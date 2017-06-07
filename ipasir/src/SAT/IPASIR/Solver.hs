{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
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

import Data.Either
import Data.Maybe
import qualified Data.Map as Map

import SAT.IPASIR.Api
import SAT.IPASIR.LiteralCache
import SAT.IPASIR.Literals


type Val = Maybe Bool

type Solution v = Map.Map v Val
type Conflict v = Map.Map v Bool
type ESolution v = Either (Solution v) (Conflict v)

class (MSolver m) => Solver (m :: *)  where
    solve :: (Clauses m c, v ~ VariableType c) => m -> c -> ESolution v
    solve m c = runIdentity $ unsafePerformIO $ runSolver m $ do
        addClauses c
        mSolve

    solveAllForVars' :: (Clauses m c, VariableType c ~ v) => m -> c -> [v] -> ([Solution v], Conflict v)
    solveAllForVars' m c ls = runIdentity $ unsafePerformIO $ runSolver m $ do
        addClauses c
        mSolveAllForVars ls

    solveAllForVars :: (Clauses m c) => m -> c -> [VariableType c] -> [Solution (VariableType c)]
    solveAllForVars m c ls = fst $ solveAllForVars' m c ls
    solveAll :: (Clauses m c) => m -> c -> [Solution (VariableType c)]
    solveAll m c = solveAllForVars m c $ getVars c


class (Ord (VariableType c)) => HasVariables c where
    type VariableType c
    getVars :: c -> [VariableType c]

class (HasVariables c, MSolver marker) => Clauses marker c where
    addClauses :: (s ~ MSolver' marker, Traversable m, Clauses marker c) => c -> StateT (m (s (VariableType c))) IO ()

class MSolver (marker :: *) where
    type family MSolver' marker = (s :: * -> *) | s -> marker
    newMSolver :: (Applicative m, Monoid (m (s v)), Ord v) => marker -> StateT (m (s v)) IO ()
    
    mSolve :: forall m v. (Traversable m, Ord v) => StateT (m (MSolver' marker v)) IO (m (ESolution v))
    mSolveAllForVars :: forall m v. (Traversable m, Ord v) => [v] -> StateT (m (MSolver' marker v)) IO (m ([Solution v], Conflict v))
 
runSolver' :: (Monoid a, Monad m) => StateT a m b -> m b
runSolver' s = evalStateT s mempty 

runSolver :: (MSolver m, Ord v) => m -> StateT (Identity (MSolver' m v)) IO a -> IO a
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
