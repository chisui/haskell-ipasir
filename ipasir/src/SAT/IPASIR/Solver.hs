{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module SAT.IPASIR.Solver where

import System.IO.Unsafe

import Control.Monad
import Control.Monad.Loops
import Control.Comonad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Lens

import Data.Bifunctor
import Data.Either
import Data.Maybe
import Data.Proxy
import Data.Kind
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

import SAT.IPASIR.Api
import SAT.IPASIR.Literals
import SAT.IPASIR.VarCache

-- | A solution for a single variable.
-- @Just a@ indicates that the variable is @a@ in the solution
-- @Nothing@ indicates that the variable is not important for the solution.
-- both @True@ and @False@ are valid assignments.
-- 
-- Working with this representation may be cumbersome. If you do not want to
-- deal with unimportant variables pass your solutions through @expandSolution@.
type Val = Maybe Bool

-- | A solution of a SAT-Solver.
-- The keys contain both variables and helpers.
-- If you don't care about helpers use @withoutHelpers@
type Solution v = Map.Map (Var v) Val

-- | A conflict of a SAT-Solver.
-- The @Set@ contains all conflicting variables.
type Conflict v = Set.Set (Var v)

-- | The result of a SAT-Solver.
type ESolution v = Either (Conflict v) (Solution v)

-- | Base class for all @Clauses@.
-- It defines the @VariableType@ family that indicates what type of varibales @c@ contains.
-- Additionally it defines multiple functions to access the variables of @c@. This
-- is mainly used for statistics.
class (Ord (VariableType c)) => HasVariables c where
    {-# MINIMAL getAllVariables #-}
    -- | Defines what type variables of @c@ have
    type VariableType c

    -- | Extracts all variables (helper or not) from @c@.
    -- new helpers may be constructed using the @VarCache@.
    -- if @c@ has multiple occurances of a variable it has to be included multiple times in the result.
    getAllVariables :: c -> VarCache (VariableType c) -> [Var (VariableType c)]
    -- | Extract all labels (variables that aren't helpers) from @c@
    -- if @c@ has multiple occurances of a variable it has to be included multiple times in the result.
    getAllLabels :: c -> [VariableType c]
    getAllLabels c = rights $ getAllVariables c emptyCache
    -- | Extract all helpers from @c@.
    -- new helpers have to be created using the @VarCache@
    -- if @c@ has multiple occurances of a variable it has to be included multiple times in the result.
    getAllHelpers :: c -> VarCache (VariableType c) -> [Word]
    getAllHelpers c vc = lefts $ getAllVariables c vc
    
    -- | Extracts all variables (helper or not) from @c@.
    -- new helpers may be constructed using the @VarCache@.
    getVariables :: c -> VarCache (VariableType c) -> Set.Set (Var (VariableType c))
    getVariables c vc = Set.fromList $ getAllVariables c vc
    -- | Extract all labels (variables that aren't helpers) from @c@
    getLabels :: c -> Set.Set (VariableType c)
    getLabels c = Set.fromList $ getAllLabels c
    -- | Extract all helpers from @c@.
    -- new helpers have to be created using the @VarCache@
    getHelpers :: c -> VarCache (VariableType c) -> Set.Set Word
    getHelpers c vc = Set.fromList $ getAllHelpers c vc

-- | Everything that can be added into a solver @s@ has to implement @Clauses@
class (MSolver s, HasVariables c) => Clauses s c where

    -- | add @c@ to all solvers @s@ in @m@.    
    addClauses :: Traversable m => c -> StateT (m (s (VariableType c))) IO ()

-- | Monadic Solver.
class MSolver (s :: * -> *) where

    -- | Create a new solver of type @s@ and add it to the state.
    newMSolver :: (Ord v, Applicative m, Monoid (m (s v))) => Proxy (s v) -> StateT (m (s v)) IO ()
    --{-# SPECIALIZE newMSolver :: Ord v => Proxy (s v) -> StateT (Last (s v)) IO () #-}
    --{-# SPECIALIZE newMSolver :: Ord v => Proxy (s v) -> StateT [s v] IO () #-}

    -- | Find a Solution for each solver inside @m@.
    mSolve :: (Ord v, Traversable m) => StateT (m (s v)) IO (m (ESolution v))
    --{-# SPECIALIZE mSolve :: Ord v => StateT (Last (s v)) IO (Last (ESolution v)) #-}
    --{-# SPECIALIZE mSolve :: Ord v => StateT [s v] IO [ESolution v] #-}

    -- | Find all Solutions  for each solver inside @m@.
    mSolveAllForVars :: (Ord v, Traversable m) => [Var v] -> StateT (m (s v)) IO (m (Conflict v, [Solution v]))
    --{-# SPECIALIZE mSolveAllForVars :: Ord v => [Var v] -> StateT (Last (s v)) IO (Last (Conflict v, [Solution v])) #-}
    --{-# SPECIALIZE mSolveAllForVars :: Ord v => [Var v] -> StateT [s v] IO [(Conflict v, [Solution v])] #-}

class (MSolver s) => Solver s where
    
    solve :: (Clauses s c, v ~ VariableType c, Ord v) => Proxy (s v) -> c -> ESolution v
    solve m c = runIdentity $ unsafePerformIO $ runSolver m $ do
        addClauses c
        mSolve

    solve_ :: (Clauses s c, v ~ VariableType c, Ord v) => Proxy (s v) -> c -> Either (Conflict v) (Map.Map v Bool)
    solve_ m c = second (withoutHelpers . head . expandSolution) $ solve m c

    solveAllForVars' :: (Ord v, Clauses s c, VariableType c ~ v) => Proxy (s v) -> c -> [Var v] -> (Conflict v, [Solution v])
    solveAllForVars' m c ls = runIdentity $ unsafePerformIO $ runSolver m $ do
        addClauses c
        mSolveAllForVars ls

    solveAllForVars :: (Clauses s c, VariableType c ~ v) => Proxy (s v) -> c -> [Var v] -> [Solution v]
    solveAllForVars m c ls = snd $ solveAllForVars' m c ls
    solveAll :: (Clauses s c, VariableType c ~ v) => Proxy (s v) -> c -> [Solution v]
    solveAll m c = solveAllForVars m c $ Set.toList $ getVariables c emptyCache

    solveAll_ :: (Clauses s c, VariableType c ~ v) => Proxy (s v) -> c -> [Map.Map v Bool]
    solveAll_ m c = map withoutHelpers $ expandSolution =<< solveAll m c

expandSolution :: (Traversable t, Applicative f, Monoid (f Bool), Monoid (f (Maybe Bool))) => t (Maybe Bool) -> f (t Bool)
{-# SPECIALIZE expandSolution :: Solution v -> [Map.Map (Var v) Bool] #-}
{-# SPECIALIZE expandSolution :: Solution v -> First (Map.Map (Var v) Bool) #-}
{-# SPECIALIZE expandSolution :: Solution v -> Last (Map.Map (Var v) Bool) #-}
expandSolution = traverse $ maybe (pure True <> pure False) pure

withoutHelpers :: Ord v => Map.Map (Var v) r -> Map.Map v r
withoutHelpers = Map.mapKeys (\(Right v) -> v) . Map.filterWithKey (const . isRight)

runSolver' :: (Monoid a, Monad m) => StateT a m b -> m b
runSolver' s = evalStateT s mempty

runSolver :: (MSolver s, Ord v) => Proxy (s v) -> StateT (Identity (s v)) IO a -> IO a
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
