{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module SAT.IPASIR.State where

import SAT.IPASIR.Solver
import Control.Monad.Trans.State.Lazy

import qualified Data.Map as M


class StatefulSolver (s :: * -> *) where
    newSolver   :: Ord l => IO (s l)
    solveSolver :: Ord l => s l -> (ESolution l, s l)

instance (Solver s) => StatefulClause s where
    newSolver = new
    solveSolver = undefined

class (Ord (StClausesLabel c), StatefulSolver s) => StatefulClause s (c :: *) where
    type StClausesLabel c
    addClauses :: (Traversable m, Ord l, StatefulClause s c) => c -> StateT (m (s l)) IO ()

instance (Clauses s c, Ord (StClausesLabel c), StatefulSolver s) => StatefulClause s c where
    type StClausesLabel c = ClausesLabel c
    addClauses :: (Traversable m, Ord l, StatefulClause s c) => c -> StateT (m (s l)) IO ()
    addClauses clauses = do
        solver  <- get
        solver' <- lift $ mapM (addClauses clauses) solver
        put solver'
