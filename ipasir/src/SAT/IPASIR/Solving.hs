module SAT.IPASIR.Solving where

import SAT.IPASIR.ClausesCache
import SAT.IPASIR.CSolver
import SAT.IPASIR.Formula

import qualified Data.Map as M

type Var = Maybe Bool

type Solution  = M.Map v Var
type Conflict  = M.Map v Bool
type ESolution = Either Solution Conflict

data CryptoMiniSat = CryptoMiniSat
data MiniSat = MiniSat

class Solver s where
    solveAssuming   :: (IsClause s c) => s -> c v -> [v] -> ESolution
    
    solve           :: (IsClause s c) => s -> c v -> ESolution
    solve       s c = solveAssuming s c []
    solveAll        :: (IsClause s c) => s -> c v -> [Solution]
    solveAll        = undefined
    solveAllIn      :: (IsClause s c) => s -> c v -> [v] -> [Solution]
    solveAllIn      = undefined
    solveMinimizing :: (IsClause s c) => s -> c v -> [v] -> [Solution]
    solveMinimizing = undefined
    solveMaximizing :: (IsClause s c) => s -> c v -> [v] -> [Solution]
    solveMaximizing = undefined

class MSolver (s :: * -> * -> *) where
    type SolverType s
    newSolver :: (PrimMonad m, Ord l) => SolverType s -> m (s l m)
    addClause :: (PrimMonad m, Ord l, IsClause (SolverType s) c l) => c -> s l m -> m (s l m)
    mSolve :: (PrimMonad m, Ord l) => SolverType s -> m (ESolution, s l m)

type CCryptominiSat = ForeignPtr ()
type MCryptoMiniSat l m = CIpasir CCryptominiSat

instance MSolver MCryptoMiniSat where
    type SolverType MCryptoMiniSat = CryptoMiniSat
    newSolver = undefined
