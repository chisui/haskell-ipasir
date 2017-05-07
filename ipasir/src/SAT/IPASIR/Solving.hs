module SAT.IPASIR.Solving where

import SAT.IPASIR.ClausesCache
import SAT.IPASIR.CSolver
import SAT.IPASIR.Formula

import qualified Data.Map as M

type Var = Maybe Bool

solve           :: (ClausesForm cf, Solver s) => s -> cf v -> Either ( M.Map v Var, M.Map v Bool ) 
solve           = undefined
solveAssuming   :: (ClausesForm cf, Solver s) => s -> cf v -> [v] -> Either ( M.Map v Var, M.Map v Bool ) 
solveAssuming   = undefined
solveAll        :: (ClausesForm cf, Solver s) => s -> cf v -> [M.Map v Var]
solveAll        = undefined
solveAllIn      :: (ClausesForm cf, Solver s) => s -> cf v -> [v] -> [M.Map v Var]
solveAllIn      = undefined
solveMinimizing :: (ClausesForm cf, Solver s) => s -> cf v -> [v] -> [M.Map v Var]
solveMinimizing = undefined
solveMaximizing :: (ClausesForm cf, Solver s) => s -> cf v -> [v] -> [M.Map v Var]
solveMaximizing = undefined



