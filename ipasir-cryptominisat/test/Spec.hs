module Main (main) where

import SAT.IPASIR
import SAT.IPASIR.Cryptominisat
import Data.Either
import Data.Functor.Identity
import Control.Monad

main :: IO ()
main = print $ solveAll cryptoMiniSat (Even [Var "a", Var "b", Var "c"])
    --solution <- runIdentity <$> mSolveAll
    --unless (isLeft solution) $ error "should be solvable"
    