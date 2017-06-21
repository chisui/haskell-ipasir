module Main (main) where

import SAT.IPASIR
import SAT.IPASIR.Cryptominisat
import Data.Either
import Data.Functor.Identity
import Control.Monad

main :: IO ()
main = print $ cryptoMiniSat `solveAll` Odd [var "a", var "b", var "c"]
    --solution <- runIdentity <$> mSolveAll
    --unless (isLeft solution) $ error "should be solvable"
    