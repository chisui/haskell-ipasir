module Main (main) where

import SAT.IPASIR
import SAT.IPASIR.Cryptominisat.C

main :: IO ()
main = do
    putStrLn "signature is:"
    solver <- ipasirInit :: IO CryptominisatSolver
    print =<< ipasirSignature solver
    ipasirAddClause [Pos 0, Neg 1, Pos 2] solver
    print =<< ipasirSolve solver
    l0 <- ipasirVal 0 solver
    l1 <- ipasirVal 1 solver
    l2 <- ipasirVal 2 solver
    print [l0,l1,l2]

