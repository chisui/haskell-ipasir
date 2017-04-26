module Main (main) where

import SAT.IPASIR
import SAT.IPASIR.Cryptominisat
import SAT.IPASIR.Cryptominisat.C

import Debug.Trace

main :: IO ()
main = do
    {-
    putStrLn "signature is:"
    solver <- ipasirInit :: IO CryptominisatSolver
    print =<< ipasirSignature solver
   -- ipasirAddClause [Pos 0, Neg 1, Pos 2] solver
    ipasirAddClause [Pos 0, Pos 1, Pos 2] solver
    print =<< ipasirSolve solver
    l0 <- ipasirVal 0 solver
    l1 <- ipasirVal 1 solver
    l2 <- ipasirVal 2 solver
    print [l0,l1,l2]
    -}

    s <- new :: IO (Cryptominisat LitCache String)
    s' <- addClauses s [[Pos "a", Pos "b", Pos "c"]]
    (s'', solution) <- solve s'
    print solution
    putStrLn "\nBasic API works"

    
