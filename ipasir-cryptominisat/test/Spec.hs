module Main (main) where

import SAT.IPASIR
import SAT.IPASIR.Cryptominisat
import SAT.IPASIR.Cryptominisat.C


main :: IO ()
main = do
    s <- new :: IO (Cryptominisat String)
    s' <- addClauses s [[Pos "a", Pos "b", Pos "c"]]
    (s'', solution) <- solve s'
    print solution
    putStrLn "\nBasic API works"

    
