module Spec.SAT.IPASIR where

import SAT.IPASIR

data MySolver l = MySolver deriving Show

instance Solver MySolver where
    new = return MySolver
    addClauses s c = return s
    solve s = return (s, Nothing)

main :: IO ()
main = do
    s <- new :: IO (MySolver String)
    s' <- addClauses s [[Pos "a"]]
    (s'', Nothing) <- solve s'
    putStrLn "\nBasic API works"
