module Spec.SAT.IPASIR where

import qualified Data.Map as Map
import SAT.IPASIR

data MySolver l = MySolver deriving Show

instance Solver MySolver where
    new = return MySolver
    addClauses s c = return s
    solve s = return (s, Left Map.empty)

main :: IO ()
main = do
    s <- new :: IO (MySolver String)
    s' <- addClauses s [[Pos "a"]]
    (s'', _) <- solve s'
    putStrLn "\nBasic API works"
