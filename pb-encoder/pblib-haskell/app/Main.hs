module Main (main) where

import Data.Word
import SAT.PseudoBoolean

import Debug.Trace

main :: IO ()
main = mapM_ print $ runEncoder (defaultConfig :: Config PseudoBoolean) lits CBoth 5 100 4 $ do
    clauses0 <- getClauses
    clauses1 <- encodeNewGeq 10
    clauses2 <- encodeNewLeq 50
    return clauses1 -- (clauses0, clauses1, clauses2)


lits =
    [ 1 $-$ 5
    , 2 $-$ 1
    , 3 $-$ 5
    ]
