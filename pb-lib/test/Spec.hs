module Main (main) where

import Control.Monad.Trans.Class

import Data.Word
import SAT.PseudoBoolean

import Debug.Trace

main :: IO ()
main = evalEncoder (defaultConfig :: Config PseudoBoolean) lits CBoth 5 100 4 $ do
    clauses0 <- getClauses
    clauses1 <- encodeNewGeq 10
    clauses2 <- encodeNewLeq 50
    lift $ print clauses0
    lift $ print clauses1
    lift $ print clauses2
    return ()


lits =
    [ 1 $-$ 5
    , 2 $-$ 1
    , 3 $-$ 5
    ]
