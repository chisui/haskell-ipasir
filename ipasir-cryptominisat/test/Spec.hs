{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import SAT.IPASIR
import SAT.IPASIR.Cryptominisat
import Data.Either
import Control.Monad

main :: IO ()
main = do
    s <- new :: IO (Cryptominisat String)
    s' <- addClauses s (Some [
            All [     "a",      "b", notB "c"],
            All [     "a", notB "b", notB "c"],
            All [     "a", notB "b",      "c"],
            All [notB "a", notB "b", notB "c"]
        ])
    s'' <- addClauses s' (Even ["a", "b", "c"])
    (_, solution) <- solve s''
    unless (isLeft solution) $ error "should be solvable"
    