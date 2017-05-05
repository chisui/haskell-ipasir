{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Spec.SAT.IPASIR where

import qualified Data.Map as Map
import SAT.IPASIR

data MySolver l = MySolver deriving Show

instance Solver MySolver where
    new = return MySolver
    solve s = return (s, Left Map.empty)
    solveAllOverVars = undefined
instance (Ord l) => Clauses MySolver [[Lit l]] where
    type ClausesLabel [[Lit l]] = l
    addClauses s _ = return s

main :: IO ()
main = do
    s <- new :: IO (MySolver String)
    s' <- addClauses s [[Pos "a"]]
    (s'', _) <- solve s'
    putStrLn "\nBasic API works"
