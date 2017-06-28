{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Spec.SAT.IPASIR where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Monoid hiding (All)
import Data.Proxy
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import SAT.IPASIR
import SAT.IPASIR.Cryptominisat


mySolver = Proxy :: Proxy (MySolver v)
data MySolver v = MySolver deriving Show

instance MSolver MySolver where
    newMSolver _ = do
        oldSolvers <- get 
        put $ oldSolvers <> pure MySolver
        return ()
    mSolve = do
        solvers <- get
        lift $ mapM (const $ return $ Left Set.empty) solvers
    mSolveAllForVars _ = undefined
instance (Ord l) => Clauses MySolver [[Lit l]] where
    addClauses _ = return ()

{-
main :: IO ()
main = runSolver mySolver $ do
    addClauses [[Pos "a"]]
    _ <- mSolve
    lift $ putStrLn "\nBasic API works"
-}

s1 = "asdf"
s2 = "asdfghijz''"
main = solve cryptoMiniSat $ All [Some $ map var s1, Odd $ map var s2]

