{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Spec.SAT.IPASIR where

import qualified Data.Map as Map
import Data.Monoid
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import SAT.IPASIR


data MySolver v = MySolver deriving Show
data MyMarker = MyMarker

instance MSolver MySolver where
    type Marker MySolver = MyMarker
    newMSolver _ = do
        oldSolvers <- get 
        put $ oldSolvers <> pure MySolver
        return ()
    mSolve = do
        solvers <- get
        lift $ mapM (const $ return $ Left Map.empty) solvers
    mSolveAllForVars _ = undefined
instance (Ord l) => Clauses MySolver [[Lit l]] where
    addClauses _ = return ()

main :: IO ()
main = runSolver MyMarker $ do
    addClauses [[Pos "a"]]
    _ <- mSolve
    lift $ putStrLn "\nBasic API works"
