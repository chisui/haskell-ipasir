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
import Data.Monoid
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import SAT.IPASIR


data MySolver (lc :: * -> *) l = MySolver deriving Show
data MyMarker = MyMarker

instance Ord v => MSolver MySolver LitCache v where
    type Marker MySolver LitCache = MyMarker
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
