{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module SAT.IPASIR.Minicard
{-    ( Minicard
    , minicard
    ) -} where

import Data.Proxy
import Data.Functor

import Control.Comonad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import SAT.IPASIR
import SAT.IPASIR.Api
import SAT.IPASIR.Minicard.C

minicard :: Proxy (IpasirSolver Minicard s)
minicard = Proxy

instance Ord v => Clauses (IpasirSolver Minicard) (Formula v) where
    addClauses f = do
        solvers <- get
        newSolver <- lift $ mapM (addClauses' f) solvers
        put newSolver
        return ()
        where
            addClauses' :: Formula v -> IpasirSolver Minicard v -> IO (IpasirSolver Minicard v)
            addClauses' f (IpasirSolver cSolver vc) = do
                ipasirAddClauses    cSolver intCNF
                return (IpasirSolver cSolver vc')
                where
                    (vc', cnf) = formulaToCNF vc f
                    intCNF  = clausesToInt vc' cnf

