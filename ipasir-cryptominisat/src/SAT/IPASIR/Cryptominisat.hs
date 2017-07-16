{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module SAT.IPASIR.Cryptominisat
    ( CryptoMiniSat
    , cryptoMiniSat
    ) where

import Data.Proxy
import Data.Functor

import Control.Comonad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import SAT.IPASIR
import SAT.IPASIR.Api
import SAT.IPASIR.Cryptominisat.C


cryptoMiniSat :: Proxy (IpasirSolver CryptoMiniSat s)
cryptoMiniSat = Proxy

instance Ord v => Clauses (IpasirSolver CryptoMiniSat) (Formula v) where
    addClauses f = do
        solvers <- get
        newSolver <- lift $ mapM (addClauses' f) solvers
        put newSolver
        return ()
        where
            addClauses' :: Formula v -> IpasirSolver CryptoMiniSat v -> IO (IpasirSolver CryptoMiniSat v)
            addClauses' f (IpasirSolver cSolver vc) = do
                ipasirAddClauses    intOrs  cSolver
                cryptoAddXorClauses intXors cSolver
                return (IpasirSolver cSolver vc')
                where
                    (vc', (ors, xors)) = formulaToNormalform vc f
                    intOrs  = clausesToInt vc' ors
                    intXors = clausesToInt vc' xors
                    
