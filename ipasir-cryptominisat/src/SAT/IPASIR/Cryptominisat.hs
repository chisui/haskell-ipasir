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

import Data.Functor

import Control.Comonad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import SAT.IPASIR
import SAT.IPASIR.Cryptominisat.C


cryptoMiniSat :: IpasirSolver CryptoMiniSat
cryptoMiniSat = undefined

instance Ord v => Clauses (MIpasirSolver CryptoMiniSat) (Formula v) where
    addClauses f = do
        solvers <- get
        newSolver <- lift $ mapM (addClauses' f) solvers
        put newSolver
        return ()
        where
            addClauses' :: Formula v -> MIpasirSolver CryptoMiniSat v -> IO (MIpasirSolver CryptoMiniSat v)
            addClauses' f (MIpasirSolver cSolver vc) = do
                ipasirAddClauses    intOrs  cSolver
                cryptoAddXorClauses intXors cSolver
                return (MIpasirSolver cSolver vc')
                where
                    (vc', (ors, xors)) = formulaToNormalform vc f
                    intOrs  = clausesToInt vc' ors
                    intXors = clausesToInt vc' xors
                    