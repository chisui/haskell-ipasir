{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module SAT.IPASIR.Cryptominisat
    ( CryptominisatSolver
    ) where

import Data.Functor

import Control.Comonad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import SAT.IPASIR
import SAT.IPASIR.Cryptominisat.C

instance Ord v => Clauses (MIpasirSolver CryptominisatSolver) (Formula v) where
    addClauses f = do
        solvers <- get
        newSolver <- lift $ mapM addClauses' solvers
        put newSolver
        return ()
        where
            (rawOrs, rawXors)  = formulaToNormalform f
            addClauses' (MIpasirSolver cSolver litCache) = do
                ipasirAddClauses    ors  cSolver
                cryptoAddXorClauses xors cSolver
                return (MIpasirSolver cSolver litCache'')
                where
                    (litCache',  ors)      = clausesToIntClauses litCache  rawOrs
                    (litCache'', xorsLits) = clausesToIntClauses litCache' wrappedXors
                    xors = zipWith clauseToEXOrClause rawXors xorsLits
                    clauseToEXOrClause raw lits = raw $> map extract lits
                    wrappedXors = map return . extract <$> rawXors
                    