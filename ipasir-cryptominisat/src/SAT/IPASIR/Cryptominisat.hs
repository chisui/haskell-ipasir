{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module SAT.IPASIR.Cryptominisat
    ( CryptominisatSolver
    , Cryptominisat ) where

import Data.Functor

import Control.Comonad

import SAT.IPASIR
import SAT.IPASIR.CSolver
import SAT.IPASIR.Cryptominisat.C

type Cryptominisat v = CIpasir CryptominisatSolver LitCache v

instance (Ord l) => Clauses (CIpasir CryptominisatSolver LitCache) (Formula l) where
    type ClausesLabel (Formula l) = l
    addClauses (CIpasir cSolver litCache) f = do
        ipasirAddClauses    ors  cSolver
        cryptoAddXorClauses xors cSolver
        return (CIpasir cSolver litCache'')
        where
            (rawOrs,     rawXors)  = formulaToNormalform f
            (litCache',  ors)      = clausesToIntClauses litCache  rawOrs
            (litCache'', xorsLits) = clausesToIntClauses litCache' wrappedXors
            xors = zipWith clauseToEXOrClause rawXors xorsLits
            clauseToEXOrClause raw lits = raw $> map extract lits
            wrappedXors = map return . extract <$> rawXors
