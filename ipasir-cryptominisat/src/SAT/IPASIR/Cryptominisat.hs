module SAT.IPASIR.Cryptominisat
    ( CryptominisatSolver
    , Cryptominisat ) where

import SAT.IPASIR
import SAT.IPASIR.Cryptominisat.C

type Cryptominisat v = CIpasir CryptominisatSolver LitCache v
