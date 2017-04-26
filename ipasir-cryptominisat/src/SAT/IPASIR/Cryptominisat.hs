module SAT.IPASIR.Cryptominisat
    ( CryptominisatSolver ) where

import SAT.IPASIR
import SAT.IPASIR.Cryptominisat.C

type Cryptominisat v = CIpasir CryptominisatSolver v
