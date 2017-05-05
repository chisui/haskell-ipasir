{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}

{#context lib="ipasircryptominisat5" prefix="ipasir"#}
#include "ipasir.h"
#include "cryptominisat_bindings.h"

module SAT.IPASIR.Cryptominisat.C
    ( CryptominisatSolver, cryptoAddXorClauses ) where

import Data.Word

import SAT.IPASIR
import SAT.IPASIR.CSolver

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array


newtype CryptominisatSolver = CryptominisatSolver (ForeignPtr ())

foreign import ccall unsafe "SAT/IPASIR/Cryptominisat/C.chs.h &ipasir_release"
    ipasir_release :: FinalizerPtr ()

withCS :: (Ptr () -> IO a) -> CryptominisatSolver -> IO a
withCS f (CryptominisatSolver fPtr) = withForeignPtr fPtr f

instance CSolver CryptominisatSolver where
    ipasirSignature _ = do
        ptr <- {#call unsafe signature #}
        let iPtr = castPtr ptr :: Ptr Word8
        ints <- peekArray0 0 iPtr
        return $ "cryptominisat:" ++ map ( toEnum . fromEnum) ints

    ipasirInit = do
        ptr <- {#call unsafe init #}
        foreignPtr <- newForeignPtr ipasir_release ptr
        return $ CryptominisatSolver foreignPtr

    ipasirAdd lit = withCS (`{#call unsafe add #}` (maybe 0 lit2int lit))
    ipasirAssume lit = withCS (`{#call unsafe assume #}` lit2int lit)
    ipasirSolve = (int2SolveRes . fromEnum <$>) . withCS {#call unsafe solve #}
        where
            int2SolveRes 0  = Nothing
            int2SolveRes 10 = Just True
            int2SolveRes 20 = Just False
            int2SolveRes a  = error $ "cryptominisat ipasir is behaving poorly: solve returned " ++ show a
    ipasirVal lit = (int2Lit . fromEnum <$>) . withCS (`{#call unsafe val #}` (toEnum . (+1) . fromEnum) lit)
    ipasirFailed lit = (toEnum . fromEnum <$>) . withCS (`{#call unsafe failed #}` (toEnum . fromEnum) lit)

cryptoAddXorClauses :: [[Lit Word]] -> CryptominisatSolver -> IO ()
cryptoAddXorClauses = undefined
    

lit2int (Pos a) = toEnum $   1 +  fromEnum a
lit2int (Neg a) = toEnum $ (-1) -(fromEnum a)

int2Lit lit
    | lit >  0 = Just $ Pos $ toEnum (abs lit)
    | lit <  0 = Just $ Neg $ toEnum (abs lit)
    | lit == 0 = Nothing
