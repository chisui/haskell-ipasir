{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}

{#context lib="ipasircryptominisat5" #}
#include "cryptominisat_bindings.h"
#include "ipasir.h"

module SAT.IPASIR.Cryptominisat.C
    ( CryptominisatSolver
    , cryptoAddXorClauses
    , cryptoAddXorClause
    ) where

import Data.Word
import qualified Data.Vector.Storable as Vec

import Control.Comonad

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.C.Types

import SAT.IPASIR
import SAT.IPASIR.CSolver


newtype CryptominisatSolver = CryptominisatSolver (ForeignPtr ())

foreign import ccall unsafe "SAT/IPASIR/Cryptominisat/C.chs.h &ipasir_release"
    ipasir_release :: FinalizerPtr ()

withCS :: (Ptr () -> IO a) -> CryptominisatSolver -> IO a
withCS f (CryptominisatSolver fPtr) = withForeignPtr fPtr f

instance CSolver CryptominisatSolver where
    ipasirSignature _ = do
        ptr <- {#call unsafe ipasir_signature #}
        let iPtr = castPtr ptr :: Ptr Word8
        ints <- peekArray0 0 iPtr
        return $ "cryptominisat:" ++ map ( toEnum . fromEnum) ints

    ipasirInit = do
        ptr <- {#call unsafe ipasir_init #}
        foreignPtr <- newForeignPtr ipasir_release ptr
        return $ CryptominisatSolver foreignPtr

    ipasirAdd lit = withCS (`{#call unsafe ipasir_add #}` (maybe 0 lit2int lit))
    ipasirAssume lit = withCS (`{#call unsafe ipasir_assume #}` lit2int lit)
    ipasirSolve = (int2SolveRes . fromEnum <$>) . withCS {#call unsafe ipasir_solve #}
        where
            int2SolveRes 0  = Nothing
            int2SolveRes 10 = Just True
            int2SolveRes 20 = Just False
            int2SolveRes a  = error $ "cryptominisat ipasir is behaving poorly: solve returned " ++ show a
    ipasirVal lit = (int2Lit . fromEnum <$>) . withCS (`{#call unsafe ipasir_val #}` (toEnum . fromEnum) lit)
    ipasirFailed lit = (toEnum . fromEnum <$>) . withCS (`{#call unsafe ipasir_failed #}` (toEnum . fromEnum) lit)

cryptoAddXorClauses :: [Lit [Word]] -> CryptominisatSolver -> IO ()
cryptoAddXorClauses [] s = return ()
cryptoAddXorClauses (l:ls) s = do
    cryptoAddXorClause  l s
    cryptoAddXorClauses ls s

cryptoAddXorClause :: Lit [Word] -> CryptominisatSolver -> IO ()
cryptoAddXorClause clause = withCS addXorInternal
    where
        addXorInternal solverPtr = do
            let litClause = map (toEnum . (\l -> l - 1) . fromEnum) (extract clause) :: [CInt]
            let vector = Vec.fromList litClause
            Vec.unsafeWith vector $ \ vecPtr -> do
                let size = toEnum $ fromEnum $ length $ extract clause
                {#call unsafe crypto_add_xor_clause #} solverPtr (castPtr vecPtr) size s
        s = toEnum $ fromEnum $ sign clause

lit2int (Pos a) = toEnum $   fromEnum a
lit2int (Neg a) = toEnum $ -(fromEnum a)

int2Lit lit
    | lit >  0 = Just $ Pos $ toEnum (abs lit)
    | lit <  0 = Just $ Neg $ toEnum (abs lit)
    | lit == 0 = Nothing
