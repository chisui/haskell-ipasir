{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}

{#context lib="ipasircryptominisat5" #}
#include "cryptominisat_bindings.h"
#include "ipasir.h"

module SAT.IPASIR.Cryptominisat.C
    ( CryptoMiniSat
    , cryptoAddXorClauses
    , cryptoAddXorClause
    ) where

import Data.Word
import Data.Bits (toIntegralSized)
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as Vec

import Control.Comonad

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import SAT.IPASIR
import SAT.IPASIR.Api


newtype CryptoMiniSat = CryptoMiniSat (ForeignPtr ()) deriving (Eq,Ord)

foreign import ccall unsafe "SAT/IPASIR/Cryptominisat/C.chs.h &ipasir_release"
    ipasir_release :: FinalizerPtr ()

withCS :: CryptoMiniSat -> (Ptr () -> IO a) -> IO a
withCS (CryptoMiniSat fPtr) f = withForeignPtr fPtr f

instance Ipasir CryptoMiniSat where
    ipasirGetID (CryptoMiniSat ptr) = fromJust $ toIntegralSized $ toInteger $ ptrToWordPtr $ unsafeForeignPtrToPtr ptr

    ipasirSignature _ = do
        ptr <- {#call unsafe ipasir_signature #}
        let iPtr = castPtr ptr :: Ptr Word8
        ints <- peekArray0 0 iPtr
        return $ "cryptominisat:" ++ map ( toEnum . fromEnum) ints

    ipasirInit' = do
        ptr <- {#call unsafe ipasir_init #}
        foreignPtr <- newForeignPtr ipasir_release ptr
        return $ CryptoMiniSat foreignPtr

    ipasirAdd' s lit = withCS s (`{#call unsafe ipasir_add #}` (toEnum lit))
    ipasirAssume' s lit = withCS s (`{#call unsafe ipasir_assume #}` (toEnum lit))
    ipasirSolve' s = int2SolveRes . fromEnum <$> withCS s {#call unsafe ipasir_solve #}
        where
            int2SolveRes 0  = LUndef
            int2SolveRes 10 = LTrue
            int2SolveRes 20 = LFalse
            int2SolveRes a  = error $ "cryptominisat ipasir is behaving poorly: solve returned " ++ show a
    ipasirVal' s lit    = fromEnum <$> withCS s (`{#call unsafe ipasir_val #}` (toEnum . fromEnum) lit)
    ipasirFailed' s lit = (>0)     <$> withCS s (`{#call unsafe ipasir_failed #}` (toEnum . fromEnum) lit)

cryptoAddXorClauses :: CryptoMiniSat -> [Lit [Word]] -> IO ()
cryptoAddXorClauses _ [] = return ()
cryptoAddXorClauses s (l:ls) = do
    cryptoAddXorClause  s l
    cryptoAddXorClauses s ls

cryptoAddXorClause :: CryptoMiniSat -> Lit [Word] -> IO ()
cryptoAddXorClause s clause = withCS s addXorInternal
    where
        addXorInternal solverPtr = do
            let litClause = map (toEnum . (\l -> l - 1) . fromEnum) (extract clause) :: [CInt]
            let vector = Vec.fromList litClause
            Vec.unsafeWith vector $ \ vecPtr -> do
                let size = toEnum $ fromEnum $ length $ extract clause
                {#call unsafe crypto_add_xor_clause #} solverPtr (castPtr vecPtr) size sign
        sign = toEnum $ fromEnum $ isPositive clause

