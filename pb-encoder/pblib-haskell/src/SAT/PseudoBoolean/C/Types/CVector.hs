module SAT.PseudoBoolean.C.Types.CVector where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.C.Types

newtype CVector a = CVector { toList :: [a] }

instance Storable a => Storable (CVector a) where
    sizeOf _ = sizeOf (undefined :: Ptr a) + sizeOf (undefined :: CSize)
    alignment = sizeOf
    peek ptr = do
        len <- peek (castPtr ptr)
        arrayPtr <- peekByteOff (castPtr ptr) (sizeOf (undefined :: CSize))
        if  arrayPtr == nullPtr
            then return $ CVector []
            else CVector <$> peekArray len arrayPtr
    poke ptr (CVector elems) = do
        arrPtr <- mallocArray $ length elems
        pokeArray arrPtr elems
        poke (castPtr ptr) arrPtr
