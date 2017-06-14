module SAT.PseudoBoolean.C.Types.WeightedLit where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

data WeightedLit = WeightedLit 
    { literal :: CInt
    , weight :: CLong
    } deriving (Show, Eq, Ord)

($-$) :: (Enum l, Integral w) => l -> w -> WeightedLit
($-$) = weightedLit

weightedLit :: (Enum l, Integral w) => l -> w -> WeightedLit
weightedLit l w = WeightedLit (toEnum $ fromEnum l) (fromInteger $ toInteger w)
instance Storable WeightedLit where
    sizeOf _ = sizeOf (undefined :: Ptr WeightedLit)
    alignment = sizeOf
    peek ptr = undefined
    poke ptr wl = do

        wlPtr <- new_WeightedLit (literal wl) (weight wl)
        poke (castPtr ptr) wlPtr

foreign import ccall unsafe "new_WeightedLit" new_WeightedLit :: CInt -> CLong -> IO (Ptr WeightedLit)
