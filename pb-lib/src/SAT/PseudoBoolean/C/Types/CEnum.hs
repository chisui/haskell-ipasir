{-# LANGUAGE DeriveGeneric #-}
module SAT.PseudoBoolean.C.Types.CEnum where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

import GHC.Generics

{-# ANN module "HLint: ignore Use camelCase" #-}

data AMO_ENCODER
    = AMO_BEST
    | AMO_NESTED
    | AMO_BDD
    | AMO_BIMANDER
    | AMO_COMMANDER
    | AMO_KPRODUCT
    | AMO_BINARY
    | AMO_PAIRWISE
        deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

data AMK_ENCODER
    = AMK_BEST
    | AMK_BDD
    | AMK_CARD
        deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

data PB_ENCODER
    = PB_BEST
    | PB_BDD
    | PB_SWC
    | PB_SORTINGNETWORKS
    | PB_ADDER
    | PB_BINARY_MERGE
        deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)


newtype CEnum a = CEnum a deriving (Eq, Ord, Bounded, Show, Read, Generic)
instance Enum a => Enum (CEnum a) where
    toEnum = CEnum . toEnum
    fromEnum (CEnum e) = fromEnum e
instance Enum a => Storable (CEnum a) where
    sizeOf _ = sizeOf (undefined :: CInt)
    alignment = sizeOf
    peek = (toEnum <$>) . peek . castPtr
    poke ptr = poke (castPtr ptr) . fromEnum
