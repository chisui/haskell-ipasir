module SAT.PseudoBoolean.C
    ( encoder
    , encodeNewGeq
    , encodeNewLeq
    , getClauses
    , Comp(..)
    , module Export
    , C_Encoder
    ) where

import Data.Maybe
import Data.Word
import Data.Int
import Data.Bits

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import GHC.Generics

import SAT.IPASIR
import SAT.PseudoBoolean.C.Types as Export
import SAT.PseudoBoolean.C.Bindings
import SAT.PseudoBoolean.Config


data Comp
    = CLeq
    | CGeq
    | CBoth
        deriving (Show, Eq, Enum, Ord)

coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum

encoder :: CardinalityMethod a => Config a -> [WeightedLit] -> Comp -> Int64 -> Int64 -> Int -> IO (ForeignPtr C_Encoder)
encoder config lits comp lowerBound upperBound firstFreeLit = do
    ptr <- mallocArray (length lits) :: IO (Ptr WeightedLit)
    pokeArray (castPtr ptr) lits
    let size   = coerceEnum $ length lits
    let ccomp  = coerceEnum comp
    let clower = coerceNum lowerBound
    let cupper = coerceNum upperBound
    let cfirstFree = coerceNum firstFreeLit
    rawEncoder <- toEncoder config ptr size ccomp clower cupper cfirstFree
    newForeignPtr free_C_Encoder rawEncoder


toEncoder :: CardinalityMethod a => Config a -> Ptr WeightedLit -> CSize -> CInt -> CLong -> CLong -> CInt -> IO (Ptr C_Encoder)
toEncoder = new_C_Encoder <$> (coerceEnum <$> pb_encoder)
                          <*> (coerceEnum <$> amk_encoder)
                          <*> (coerceEnum <$> amo_encoder)
                          <*> (coerceEnum <$> bimander_m_is)
                          <*> bimander_m
                          <*> k_product_minimum_lit_count_for_splitting
                          <*> k_product_k
                          <*> commander_encoding_k
                          <*> max_clauses_per_constant
                          <*> use_formula_cache
                          <*> print_used_encodings
                          <*> check_for_dup_literals
                          <*> use_gac_binary_merge
                          <*> binary_merge_no_support_for_single_bits
                          <*> use_recursive_bdd_test
                          <*> use_real_robdds
                          <*> use_watch_dog_encoding_in_binary_merger
                          <*> just_approximate
                          <*> approximate_max_value


encodeNewGeq :: ForeignPtr C_Encoder -> Int64 -> IO ()
encodeNewGeq encoderPtr bound = withForeignPtr encoderPtr doGeq
    where
        doGeq ptr = c_encodeNewGeq ptr $ coerceNum bound
encodeNewLeq :: ForeignPtr C_Encoder -> Int64 -> IO ()
encodeNewLeq encoderPtr bound = withForeignPtr encoderPtr doLeq
    where
        doLeq ptr = c_encodeNewLeq ptr $ coerceNum bound

getClauses :: ForeignPtr C_Encoder -> IO [[Lit Word]]
getClauses encoder = do
    clausesPtr <- withForeignPtr encoder c_getClauses
    rawClauses <- peek clausesPtr
    return $ map (map readLit . toList) $ toList rawClauses

readLit i = lit (i > 0) $ coerceEnum $ abs i
