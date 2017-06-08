{-# LANGUAGE ForeignFunctionInterface #-}
module SAT.PseudoBoolean.C.Bindings where

import SAT.PseudoBoolean.C.Types
import SAT.PseudoBoolean.Config

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr

{-# ANN module "HLint: ignore Use camelCase" #-}

data C_Encoder = C_Encoder

foreign import ccall unsafe "new_C_Encoder" new_C_Encoder ::
    CInt -> -- pb_encoder
    CInt -> -- amk_encoder
    CInt -> -- amo_encoder
    CInt -> -- bimander_m_is
    CInt -> -- bimander_m
    CInt -> -- k_product_minimum_lit_count_for_splitting
    CInt -> -- k_product_k
    CInt -> -- commander_encoding_k
    CLong -> -- MAX_CLAUSES_PER_CONSTRAINT
    CBool -> -- use_formula_cache
    CBool -> -- print_used_encodings
    CBool -> -- check_for_dup_literals
    CBool -> -- use_gac_binary_merge
    CBool -> -- binary_merge_no_support_for_single_bits
    CBool -> -- use_recursive_bdd_test
    CBool -> -- use_real_robdds
    CBool -> -- use_watch_dog_encoding_in_binary_merger
    CBool -> -- just_approximate
    CLong -> -- approximate_max_value
    Ptr WeightedLit -> -- literals
    CSize -> -- numLiterals
    CInt -> -- comp 
    CLong -> -- lowerBound
    CLong -> -- upperBound
    CInt -> --first_free_variable
    IO (Ptr C_Encoder)

foreign import ccall unsafe "&free_C_Encoder" free_C_Encoder :: FinalizerPtr C_Encoder

foreign import ccall unsafe "c_encodeNewGeq" c_encodeNewGeq :: Ptr C_Encoder -> CLong -> IO ()
foreign import ccall unsafe "c_encodeNewLeq" c_encodeNewLeq :: Ptr C_Encoder -> CLong -> IO ()

foreign import ccall unsafe "c_getClauses" c_getClauses :: Ptr C_Encoder -> IO (Ptr (CVector (CVector CInt)))
