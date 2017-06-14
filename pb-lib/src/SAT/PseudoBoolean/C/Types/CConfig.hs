module SAT.PseudoBoolean.C.Types.CConfig where

import Data.Maybe

import Foreign.C.Types

import SAT.PseudoBoolean.Config

import SAT.PseudoBoolean.C.Types.CEnum
import SAT.PseudoBoolean.C.Types.CBool

{-# ANN module "HLint: ignore Use camelCase" #-}


coerceNum :: (Integral a, Num b) => a -> b
coerceNum = fromInteger . toInteger

class CardinalityMethod a where
    pb_encoder :: Config a -> CEnum PB_ENCODER
    pb_encoder _ = minBound
    
    amk_encoder :: Config a -> CEnum AMK_ENCODER
    amk_encoder _ = minBound
    
    amo_encoder :: Config a -> CEnum AMO_ENCODER
    amo_encoder _ = minBound
    
    bimander_m_is :: Config a -> CEnum BimanderMIs
    bimander_m_is _ = minBound
    
    bimander_m :: Config a -> CInt
    bimander_m _ = 3
    
    k_product_minimum_lit_count_for_splitting :: Config a -> CInt
    k_product_minimum_lit_count_for_splitting _ = 10
    
    k_product_k :: Config a -> CInt
    k_product_k _ = 2
    
    commander_encoding_k :: Config a -> CInt
    commander_encoding_k _ = 3
    
    max_clauses_per_constant :: Config a -> CLong
    max_clauses_per_constant _ = 1000000
    
    use_formula_cache :: Config a -> CBool
    use_formula_cache = cBool . useFormulaCache
    
    print_used_encodings :: Config a -> CBool
    print_used_encodings _ = false
    
    check_for_dup_literals :: Config a -> CBool
    check_for_dup_literals = cBool . checkForDuplicateLiterals
    
    use_gac_binary_merge :: Config a -> CBool
    use_gac_binary_merge _ = false
    
    binary_merge_no_support_for_single_bits :: Config a -> CBool
    binary_merge_no_support_for_single_bits _ = true
    
    use_recursive_bdd_test :: Config a -> CBool
    use_recursive_bdd_test _ = false
    
    use_real_robdds :: Config a -> CBool
    use_real_robdds _ = false
    
    use_watch_dog_encoding_in_binary_merger :: Config a -> CBool
    use_watch_dog_encoding_in_binary_merger _ = false


    just_approximate :: Config a -> CBool
    just_approximate = cBool . isJust . approximate

    approximate_max_value :: Config a -> CLong
    approximate_max_value = coerceNum . fromMaybe 1000 . approximate

instance CardinalityMethod AtMostOne where
    amo_encoder = CEnum . maybe AMO_BEST encoder . config
        where
            encoder AmoNested = AMO_NESTED
            encoder AmoBdd{} = AMO_BDD
            encoder AmoBimander{} = AMO_BIMANDER
            encoder AmoCommander{} = AMO_COMMANDER
            encoder AmoKProduct{} = AMO_KPRODUCT
            encoder AmoBinary = AMO_BINARY
            encoder AmoPairwise = AMO_PAIRWISE
      
    bimander_m_is Config { config = Just a@AmoBimander{} } = CEnum $ mIs a
    bimander_m_is _ = minBound

    bimander_m Config { config = Just a@AmoBimander {} } = coerceNum $ m a
    bimander_m _ = 3

    k_product_minimum_lit_count_for_splitting Config { config = Just a@AmoKProduct{} } = coerceNum $ splitThreshold a
    k_product_minimum_lit_count_for_splitting _ = 10

    k_product_k Config { config = Just a@AmoKProduct{} } = coerceNum $ chunkSize a
    k_product_k _ = 2

    commander_encoding_k Config { config = Just a@AmoCommander{} } = coerceNum $ k a
    commander_encoding_k _ = 3

    use_recursive_bdd_test Config { config = Just a@AmoBdd{} } = cBool $ amoRecursive a
    use_recursive_bdd_test _ = false
    
    use_real_robdds Config { config = Just a@AmoBdd{} } = cBool $ amoRealdRoBdds a
    use_real_robdds _ = false

instance CardinalityMethod AtMostK where
    amk_encoder = CEnum . maybe AMK_BEST encoder . config
        where
            encoder AmkCard = AMK_CARD
            encoder AmkBdd{} = AMK_BDD

    use_recursive_bdd_test Config { config = Just a@AmkBdd{} } = cBool $ amkRecursive a
    use_recursive_bdd_test _ = false
    
    use_real_robdds Config { config = Just a@AmkBdd{} } = cBool $ amkRealdRoBdds a
    use_real_robdds _ = false

instance CardinalityMethod PseudoBoolean where
    pb_encoder = CEnum . maybe PB_BEST encoder . config
        where
            encoder PbBdd = PB_BDD
            encoder PbSequentialWeightCounter = PB_SWC
            encoder PbSortingNetworks = PB_SORTINGNETWORKS
            encoder PbAdder = PB_ADDER
            encoder PbBinaryMerge{} = PB_BINARY_MERGE
          
    use_gac_binary_merge Config { config = Just a@PbBinaryMerge{} } = cBool $ useGac a
    use_gac_binary_merge _ = false

    use_watch_dog_encoding_in_binary_merger Config { config = Just a@PbBinaryMerge{} } = cBool $ noSupportForSingeBits a
    use_watch_dog_encoding_in_binary_merger _ = false
