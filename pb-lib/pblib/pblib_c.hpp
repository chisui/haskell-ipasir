#ifndef PBLIB_C_H
#define PBLIB_C_H

#include "PBConfig.h"
#include "incpbconstraint.h"
#include "weightedlit.h"
#include "clausedatabase.h"
#include "auxvarmanager.h"
#include "pb2cnf.h"

extern "C" {

struct C_Encoder {
    IncPBConstraint* constraint;
    VectorClauseDatabase* clauseDb;
    AuxVarManager* auxManager;
    PB2CNF* encoder;
};

struct C_Clause {
    size_t size;
    int32_t* literals;
};

struct C_Clauses {
    size_t size;
    C_Clause* clauses;
};

PBLib::WeightedLit* new_WeightedLit(int32_t lit, int64_t weight);

C_Encoder* new_C_Encoder(
    PB_ENCODER::PB2CNF_PB_Encoder pb_encoder,
    AMK_ENCODER::PB2CNF_AMK_Encoder amk_encoder,
    AMO_ENCODER::PB2CNF_AMO_Encoder amo_encoder,
    BIMANDER_M_IS::BIMANDER_M_IS bimander_m_is,
    int bimander_m,
    int k_product_minimum_lit_count_for_splitting,
    int k_product_k,
    int commander_encoding_k,
    int64_t MAX_CLAUSES_PER_CONSTRAINT,
    bool use_formula_cache,
    bool print_used_encodings,
    bool check_for_dup_literals,
    bool use_gac_binary_merge,
    bool binary_merge_no_support_for_single_bits,
    bool use_recursive_bdd_test,
    bool use_real_robdds,
    bool use_watch_dog_encoding_in_binary_merger,
    bool just_approximate,
    int64_t approximate_max_value,
    PBLib::WeightedLit** literals,
    size_t numLiterals,
    PBLib::Comparator comp,
    int64_t lowerBound,
    int64_t upperBound,
    int32_t first_free_variable);
void free_C_Encoder(C_Encoder* ptr);

void c_encodeNewGeq(C_Encoder* constraint, int64_t newGeq);
void c_encodeNewLeq(C_Encoder* constraint, int64_t newLeq);

const C_Clauses* c_getClauses(C_Encoder* db);

}

#endif