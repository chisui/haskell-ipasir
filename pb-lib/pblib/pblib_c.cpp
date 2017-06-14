#include <iterator>
#include <vector>
#include "pblib_c.hpp"

using namespace std;

PBLib::WeightedLit* new_WeightedLit(int32_t lit, int64_t weight) {
    return new PBLib::WeightedLit(lit, weight);
}

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
    int32_t first_free_variable
) {
    PBConfig config = make_shared<PBConfigClass>();

    config->pb_encoder = pb_encoder;
    config->amk_encoder = amk_encoder;
    config->amo_encoder = amo_encoder;
    config->bimander_m_is = bimander_m_is;
    config->bimander_m = bimander_m;
    config->k_product_minimum_lit_count_for_splitting = k_product_minimum_lit_count_for_splitting;
    config->k_product_k = k_product_k;
    config->commander_encoding_k = commander_encoding_k;
    config->MAX_CLAUSES_PER_CONSTRAINT = MAX_CLAUSES_PER_CONSTRAINT;
    config->use_formula_cache = use_formula_cache;
    config->print_used_encodings = print_used_encodings;
    config->check_for_dup_literals = check_for_dup_literals;
    config->use_gac_binary_merge = use_gac_binary_merge;
    config->binary_merge_no_support_for_single_bits = binary_merge_no_support_for_single_bits;
    config->use_recursive_bdd_test = use_recursive_bdd_test;
    config->use_real_robdds = use_real_robdds;
    config->use_watch_dog_encoding_in_binary_merger = use_watch_dog_encoding_in_binary_merger;
    config->just_approximate = just_approximate;
    config->approximate_max_value = approximate_max_value;

    VectorClauseDatabase* formula = new VectorClauseDatabase(config);

    PB2CNF* pb2cnf = new PB2CNF(config);
    AuxVarManager* auxvars = new AuxVarManager(first_free_variable);
    vector<PBLib::WeightedLit>* literals_v = new vector<PBLib::WeightedLit>();
    for(int i = 0; i < numLiterals; i++) {
        PBLib::WeightedLit* lit = literals[i];
        literals_v->push_back(*lit);
    }

    IncPBConstraint* constraint = new IncPBConstraint(*literals_v, comp, upperBound, lowerBound);

    pb2cnf->encodeIncInital(*constraint, *formula, *auxvars);

    C_Encoder* e = (C_Encoder*)malloc(sizeof(C_Encoder));
    e->constraint = constraint;
    e->clauseDb = formula;
    e->auxManager = auxvars;
    e->encoder = pb2cnf;
    return e;
};

void free_C_Encoder(C_Encoder* e) {
    delete e->constraint;
    delete e->clauseDb;
    delete e->auxManager;
    delete e->encoder;
    delete e;
};

void c_encodeNewGeq(C_Encoder* e, int64_t newGeq) {
    e->clauseDb->clearDatabase();
    e->constraint->encodeNewGeq(newGeq, *(e->clauseDb), *(e->auxManager));
}
void c_encodeNewLeq(C_Encoder* e, int64_t newLeq) {
    e->clauseDb->clearDatabase();
    e->constraint->encodeNewLeq(newLeq, *(e->clauseDb), *(e->auxManager));
}

const C_Clauses* c_getClauses(C_Encoder* e) {
    VectorClauseDatabase* formula = e->clauseDb;
    vector<vector<int32_t>> clauses_v = formula->getClauses();
    C_Clauses* clauses = (C_Clauses*)malloc(sizeof(C_Clauses));
    clauses->size = clauses_v.size();
    clauses->clauses = (C_Clause*)malloc(sizeof(C_Clause) * clauses_v.size());
    for (int i=0; i<clauses_v.size(); i++) {
        vector<int32_t> clause_v = clauses_v[i];
        C_Clause* clause = (C_Clause*)malloc(sizeof(C_Clause));
        clause->size = clause_v.size();
        clause->literals = (int32_t*)malloc(sizeof(int32_t) * clause_v.size());
        clauses->clauses[i] = *clause;
        int j=0;
        for (int32_t lit : clause_v) {
            clause->literals[j++] = lit;
        }
    }
    return clauses;
}

