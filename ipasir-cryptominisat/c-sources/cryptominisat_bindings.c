#include "cryptominisat_bindings.h"
#include <stdio.h>

#ifndef max
    #define max(a,b) ((a) > (b) ? (a) : (b))
#endif

void crypto_add_xor_clause(void* rawSolver, void* vars, size_t num_vars, bool rhs) {
    // printf("crypto_add_xor_clause [");
    SATSolver* satSolver = get_real_solver(rawSolver);

    int maxLit = 0;
    unsigned* vec = (unsigned*) vars;
    int i;
    for (i=0; i<num_vars; i++) {
        int lit = vec[i];
        // printf("%d ", lit);
        maxLit = max(maxLit, lit);
    }
    // printf("] max=%d\n", maxLit);
    int currMax = cmsat_nvars(satSolver);
    if (maxLit > currMax) {
        int newVars = 1 + (maxLit - currMax);
        // printf("add %d\n", newVars);
        cmsat_new_vars(satSolver, newVars);
    }
    
    cmsat_add_xor_clause(satSolver, vars, num_vars, rhs);
}
