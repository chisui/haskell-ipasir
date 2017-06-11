#include "cryptominisat_bindings.h"
#include <stdio.h>

#ifndef max
    #define max(a,b) ((a) > (b) ? (a) : (b))
#endif

void crypto_add_xor_clause(void* rawSolver, void* vars, size_t num_vars, bool rhs) {
    printf("crypto_add_xor_clause [");
    SATSolver* satSolver = get_real_solver(rawSolver);

    unsigned maxLit = 0;
    unsigned* vec = (unsigned*) vars;
    for (int i=0; i<num_vars; i++) {
        unsigned lit = vec[i];
        printf("%d ", lit);
        maxLit = max(maxLit, lit);
    }
    printf("] max=%d\n", maxLit);
    unsigned currMax = cmsat_nvars(satSolver);
    if (maxLit > currMax) {
        unsigned newVars = 1 + (maxLit - currMax);
        printf("add %d\n", newVars);
        cmsat_new_vars(satSolver, newVars);
    }
    
    cmsat_add_xor_clause(satSolver, vars, num_vars, rhs);
}
