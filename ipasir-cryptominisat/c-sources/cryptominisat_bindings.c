#include "cryptominisat_bindings.h"
#include <stdio.h>

#ifndef max
    #define max(a,b) ((a) > (b) ? (a) : (b))
#endif

void crypto_add_xor_clause(void* rawSolver, void* vars, size_t num_vars, bool rhs) {
    printf("crypto_add_xor_clause [");
    SATSolver* satSolver = (rawSolver + 64);

    unsigned maxLit = 0;
    unsigned* vec = (unsigned*) vars;
    for (int i=0; i<num_vars; i++) {
        unsigned lit = vec[i];
        printf("%d ", lit);
        maxLit = max(maxLit, lit);
    }
    printf("]\n", maxLit);

    unsigned currMax = cmsat_nvars(satSolver) - 1;
    if (maxLit > currMax) {
        cmsat_new_vars(satSolver, maxLit - currMax);
    }
    
    cmsat_add_xor_clause(satSolver, vars, num_vars, rhs);
}
