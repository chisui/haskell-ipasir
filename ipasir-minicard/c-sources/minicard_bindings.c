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
//        printf("%d ", lit);
        maxLit = max(maxLit, lit);
    }
    int currMax = cmsat_nvars(satSolver);
//    printf("] max=%d,    where currMax=%d\n", maxLit,currMax);

    if (maxLit >= currMax) { // >= to remove out of 1 error. The Vector vars starts at 0 (so maxLit is increased by 1)
        int newVars = 1 + (maxLit - currMax);

    //    printf("add %d,   Max before: %d,    " , newVars, currMax);
        cmsat_new_vars(satSolver, newVars);
    //    printf("Max after: %d\n" , cmsat_nvars(satSolver));
    }

    cmsat_add_xor_clause(satSolver, vars, num_vars, rhs);
}

