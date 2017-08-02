#include "cryptominisat_bindings.h"
#include <stdio.h>

#ifndef max
    #define max(a,b) ((a) > (b) ? (a) : (b))
#endif

/*
  Every vector consists of 3 pointer. 
*/
const int vector_size=3*sizeof( int32_t* );
const int offset =2*vector_size;

void crypto_add_xor_clause(void* rawSolver, void* vars, size_t num_vars, bool rhs) {

    SATSolver* satSolver = * ((SATSolver**) (rawSolver+offset));
    int i;
    int maxLit = 0;
    unsigned* vec = (unsigned*) vars;
    for (i=0; i<num_vars; i++) {
        int lit = vec[i];
        maxLit = max(maxLit, lit);
    }
    int currMax = cmsat_nvars(satSolver);

    if (maxLit >= currMax) { // >= to remove out of 1 error. The Vector vars starts at 0 (so maxLit is increased by 1)
        int newVars = 1 + (maxLit - currMax);
        cmsat_new_vars(satSolver, newVars);
    }

    cmsat_add_xor_clause(satSolver, vars, num_vars, rhs);
}

