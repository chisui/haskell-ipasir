#include <cryptominisat5/cryptominisat_c.h>
#include <cryptominisat5/cryptominisat_ipasir.h>

void crypto_add_xor_clause(void* solver, void* vars, size_t num_vars, bool rhs);
