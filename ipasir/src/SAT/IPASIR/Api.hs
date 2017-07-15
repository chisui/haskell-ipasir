module SAT.IPASIR.Api where

import Control.Monad

import SAT.IPASIR.Literals

{- |Represents all solver, which implemented the IPASIR header file.
    The following code blocks are the definitions in the header of
    the corresponding functions in the c-header file. 
-}
class Ipasir a where
    {- |Returns the same string as the corresponding c-function.
        
        >  Return the name and the version of the incremental SAT
        >  solving library. 
        >  
        >const char * ipasir_signature ();
    -}
    ipasirSignature :: a -> IO String

    {- |Initialises a new solver. 
               
        >  Construct a new solver and return a pointer to it.
        >  Use the returned pointer as the first parameter in each
        >  of the following functions.
        >  
        >  Required state: N/A
        >  State after: INPUT
        >  
        >void * ipasir_init ();
    -}
    ipasirInit   :: IO a

    {- |Adds a literal to the clause, which is currently under construction.
        @Just (Neg (-5))@ would add -5, which stands for \\( \\lnot x_5 \\). 
        @Nothing@ ends the clause, adds it to the solver and starts
        the construction of a new one. 
                
        >  Add the given literal into the currently added clause
        >  or finalize the clause with a 0.  Clauses added this way
        >  cannot be removed. The addition of removable clauses
        >  can be simulated using activation literals and assumptions.
        >  
        >  Required state: INPUT or SAT or UNSAT
        >  State after: INPUT
        >  
        >  Literals are encoded as (non-zero) integers as in the
        >  DIMACS formats.  They have to be smaller or equal to
        >  INT_MAX and strictly larger than INT_MIN (to avoid
        >  negation overflow).  This applies to all the literal
        >  arguments in API functions.
        >
        >void ipasir_add (void * solver, int lit_or_zero);
    -}
    ipasirAdd    :: Maybe (Lit Word) -> a -> IO ()

    {- |Adds an assumption to the solver. The assumption is used only for the
        next solving process. Note that assumptions are connected by \\( \\wedge \\)
        and not by \\( \\vee \\). 
        
        >  Add an assumption for the next SAT search (the next call
        >  of ipasir_solve). After calling ipasir_solve all the
        >  previously added assumptions are cleared.
        >
        >  Required state: INPUT or SAT or UNSAT
        >
        >  State after: INPUT
        >
        >void ipasir_assume (void * solver, int lit);
    -}
    ipasirAssume :: Lit Word -> a -> IO ()

    {- |Tells the solver to start solving. The returning value stands for

        * @Just True@  - There is a model. You can read the model by using 'ipasirVal'.
        * @Just False@ - Unsatisfiable. You can read the minimal conflict by using 'ipasirFailed'.
        * @Nothing@    - Interrupted. This case shouldn't be possible with this haskell package, since we didn't
          implement the @terminate@-function.

        >  Solve the formula with specified clauses under the specified assumptions.
        >  If the formula is satisfiable the function returns 10 and the state of the solver is changed to SAT.
        >  If the formula is unsatisfiable the function returns 20 and the state of the solver is changed to UNSAT.
        >  If the search is interrupted (see ipasir_set_terminate) the function returns 0 and the state of the solver remains INPUT.
        >  This function can be called in any defined state of the solver.
        >  
        >  Required state: INPUT or SAT or UNSAT
        >  State after: INPUT or SAT or UNSAT
        >
        >int ipasir_solve (void * solver);
    -}
    ipasirSolve  :: a -> IO (Maybe Bool)

    {- | Can only be used if 'ipasirSolve' was aleady used and @Just True@ was the return value.
         This function returns the value of the given variable in the model.
        
        * @Just (Pos _)@ - The variable is @True@
        * @Just (Neg _)@ - The variable is @False@
        * @Nothing     @ - The truth of the variable is not important for the solution.

        Warning: The header file @ipasir.h@ sais in the comment, that you get @-lit@, @0@ or @lit@.
        Some solver didn't implement it that way. They give @-1@, @0@ or @1@. We recommend you not
        using returning value of the @Word@.

        >  Get the truth value of the given literal in the found satisfying
        >  assignment. Return 'lit' if True, '-lit' if False, and 0 if not important.
        >  This function can only be used if ipasir_solve has returned 10
        >  and no 'ipasir_add' nor 'ipasir_assume' has been called
        >  since then, i.e., the state of the solver is SAT.
        >  
        >  Required state: SAT
        >  State after: SAT
        >
        >int ipasir_val (void * solver, int lit);
    -}
    ipasirVal    :: Word -> a -> IO (Maybe (Lit Word))

    {- |Can only be used if 'ipasirSolve' was aleady used and @Just False@ was the return value.
        Used to read a conflict. 

        All variables which maps onto @True@ are part of the conflict. Otherwise they aren't. 

        >  Set a callback function used to indicate a termination requirement to the
        >  solver. The solver will periodically call this function and check its return
        >  value during the search. The ipasir_set_terminate function can be called in any
        >  state of the solver, the state remains unchanged after the call.
        >  The callback function is of the form "int terminate(void * state)"
        >      - it returns a non-zero value if the solver should terminate.
        >      - the solver calls the callback function with the parameter "state"
        >        having the value passed in the ipasir_set_terminate function (2nd parameter).
        >  
        >  Required state: INPUT or SAT or UNSAT
        >  State after: INPUT or SAT or UNSAT
        >
        >int ipasir_failed (void * solver, int lit);
    -}
    ipasirFailed :: Word -> a -> IO Bool
    
    -- | Adds a complete clause. The standard definition does that by using 'ipasirAdd' multiple times.
    ipasirAddClause :: [Lit Word] -> a -> IO ()
    ipasirAddClause [] s = do
        --traceM " 0"
        ipasirAdd Nothing s
    ipasirAddClause (l:ls) s = do
        --traceM (show l)
        ipasirAdd (Just l) s
        ipasirAddClause ls s
        
    -- | Adds a list of complete clauses. The standard definition does that by using 'ipasirAddClause' multiple times.
    ipasirAddClauses :: [[Lit Word]] -> a -> IO ()
    ipasirAddClauses [] s = return ()
    ipasirAddClauses (l:ls) s = do
        ipasirAddClause  l s
        ipasirAddClauses ls s

    --ipasir_set_terminate :: a ->  (void * solver, void * state, int (*terminate)(void * state));
