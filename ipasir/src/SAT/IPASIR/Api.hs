{-# LANGUAGE FlexibleInstances, TypeFamilies, UndecidableInstances #-}

module SAT.IPASIR.Api
    {-( Ipasir(..)
    ) -}where


import Control.Monad
import Control.Comonad
import Data.Vector hiding ((++))
import Data.IORef
import System.Random

import SAT.IPASIR.EnvironmentVariable
import SAT.IPASIR.Literals

env :: Ipasir i => Env i Word
env = newEnv

newtype TestSolver = TestSolver Int deriving (Eq,Ord)

instance Ipasir (TestSolver) where
    ipasirSignature (TestSolver i) = return $ "TestSolver " ++ show i
    ipasirInit     = TestSolver <$> System.Random.randomIO
  --  ipasirAdd  =
    ipasirSolve _    = return Nothing
    ipasirVal _ _    = return Nothing
    ipasirFailed _ _ = return False
    ipasirAssume _ _ = undefined
    ipasirAddClause  = undefined
     


{-|
    Class that models the <https://github.com/biotomas/ipasir/blob/master/ipasir.h ipasir.h> interface.
    This class is meant to be implemented using foreign function interfaces to the actual C solver.
    In most cases the type @a@ will be a @newtype@ around a 'ForeignPtr'.
-}
class Ord a => Ipasir a where

    {-|
     Return the name and the version of the incremental @SAT@
     solving library.
    -}
    ipasirSignature :: a -> IO String

    {-|
     Construct a new solver and return a pointer to it.
     Use the returned pointer as the first parameter in each
     of the following functions.

     Required state: @N/A@
     State after: @INPUT@

     This function also has to take care of deleding the solver when it gets carbage collected.
    -}
    ipasirInit :: IO a

    {-|
     Add the given literal into the currently added clause
     or finalize the clause with a 'Nothing'.  Clauses added this way
     cannot be removed. The addition of removable clauses
     can be simulated using activation literals and assumptions.

     Required state: @INPUT@ or @SAT@ or @UNSAT@
     State after: @INPUT@

     Literals are encoded as (non-zero) integers as in the
     DIMACS formats.  They have to be smaller or equal to
     INT_MAX and strictly larger than INT_MIN (to avoid
     negation overflow).  This applies to all the literal
     arguments in API functions.
    -}
    ipasirAdd :: a -> Maybe (Lit Word) -> IO ()
    ipasirAdd ptr Nothing = return ()
    ipasirAdd ptr (Just x) = do
        modifyVar env ptr (max (extract x))

    {-|
     Add an assumption for the next @SAT@ search (the next call
     of 'ipasirSolve'). After calling 'ipasirSolve' all the
     previously added assumptions are cleared.

     Required state: @INPUT@ or @SAT@ or @UNSAT@
     State after: @INPUT@
    -}
    ipasirAssume :: a -> Lit Word -> IO ()

    {-|
     Solve the formula with specified clauses under the specified assumptions.
     If the formula is satisfiable the function returns @Just True@ and the state of the solver is changed to @SAT@.
     If the formula is unsatisfiable the function returns @Just False@ and the state of the solver is changed to @UNSAT@.
     @ipasir_set_terminate@ is not supported.
     This function can be called in any defined state of the solver.

     Required state: @INPUT@ or @SAT@ or @UNSAT@
     State after: @INPUT@ or @SAT@ or @UNSAT@
    -}
    ipasirSolve :: a -> IO (Maybe Bool)

    {-|
     Get the truth value of the given literal in the found satisfying
     assignment. Return @Pos a@ if True, @Neg a@ if False, and @Nothing@ if not important.
     This function can only be used if ipasirSolve has returned @Just True@
     and no 'ipasirAdd' nor 'ipasirAssume' has been called
     since then, i.e., the state of the solver is @SAT@.

     Required state: @SAT@
     State after: @SAT@
    -}
    ipasirVal :: a -> Word -> IO (Maybe (Lit Word))
    
    {-|
     Check if the given assumption literal was used to prove the
     unsatisfiability of the formula under the assumptions
     used for the last @SAT@ search. Return @True@ if so, @False@ otherwise.
     This function can only be used if 'ipasirSolve' has returned @Just False@ and
     no 'ipasirAdd' or 'ipasirAssume' has been called since then, i.e.,
     the state of the solver is @UNSAT@.

     Required state: @UNSAT@
     State after: @UNSAT@
    -}
    ipasirFailed :: a -> Word -> IO Bool

    {-|
     Add the given clause.  Clauses added this way
     cannot be removed. The addition of removable clauses
     can be simulated using activation literals and assumptions.

     Required state: @INPUT@ or @SAT@ or @UNSAT@
     State after: @INPUT@

     The default implementation adds each literal of the clause by calling 'ipasirAdd' and finally adding a 'Nothing'.
    -}
    ipasirAddClause :: a -> [Lit Word] -> IO ()
    ipasirAddClause s [] = ipasirAdd s Nothing
    ipasirAddClause s (l:ls) = do
        ipasirAdd s (Just l)
        ipasirAddClause s ls
    
    {-|
     Add the given clauses.  Clauses added this way
     cannot be removed. The addition of removable clauses
     can be simulated using activation literals and assumptions.

     Required state: @INPUT@ or @SAT@ or @UNSAT@
     State after: @INPUT@

     The default implementation adds each clause by calling 'ipasirAddClause'.
    -}
    ipasirAddClauses :: a -> [[Lit Word]] -> IO ()
    ipasirAddClauses _ [] = return ()
    ipasirAddClauses s (l:ls) = do
        ipasirAddClause  s l
        ipasirAddClauses s ls

    --ipasir_set_terminate :: a ->  (void * solver, void * state, int (*terminate)(void * state));

