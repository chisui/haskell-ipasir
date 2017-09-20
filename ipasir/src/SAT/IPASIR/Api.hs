{-# LANGUAGE FlexibleInstances, TypeFamilies, UndecidableInstances #-}

module SAT.IPASIR.Api
    {-( Ipasir(..)
    ) -}where


import Control.Monad
import Control.Comonad
import qualified Data.Vector as Vec
import Data.IORef
import Data.Word
import qualified Data.Set as Set
import System.Random
import System.IO.Unsafe

import SAT.IPASIR.EnvironmentVariable
import SAT.IPASIR.Literals

data SolverState = INPUT | SAT | UNSAT
    deriving (Show,Eq)

maxVar :: Env Word Word
maxVar = unsafePerformIO newEnv

clauseCreator :: Env Word [Int]
clauseCreator = unsafePerformIO newEnv

solverState :: Env Word SolverState
solverState = unsafePerformIO newEnv

newtype TestSolver = TestSolver Int deriving (Show,Eq,Ord)

instance Ipasir (TestSolver) where
    ipasirSignature (TestSolver i) = return $ "TestSolver " ++ show i
    ipasirInit     = TestSolver <$> System.Random.randomIO
  --  ipasirAdd  =
    ipasirSolve _    = return Nothing
    ipasirVal _ _    = return 0
    ipasirFailed _ _ = return False
    ipasirAssume _ _ = undefined
    ipasirAddClause  = undefined
    ipasirGetID (TestSolver i) = fromInteger $ toInteger i

{-|
    Class that models the <https://github.com/biotomas/ipasir/blob/master/ipasir.h ipasir.h> interface.
    This class is meant to be implemented using foreign function interfaces to the actual C solver.
    In most cases the type @a@ will be a @newtype@ around a 'ForeignPtr'.
-}
class Ord a => Ipasir a where

    {-|
     Every initialized Solver needs a unique ID. The ID is mostly the pointer to to solver.
    -}
    ipasirGetID :: a -> Word
    
    {-|
     Returns the maximal variable.  
    -}
    ipasirMaxVar :: a -> IO Word
    ipasirMaxVar solver = maybe 0 id <$> (saveReadVar maxVar $ ipasirGetID solver)

    {-|
     Return the name and the version of the incremental @SAT@
     solving library.
    -}
    ipasirSignature :: a -> IO String
    ipasirSignature solver = return $ "Solver with ID " ++ show (ipasirGetID solver)

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
    ipasirAdd :: a -> Int -> IO ()
    ipasirAdd solver 0  = do
        let s = ipasirGetID solver
        clause <- readVar clauseCreator s 
        ipasirAddClause solver clause
        writeVar clauseCreator s []
    ipasirAdd solver x = do
        let s = ipasirGetID solver
        modifyVar clauseCreator s (x:)

    {-|
     Add an assumption for the next @SAT@ search (the next call
     of 'ipasirSolve'). After calling 'ipasirSolve' all the
     previously added assumptions are cleared.

     Required state: @INPUT@ or @SAT@ or @UNSAT@
     State after: @INPUT@
    -}
    ipasirAssume :: a -> Int -> IO ()

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
    ipasirVal :: a -> Word -> IO Int
    
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
    ipasirAddClause :: a -> [Int] -> IO ()
    ipasirAddClause s [] = ipasirAdd s 0
    ipasirAddClause s (l:ls) = do
        ipasirAdd s l
        ipasirAddClause s ls
    
    {-|
     Add the given clauses.  Clauses added this way
     cannot be removed. The addition of removable clauses
     can be simulated using activation literals and assumptions.

     Required state: @INPUT@ or @SAT@ or @UNSAT@
     State after: @INPUT@

     The default implementation adds each clause by calling 'ipasirAddClause'.
    -}
    ipasirAddClauses :: a -> [[Int]] -> IO ()
    ipasirAddClauses _ [] = return ()
    ipasirAddClauses s (l:ls) = do
        ipasirAddClause  s l
        ipasirAddClauses s ls
    


{-|
  Initializes some variables. If you want to implement a new solver, make sure you 
  call this function in your implementation of 'ipasirInit'
-}
ipasirInitImpl :: Ipasir a => a -> IO ()
ipasirInitImpl solver = do 
    let s = ipasirGetID solver
    writeVar maxVar s 0
    writeVar clauseCreator s []
    writeVar solverState s INPUT

{-|
  If you want to implement a new solver, make sure you 
  call this function in your implementation of 'ipasirAdd' (needed iff you overwrite 'ipasirAdd')
-}
ipasirAddImpl :: Ipasir a => a ->  Int  -> IO ()
ipasirAddImpl solver x = do
    modifyMaxVar solver $ abs x
    setSolverState solver INPUT

{-|
  If you want to implement a new solver, make sure you 
  call this function in your implementation of 'ipasirAddClause' 
  (needed iff you overwrite 'ipasirAddClause')
-}
ipasirAddClauseImpl :: Ipasir a => a -> [Int] -> IO ()
ipasirAddClauseImpl solver clause = do
    modifyMaxVar solver $ maximum $ map abs clause
    setSolverState solver INPUT
    
{-|
  If you want to implement a new solver, make sure you 
  call this function in your implementation of 'ipasirAddClauses' 
  (needed iff you overwrite 'ipasirAddClauses')
-}
ipasirAddClausesImpl :: Ipasir a => a -> [[Int]] -> IO ()
ipasirAddClausesImpl solver clauses = do
    modifyMaxVar solver $ maximum $ map abs $ concat clauses
    setSolverState solver INPUT

{-|
  If you want to implement a new solver, make sure you 
  call this function in your implementation of 'ipasirAssume'.
-}
ipasirAssumeImpl :: Ipasir a => a -> IO ()
ipasirAssumeImpl solver = setSolverState solver INPUT

{-|
  If you want to implement a new solver, make sure you 
  call this function in your implementation of 'ipasirSolve'. The second
  paremeter stands for the regular return value.
-}
ipasirSolveImpl :: Ipasir a => a -> Maybe Bool -> IO ()
ipasirSolveImpl solver b = do
    case b of
        Nothing    -> setSolverState solver INPUT
        Just True  -> setSolverState solver SAT
        Just False -> setSolverState solver UNSAT

{-|
    Leads into an error if its not possible to read a solution. Use it in an implementation
    of 'ipasirVal' or 'ipasirSolution'.
-}
ipasirValImpl :: Ipasir a => a -> IO ()
ipasirValImpl solver = do
    state <- readVar solverState $ ipasirGetID solver
    case state of
        SAT -> return ()
        x   -> error $ "You cant read a solution here. The solver is in the state " 
                        ++ show x ++ " but has to be in the state " ++ show SAT

{-|
    Leads into an error if its not possible to read a conflict. Use it in an implementation
    of 'ipasirFail' or 'ipasirConflict'.
-}                
ipasirFailedImpl :: Ipasir a => a -> IO ()
ipasirFailedImpl solver = do
    state <- readVar solverState $ ipasirGetID solver
    case state of
        UNSAT -> return ()
        x     -> error $ "You cant read a conflict here. The solver is in the state " 
                          ++ show x ++ " but has to be in the state " ++ show UNSAT

{-|
    Sets the maximal variable of the solver on second parameter. Does nothing, if the value
    is already greater or equals.
-}
modifyMaxVar :: (Ipasir a, Enum e) => a -> e -> IO ()
modifyMaxVar solver var = modifyVar maxVar (ipasirGetID solver) (max (toEnum (fromEnum var)))

{-|
    Sets the solver state. This can be @INPUT@, @SAT@ or @UNSAT@. 
-}
setSolverState :: Ipasir a => a -> SolverState -> IO ()
setSolverState solver state = writeVar solverState (ipasirGetID solver) state

{-| 
    Same as 'ipasirAdd' but working on @Maybe (Lit Word)@
-}
ipasirAddLit :: Ipasir a => a -> Maybe (Lit Word) -> IO ()
ipasirAddLit s Nothing = ipasirAdd s 0
ipasirAddLit s (Just lit) = ipasirAdd s $ litToInt lit

{-| 
    Same as 'ipasirAssume' but working on @Lit Word@
-}
ipasirAssumeLit :: Ipasir a => a -> Lit Word -> IO ()
ipasirAssumeLit s lit = ipasirAssume s $ litToInt lit

{-| 
    Same as 'ipasirVal' but working on @Maybe (Lit Word)@
-}
ipasirValLit :: Ipasir a => a -> Word -> IO (Maybe (Lit Word))
ipasirValLit s x = do 
    r <- ipasirVal s x
    return $ case compare r 0 of
        EQ -> Nothing
        LT -> Just $ Neg $ toEnum (-r)
        GT -> Just $ Pos $ toEnum r

{-| 
    Same as 'ipasirAddClause' but working on @Lit Word@
-}
ipasirAddClauseLit :: Ipasir a => a -> [Lit Word] -> IO ()
ipasirAddClauseLit s = ipasirAddClause s . map litToInt

{-| 
    Same as 'ipasirAddClauses' but working on @Lit Word@
-}
ipasirAddClausesLit :: Ipasir a => a -> [[Lit Word]] -> IO ()
ipasirAddClausesLit s clauses = mapM_ (ipasirAddClauseLit s) clauses


{-
iterativeSolving :: Ipasir a => a -> b -> (Vector (Maybe Bool) -> b -> ([[Int]],b) ) -> (Set Word, [[Int]])
iterativeSolving solver v f = undefined
-}

