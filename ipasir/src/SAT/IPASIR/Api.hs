{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module SAT.IPASIR.Api 
    ( Ipasir (..)
    , SolverState (..)
    , ipasirInit
    , ipasirAdd
    , ipasirAssume
    , ipasirSolve
    , ipasirVal
    , ipasirSolution
    , ipasirFailed
    , ipasirConflict
    , ipasirAddClause
    , ipasirAddClauses
    , ipasirAddLit
    , ipasirAssumeLit
    , ipasirValBool
    , ipasirAddClauseLit
    , ipasirAddClausesLit
    , ipasirUnfoldSolving
    , ipasirIterativeSolving
    , ipasirAllSolutionsIn
    , ipasirAllSolutions
    ) where

import Control.Monad
import Control.Comonad
import qualified Data.Vector as Vec
import Data.IORef
import Data.Word
import qualified Data.Set as Set
import System.IO.Unsafe
import Data.Bifunctor
import Debug.Trace
import Control.DeepSeq

import SAT.IPASIR.EnvironmentVariable
import SAT.IPASIR.Literals

data SolverState = INPUT | SAT | UNSAT
    deriving (Show,Eq)

type IDType = Word

maxVar :: Env IDType Word
maxVar = unsafePerformIO newEnv

clauseCreator :: Env IDType [Int]
clauseCreator = unsafePerformIO newEnv

solverState :: Env IDType SolverState
solverState = unsafePerformIO newEnv

executionProcess :: Env IDType (Maybe Stuff)
executionProcess = unsafePerformIO newEnv

data Stuff = forall a. (Show a, NFData a) => Stuff a

instance NFData (Stuff) where
    rnf (Stuff x) = rnf x

{-|
    Class that models the <https://github.com/biotomas/ipasir/blob/master/ipasir.h ipasir.h> interface.
    This class is meant to be implemented using foreign function interfaces to the actual C solver.
    In most cases the type @a@ will be a @newtype@ around a 'ForeignPtr'.
    
    A solver can be in different states (see 'SolverState'). Notice, that every function ending with
    a dash (like 'ipasirAdd'') is not state secure. That means: It is not checked if the solver is in 
    a valid state to execute the function and the state doesn\'t update.
    
    These state unsafe functions are just for the devolopers, who wants to instantiate more solvers.
    Dont use these functions from outside. Every state unsafe functions has a pendant without a dash.
-}
class Ipasir a where
    {-# MINIMAL ipasirGetID, ipasirInit', ipasirAssume', ipasirSolve',
                ( ipasirAdd' | ipasirAddClause'),
                ( ipasirVal' | ipasirSolution' ),
                ( ipasirFailed' | ipasirConflict' ) #-}
                
    -- | Every initialized Solver needs a unique ID. The ID is mostly the pointer to to solver.
    ipasirGetID :: a -> IDType
    
    -- | Returns the maximal variable.  
    ipasirMaxVar :: a -> IO Word
    ipasirMaxVar solver = readVar maxVar $ ipasirGetID solver
    
    -- | Returns the maximal variable.  
    ipasirState:: a -> IO SolverState
    ipasirState solver = readVar solverState $ ipasirGetID solver

    -- | Return the name and the version of the incremental @SAT@ solving library.
    ipasirSignature :: a -> IO String
    ipasirSignature solver = return $ "Solver with ID " ++ show (ipasirGetID solver)

    {-|
     Construct a new solver and return a pointer to it.
     Use the returned pointer as the first parameter in each
     of the following functions.

     * Required state: @N/A@
     * State after: @INPUT@

     This function also has to take care of deleding the solver when it gets carbage collected.
     
     Since this function is not state safe it is not checked if the solver is in the required state
     and the solver does not switch to the \"State after\" state from above.
    -}
    ipasirInit' :: IO a

    {-|
     Add the given literal into the currently added clause
     or finalize the clause with a 'LUndef'.  Clauses added this way
     cannot be removed. The addition of removable clauses
     can be simulated using activation literals and assumptions.

     * Required state: @INPUT@ or @SAT@ or @UNSAT@
     * State after: @INPUT@

     Literals are encoded as (non-zero) integers as in the
     DIMACS formats.  They have to be smaller or equal to
     INT_MAX and strictly larger than INT_MIN (to avoid
     negation overflow).  This applies to all the literal
     arguments in API functions.
          
     Warning: Not state safe.
    -}
    ipasirAdd' :: a -> Int -> IO ()
    ipasirAdd' solver 0  = do
        let s = ipasirGetID solver
        clause <- readVar clauseCreator s 
        ipasirAddClause' solver clause
        writeVar clauseCreator s []
    ipasirAdd' solver x = do
        let s = ipasirGetID solver
        modifyVar clauseCreator s (x:)

    {-|
     Add an assumption for the next @SAT@ search (the next call
     of 'ipasirSolve'). After calling 'ipasirSolve' all the
     previously added assumptions are cleared.

     * Required state: @INPUT@ or @SAT@ or @UNSAT@
     * State after: @INPUT@
          
     Warning: Not state safe.
    -}
    ipasirAssume' :: a -> Int -> IO ()

    {-|
     Solve the formula with specified clauses under the specified assumptions.
     If the formula is satisfiable the function returns @LTrue@ and the state of the solver is changed to @SAT@.
     If the formula is unsatisfiable the function returns @LFalse@ and the state of the solver is changed to @UNSAT@.
     @ipasir_set_terminate@ is not supported.
     This function can be called in any defined state of the solver.

     * Required state: @INPUT@ or @SAT@ or @UNSAT@
     * State after: @INPUT@ or @SAT@ or @UNSAT@
          
     Warning: Not state safe.
    -}
    ipasirSolve' :: a -> IO LBool

    {-|
     Get the truth value of the given literal in the found satisfying
     assignment. Return @Pos a@ if True, @Neg a@ if False, and @LUndef@ if not important.
     This function can only be used if ipasirSolve has returned @LTrue@
     and no 'ipasirAdd' nor 'ipasirAssume' has been called
     since then, i.e., the state of the solver is @SAT@.

     * Required state: @SAT@
     * State after: @SAT@
          
     Warning: Not state safe.
    -}
    ipasirVal' :: a -> Word -> IO Int
    ipasirVal' solver index = do
        vector <- ipasirSolution' solver
        let int = fromEnum index
        let b   = vector Vec.! int
        return $ case b of
            LUndef ->    0
            LTrue  ->  int
            LFalse -> -int
    
    {-|
     ipasirSolution' gives you a Vector with a 'LUndef' at position 0 and the truth values on
     every other position. The offset makes the following property true:
     
     @
        ipasirSolution' s ! i == ipasirVal' s (toEnum i) -- ignored the IO-monad     @

     * Required state: @SAT@
     * State after: @SAT@
          
     Warning: Not state safe.
    -}
    ipasirSolution' :: a -> IO (Vec.Vector LBool)
    ipasirSolution' solver = do
        numberVars <- fromEnum <$> readVar maxVar (ipasirGetID solver)
        Vec.generateM (numberVars+1) f
        where
            f :: Int -> IO LBool
            f 0 = return LUndef
            f n = toMBool <$> compare 0 <$> ipasirVal' solver (toEnum n)
    
    {-|
     Check if the given assumption literal was used to prove the
     unsatisfiability of the formula under the assumptions
     used for the last @SAT@ search. Return @True@ if so, @False@ otherwise.
     This function can only be used if 'ipasirSolve' has returned @LFalse@ and
     no 'ipasirAdd' or 'ipasirAssume' has been called since then, i.e.,
     the state of the solver is @UNSAT@.

     * Required state: @UNSAT@
     * State after: @UNSAT@
          
     Warning: Not state safe.
    -}
    ipasirFailed' :: a -> Word -> IO Bool
    ipasirFailed' solver var = Vec.elem var <$> ipasirConflict' solver
    
    {-|
      Returns every variable, which was involved in the found conflict. The returned
      vector is sorted and distinct. It holds that
      
      @
        elem i (ipasirConflict' s) == ipasirFailed' s i -- ignored the IO-monad      @
      
      * Required state: @UNSAT@
      * State after: @UNSAT@
           
      Warning: Not state safe.
    -}
    ipasirConflict' :: a -> IO (Vec.Vector Word)
    ipasirConflict' solver = do
        numberVars <- fromEnum <$> readVar maxVar (ipasirGetID solver)
        Vec.filterM (ipasirFailed' solver) $ Vec.generate numberVars ((+1).toEnum)

    {-|
     Add the given clause.  Clauses added this way
     cannot be removed. The addition of removable clauses
     can be simulated using activation literals and assumptions.

     * Required state: @INPUT@ or @SAT@ or @UNSAT@
     * State after: @INPUT@

     The default implementation adds each literal of the clause by calling 'ipasirAdd' and finally adding a 'LUndef'.
          
     Warning: Not state safe.
    -}
    ipasirAddClause' :: a -> [Int] -> IO ()
    ipasirAddClause' s [] = ipasirAdd' s 0
    ipasirAddClause' s (l:ls) = do
        ipasirAdd' s l
        ipasirAddClause' s ls
    
    {-|
     Add the given clauses.  Clauses added this way
     cannot be removed. The addition of removable clauses
     can be simulated using activation literals and assumptions.

     * Required state: @INPUT@ or @SAT@ or @UNSAT@
     * State after: @INPUT@

     The default implementation adds each clause by calling 'ipasirAddClause'.
          
     Warning: Not state safe.
    -}
    ipasirAddClauses' :: a -> [[Int]] -> IO ()
    ipasirAddClauses' _ [] = return ()
    ipasirAddClauses' s (l:ls) = do
        ipasirAddClause'  s l
        ipasirAddClauses' s ls

ipasirSeqAll :: Ipasir a => a -> IO ()
ipasirSeqAll solver = do
    let s = ipasirGetID solver
    !ex <- readVar executionProcess s
    deepseq ex $ writeVar executionProcess s Nothing
    return ()

-- | State-safe version of 'ipasirInit''
ipasirInit :: Ipasir a => IO a
ipasirInit = do
    solver <- ipasirInit'
    let s = ipasirGetID solver
    writeVar maxVar s 0
    writeVar clauseCreator s []
    writeVar solverState s INPUT
    writeVar executionProcess s Nothing
    return solver

-- | State-safe version of 'ipasirAdd''
ipasirAdd :: Ipasir a => a -> Int -> IO ()
ipasirAdd solver lit = do
    ipasirSeqAll solver
    modifyMaxVar solver $ abs lit
    setSolverState solver INPUT
    ipasirAdd' solver lit

-- | State-safe version of 'ipasirAssume''
ipasirAssume :: Ipasir a => a -> Int -> IO ()
ipasirAssume solver lit = do
    ipasirSeqAll solver
    setSolverState solver INPUT
    ipasirAssume' solver lit

-- | State-safe version of 'ipasirSolve''
ipasirSolve :: Ipasir a => a -> IO LBool
ipasirSolve solver = do
    ipasirSeqAll solver
    res <- ipasirSolve' solver
    case res of
        LUndef -> setSolverState solver INPUT
        LTrue  -> setSolverState solver SAT
        LFalse -> setSolverState solver UNSAT
    return res

-- | State-safe version of 'ipasirVal''
ipasirVal :: Ipasir a => a -> Word -> IO Int
ipasirVal _ 0      = return 0
ipasirVal solver i = do
    ipasirSeqAll solver
    state <- readVar solverState $ ipasirGetID solver
    case state of
        SAT -> ipasirVal' solver i
        x   -> error $ "You cant read a solution here. The solver is in the state " 
                        ++ show x ++ " but has to be in the state " ++ show SAT
                        
-- | State-safe version of 'ipasirSolution''
ipasirSolution :: Ipasir a => a -> IO (Vec.Vector LBool)
ipasirSolution solver = do
    ipasirSeqAll solver
    state <- readVar solverState $ ipasirGetID solver
    case state of
        SAT -> ipasirSolution' solver
        x   -> error $ "You cant read a solution here. The solver is in the state " 
                        ++ show x ++ " but has to be in the state " ++ show SAT

-- | State-safe version of 'ipasirFailed''
ipasirFailed :: Ipasir a => a -> Word -> IO Bool
ipasirFailed solver i = do
    ipasirSeqAll solver
    state <- readVar solverState $ ipasirGetID solver
    case state of
        UNSAT -> ipasirFailed' solver i
        x     -> error $ "You cant read a conflict here. The solver is in the state " 
                          ++ show x ++ " but has to be in the state " ++ show UNSAT

-- | State-safe version of 'ipasirConflict''
ipasirConflict :: Ipasir a => a -> IO (Vec.Vector (Word))
ipasirConflict solver = do
    ipasirSeqAll solver
    state <- readVar solverState $ ipasirGetID solver
    case state of
        UNSAT -> ipasirConflict' solver
        x     -> error $ "You cant read a conflict here. The solver is in the state " 
                          ++ show x ++ " but has to be in the state " ++ show UNSAT

-- | State-safe version of 'ipasirAddClause''
ipasirAddClause :: Ipasir a => a -> [Int] -> IO ()
ipasirAddClause solver clause = do
    ipasirSeqAll solver
    noClauseStarted <- null <$> readVar clauseCreator (ipasirGetID solver)
    if noClauseStarted
        then do
            modifyMaxVar solver $ maximum $ map abs clause
            setSolverState solver INPUT
            ipasirAddClause' solver clause
        else error "You cant call ipasirAddClause if you started a clause with ipasirAdd"

-- | State-safe version of 'ipasirAddClauses''
ipasirAddClauses :: Ipasir a => a -> [[Int]] -> IO ()
ipasirAddClauses solver cnf = do
    ipasirSeqAll solver
    noClauseStarted <- null <$> readVar clauseCreator (ipasirGetID solver)
    if noClauseStarted
        then do
            modifyMaxVar solver $ maximum $ map abs $ concat cnf
            setSolverState solver INPUT
            ipasirAddClauses' solver cnf
        else error "You cant call ipasirAddClauses if you started a clause with ipasirAdd"

-- | Sets the maximal variable of the solver on second parameter. Does nothing, if the value
-- | is already greater or equals.
modifyMaxVar :: (Ipasir a, Enum e) => a -> e -> IO ()
modifyMaxVar solver var = modifyVar maxVar (ipasirGetID solver) (max (toEnum (fromEnum var)))

-- | Sets the solver state. This can be @INPUT@, @SAT@ or @UNSAT@. 
setSolverState :: Ipasir a => a -> SolverState -> IO ()
setSolverState solver state = writeVar solverState (ipasirGetID solver) state

-- | Same as 'ipasirAdd' but working on @Maybe (IntLiteral)@
ipasirAddLit :: (Ipasir a, IntLiteral il) => a -> Maybe il -> IO ()
ipasirAddLit s Nothing    = ipasirAdd s 0
ipasirAddLit s (Just lit) = ipasirAdd s $ litToInt lit

-- | Same as 'ipasirAssume' but working on 'IntLiteral'
ipasirAssumeLit :: (Ipasir a, IntLiteral il) => a -> il -> IO ()
ipasirAssumeLit s lit = ipasirAssume s $ litToInt lit
        
-- | Returns @LTrue@ iff the variable is true, @LFalse@ iff the variable is false and
-- | @LUndef@ iff the variable is undefined (not important)
ipasirValBool :: Ipasir a => a -> Word -> IO LBool
ipasirValBool s x =  toMBool . compare 0 <$> ipasirVal s x

toMBool :: Ordering -> LBool
toMBool LT = LTrue
toMBool EQ = LUndef
toMBool GT = LFalse

-- | Same as 'ipasirAddClause' but working on 'IntLiteral'
ipasirAddClauseLit :: (Ipasir a, IntLiteral il) => a -> [il] -> IO ()
ipasirAddClauseLit s = ipasirAddClause s . map litToInt

-- | Same as 'ipasirAddClauses' but working on 'IntLiteral'
ipasirAddClausesLit :: (Ipasir a, IntLiteral il) => a -> [[il]] -> IO ()
ipasirAddClausesLit s clauses = mapM_ (ipasirAddClauseLit s) clauses

-- | @unfoldSolving solver f b@ solves iterative until the solver blocks. @f@ generates new clauses in each step. The new clauses
-- | can depend on the old solution (first parameter of f) and a general state (second parameter). The start state is given in @b@.
-- | The return value is a tuple of the conflict, which optains after the solver blocked and the solutions. The solutions start with
-- | the first itetation, which makes using laziness possible. 
ipasirUnfoldSolving :: Ipasir a => a -> (Vec.Vector LBool -> b -> ([[Int]],b) ) -> b -> IO (Maybe (Set.Set Word), [Vec.Vector LBool])
ipasirUnfoldSolving solver f b = do
    ipasirSeqAll solver
    (c,s) <- ipasirUnfoldSolving' solver f b
    writeVar executionProcess sID $ Just $ Stuff c
    return (c,s)
    where
        sID = ipasirGetID solver
        ipasirUnfoldSolving' solver f b = unsafeInterleaveIO $ do
            state <- ipasirSolve' solver
            case state of
                LUndef -> do
                    return (Nothing, [])
                LFalse -> do
                        conflict <- ipasirConflict' solver
                        writeVar solverState sID UNSAT
                        return (Just $ Set.fromDistinctAscList $ Vec.toList conflict ,[])
                LTrue  -> do
                    solution <- ipasirSolution' solver
                    let (clauses,newB) = trace "solved" $ f solution b
                    ipasirAddClauses' solver clauses
                    second (solution:) <$> ipasirUnfoldSolving' solver f newB

-- | Same as 'unfoldSolving' but without a general state.
ipasirIterativeSolving :: Ipasir a => a -> (Vec.Vector LBool -> [[Int]]) -> IO (Maybe (Set.Set Word), [Vec.Vector LBool])
ipasirIterativeSolving solver f = ipasirUnfoldSolving solver (const . (,()) . f) ()

-- | Returns all possible solutions for the variables given in the second paramter.
ipasirAllSolutionsIn :: Ipasir a => a -> [Word] -> IO (Maybe (Set.Set Word), [Vec.Vector LBool])
ipasirAllSolutionsIn solver vars = ipasirIterativeSolving solver newClause
    where
        newClause sol = [filter (/=0) $ map (\v -> negate $ litToInt $ (sol Vec.! fromEnum v, v) ) vars]

-- | Returns all possible solutions.
ipasirAllSolutions :: Ipasir a => a -> IO (Maybe (Set.Set Word), [Vec.Vector LBool])
ipasirAllSolutions solver = ipasirIterativeSolving solver newClause
    where
        newClause sol = [Vec.toList $ Vec.filter (/=0) $ Vec.imap (\i b -> negate $ litToInt (b,i) ) sol]

