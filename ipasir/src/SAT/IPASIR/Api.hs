{-# LANGUAGE FlexibleInstances, TypeFamilies, UndecidableInstances #-}

module SAT.IPASIR.Api 
    ( Ipasir (..)
    , SolverState (..)
    , ipasirInit
    , ipasirAdd
    , ipasirAssume
    , ipasirSolve
    , ipasirVal
    , ipasirFailed
    , ipasirConflict
    , ipasirAddClause
    , ipasirAddClauses
    , ipasirAddLit
    , ipasirAssumeLit
    , ipasirValBool
    , ipasirAddClauseLit
    , ipasirAddClausesLit
    ) where

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
    ipasirInit'     = TestSolver <$> System.Random.randomIO
  --  ipasirAdd  =
    ipasirSolve' _    = return LUndef
    ipasirVal' _ _    = return 0
    ipasirFailed' _ _ = return False
    ipasirAssume' _ _ = undefined
    ipasirAddClause'  = undefined
    ipasirGetID (TestSolver i) = fromInteger $ toInteger i

{-|
    Class that models the <https://github.com/biotomas/ipasir/blob/master/ipasir.h ipasir.h> interface.
    This class is meant to be implemented using foreign function interfaces to the actual C solver.
    In most cases the type @a@ will be a @newtype@ around a 'ForeignPtr'.
-}
class Ord a => Ipasir a where
    {-# MINIMAL ipasirGetID, ipasirInit', ipasirAssume', ipasirSolve',
                ( ipasirAdd' | ipasirAddClause'), 
                ( ipasirVal' | ipasirSolution' ),
                ( ipasirFailed' | ipasirConflict' ) #-}
    {-|
     Every initialized Solver needs a unique ID. The ID is mostly the pointer to to solver.
    -}
    ipasirGetID :: a -> Word
    
    {-|
     Returns the maximal variable.  
    -}
    ipasirMaxVar :: a -> IO Word
    ipasirMaxVar solver = readVar maxVar $ ipasirGetID solver
    
    {-|
     Returns the maximal variable.  
    -}
    ipasirState:: a -> IO SolverState
    ipasirState solver = readVar solverState $ ipasirGetID solver

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

     * Required state: @N/A@
     * State after: @INPUT@

     This function also has to take care of deleding the solver when it gets carbage collected.
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
     every other position. The offset at Position 0 makes the following property true:
     
     @
        ipasirSolution' s ! i == ipasirVal' s (toEnum i) -- ignored the IO-monad     @

     * Required state: @SAT@
     * State after: @SAT@
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
    -}
    ipasirFailed' :: a -> Word -> IO Bool
    ipasirFailed' solver var = Vec.elem var <$> ipasirConflict' solver
    
    {-|
      returns every variable, which was involved in the found conflict. The returned
      vector is sorted. It holds that
      
      @
        elem i (ipasirConflict' s) == ipasirFailed' s i -- ignored the IO-monad      @
      
      * Required state: @UNSAT@
      * State after: @UNSAT@
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
    -}
    ipasirAddClauses' :: a -> [[Int]] -> IO ()
    ipasirAddClauses' _ [] = return ()
    ipasirAddClauses' s (l:ls) = do
        ipasirAddClause'  s l
        ipasirAddClauses' s ls

-- | Safe version of 'ipasirInit''
ipasirInit :: Ipasir a => IO a
ipasirInit = do
    solver <- ipasirInit'
    let s = ipasirGetID solver
    writeVar maxVar s 0
    writeVar clauseCreator s []
    writeVar solverState s INPUT
    return solver

-- | Safe version of 'ipasirAdd''
ipasirAdd :: Ipasir a => a -> Int -> IO ()
ipasirAdd solver lit = do
    modifyMaxVar solver $ abs lit
    setSolverState solver INPUT
    ipasirAdd' solver lit

-- | Safe version of 'ipasirAssume''
ipasirAssume :: Ipasir a => a -> Int -> IO ()
ipasirAssume solver lit = do
    setSolverState solver INPUT
    ipasirAssume' solver lit

-- | Safe version of 'ipasirSolve''
ipasirSolve :: Ipasir a => a -> IO LBool
ipasirSolve solver = do
    res <- ipasirSolve' solver
    case res of
        LUndef -> setSolverState solver INPUT
        LTrue  -> setSolverState solver SAT
        LFalse -> setSolverState solver UNSAT
    return res

-- | Safe version of 'ipasirVal''
ipasirVal :: Ipasir a => a -> Word -> IO Int
ipasirVal _ 0      = return 0
ipasirVal solver i = do
    state <- readVar solverState $ ipasirGetID solver
    case state of
        SAT -> ipasirVal' solver i
        x   -> error $ "You cant read a solution here. The solver is in the state " 
                        ++ show x ++ " but has to be in the state " ++ show SAT

-- | Safe version of 'ipasirFailed''
ipasirFailed :: Ipasir a => a -> Word -> IO Bool
ipasirFailed solver i = do
    state <- readVar solverState $ ipasirGetID solver
    case state of
        UNSAT -> ipasirFailed' solver i
        x     -> error $ "You cant read a conflict here. The solver is in the state " 
                          ++ show x ++ " but has to be in the state " ++ show UNSAT

-- | Safe version of 'ipasirConflict''
ipasirConflict :: Ipasir a => a -> IO (Vec.Vector (Word))
ipasirConflict solver = do
    state <- readVar solverState $ ipasirGetID solver
    case state of
        UNSAT -> ipasirConflict' solver
        x     -> error $ "You cant read a conflict here. The solver is in the state " 
                          ++ show x ++ " but has to be in the state " ++ show UNSAT

-- | Safe version of 'ipasirAddClause''
ipasirAddClause :: Ipasir a => a -> [Int] -> IO ()
ipasirAddClause solver clause = do
    noClauseStarted <- null <$> readVar clauseCreator (ipasirGetID solver)
    if noClauseStarted
        then do
            modifyMaxVar solver $ maximum $ map abs clause
            setSolverState solver INPUT
            ipasirAddClause' solver clause
        else error "You cant call ipasirAddClause if you started a clause with ipasirAdd"

-- | Safe version of 'ipasirAddClauses''
ipasirAddClauses :: Ipasir a => a -> [[Int]] -> IO ()
ipasirAddClauses solver cnf = do
    noClauseStarted <- null <$> readVar clauseCreator (ipasirGetID solver)
    if noClauseStarted
        then do
            modifyMaxVar solver $ maximum $ map abs $ concat cnf
            setSolverState solver INPUT
            ipasirAddClauses' solver cnf
        else error "You cant call ipasirAddClauses if you started a clause with ipasirAdd"

{-|
    Sets the maximal variable of the solver on second parameter. Does nothing, if the value
    is already greater or equals.
-}
modifyMaxVar :: (Ipasir a, Enum e) => a -> e -> IO ()
modifyMaxVar solver var = modifyVar maxVar (ipasirGetID solver) (max (toEnum (fromEnum var)))

-- | Sets the solver state. This can be @INPUT@, @SAT@ or @UNSAT@. 
setSolverState :: Ipasir a => a -> SolverState -> IO ()
setSolverState solver state = writeVar solverState (ipasirGetID solver) state

-- | Same as 'ipasirAdd' but working on @Maybe (Lit Word)@
ipasirAddLit :: Ipasir a => a -> Maybe (Lit Word) -> IO ()
ipasirAddLit s Nothing    = ipasirAdd s 0
ipasirAddLit s (Just lit) = ipasirAdd s $ litToInt lit

-- | Same as 'ipasirAssume' but working on @Lit Word@
ipasirAssumeLit :: Ipasir a => a -> Lit Word -> IO ()
ipasirAssumeLit s lit = ipasirAssume s $ litToInt lit
        
-- | Returns @LTrue@ iff the variable is true, @LFalse@ iff the variable is false and
-- | @LUndef@ iff the variable is undefined (not important)
ipasirValBool :: Ipasir a => a -> Word -> IO LBool
ipasirValBool s x =  toMBool . compare 0 <$> ipasirVal s x

toMBool :: Ordering -> LBool
toMBool LT = LTrue
toMBool EQ = LUndef
toMBool GT = LFalse

-- | Same as 'ipasirAddClause' but working on @Lit Word@
ipasirAddClauseLit :: Ipasir a => a -> [Lit Word] -> IO ()
ipasirAddClauseLit s = ipasirAddClause s . map litToInt

-- | Same as 'ipasirAddClauses' but working on @Lit Word@
ipasirAddClausesLit :: Ipasir a => a -> [[Lit Word]] -> IO ()
ipasirAddClausesLit s clauses = mapM_ (ipasirAddClauseLit s) clauses

{-
iterativeSolving :: Ipasir a => a -> b -> (Vector LBool -> b -> ([[Int]],b) ) -> (Set Word, [[Int]])
iterativeSolving solver v f = undefined
-}
