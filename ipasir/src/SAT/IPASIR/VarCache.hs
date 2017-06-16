module SAT.IPASIR.VarCache
    ( Var
    , VarCache
    , emptyCache
    , newVar
    , newVars
    , newHelper
    , newHelpers
    , numVars
    , vars
    , varToInt
    , clausesToInt
    , intToVar
    ) where

import qualified Data.Map    as Map
import qualified Data.Vector as Vec

import Control.Monad.Trans.State

import SAT.IPASIR.Literals


type Var v = Either Word v

-- | 
-- A VarCache manages a mapping between a chosen variable label type and
-- numeric Variables for a SAT-Solver.
-- The numeric values are assigend in ascending order.
--
-- A VariabelCache has to following 'laws':
--
-- @numVars emptyCache == 0@
--
-- @numVars $ execState (newVar v) emptyCache = 1@
--
-- @evalState (newVar v) emptyCache = Left v@
--
-- @numVars $ execState newHelpter emptyCache = 1@
--
-- @evalState newHelpter emptyCache = Right i@
--
-- @intToVar . varToInt = id@
--
-- @varToInt . intToVar = id@
--
data VarCache v = VarCache
    { i2v :: Vec.Vector (Var v)
    , v2i :: Map.Map (Var v) Word
    , nextHelper :: Word
    }

-- | Create an empty cache of where the label type is @l@ and the Variable
-- type is @Either l Word@.
--
-- This defaults to @emptyCache' Left Right@
emptyCache :: VarCache v
emptyCache = VarCache Vec.empty Map.empty 0

-- | insert a label into the cache and returns the resulting variable.
-- If the label is already in the cache it is returned.
newVar :: Ord v => v -> State (VarCache v) (Var v)
newVar l = do
    let v = Right l
    insert v

-- | inserts all labels into the cache and returns the resulting variables
newVars :: Ord v => [v] -> State (VarCache v) [Var v]
newVars = mapM newVar

-- | create a new helper variable
newHelper :: Ord v => State (VarCache v) (Var v)
newHelper =  do
    v <- Left . nextHelper <$> get
    modify $ \ vc -> vc { nextHelper = nextHelper vc + 1 }
    insert v

-- | create @n@ new helper variables
newHelpers :: Ord v => Word -> State (VarCache v) [Var v]
newHelpers nr = const newHelper `mapM` [1..nr]

-- | check how many variables are in the cache
numVars :: VarCache v -> Word
numVars = toEnum . length . i2v

-- | extracts all variables from the cache
vars :: VarCache v -> [Var v]
vars = Vec.toList . i2v

-- | get the integer value for given variable.
-- This will cause an error if the variable is not in the cache.
varToInt :: Ord v => VarCache v -> Var v -> Word
varToInt vc = (v2i vc Map.!)

-- | maps all variables in given clauses to integers using @varToInt@
clausesToInt :: Ord v => VarCache v -> [[Lit (Var v)]] -> [[Lit Word]]
clausesToInt vc = ((<$>).(<$>).(<$>)) (varToInt vc)

-- | get the variable for given integer.
-- This will cause an error if the given integer is not associated with a variable.
intToVar :: VarCache v -> Word -> Var v
intToVar vc = (i2v vc Vec.!) . fromEnum


insert :: Ord v => Var v -> State (VarCache v) (Var v)
insert v = do
    modify $ \ vc -> vc 
        { i2v = i2v vc `Vec.snoc` v
        , v2i = Map.insert v (numVars vc) (v2i vc)
        }
    return v
