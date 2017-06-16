{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module SAT.IPASIR.Formula where

import Prelude hiding (all)

import Data.Bits
import Data.Maybe
import Data.List
import Data.String (IsString(..))
import Data.Foldable
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Comonad

import SAT.IPASIR.Literals
import SAT.IPASIR.Clauses
import SAT.IPASIR.VarCache
import SAT.IPASIR.Solver (HasVariables(..))

data Formula v 
  = Var v                     -- ^ A variable.
  | Yes                       -- ^ The formula /true/.
  | No                        -- ^ The formula /false/.
  | Not  (Formula v)          -- ^ Negation.
  | All  [Formula v]          -- ^ All are true.
  | Some [Formula v]          -- ^ At least one is true.
  | Odd  [Formula v]          -- ^ An odd number is /true/.
  deriving (Show, Eq, Ord)

instance (IsString v) => IsString (Formula v) where
    fromString = Var . fromString

instance (Ord v) => HasVariables (Formula v) where
    type VariableType (Formula v) = v
    getVars vc = undefined -- nub . (map extract) . concat . formulaToCNF

notB (Not x) = x
notB f       = Not f

(All l1) &&* (All l2) = All $ l1++l2
(All l) &&* a = All $ a:l
a &&* (All l) = All $ a:l
a &&* b       = All [a,b]

(Some l1) ||* (Some l2) = Some $ l1++l2
(Some l) ||* a = Some $ a:l
a ||* (Some l) = Some $ a:l
a ||* b        = Some [a,b]

(Odd l1) ++* (Odd l2) = Odd $ l1++l2
(Odd l) ++* a = Odd $ a:l
a ++* (Odd l) = Odd $ a:l
a ++* b        = Odd [a,b]

a ->*  b       = notB a ||* b
a <->* b       = notB $ a ++* b

infixl 1 &&*
infixl 2 ||*
infixl 3 ++*
infixl 4 ->*
infixl 5 <->*

-- Removes all Yes and No from the Formulas
rFormula :: (Eq v) => Formula v -> Formula v
rFormula (All l)
    | No `elem` newForms  = No
    | null reducedList    = Yes
    | otherwise           = All reducedList
    where 
        newForms      = map rFormula l
        reducedList   = filter (/=Yes) newForms
rFormula (Some l)
    | Yes `elem` newForms = Yes
    | null reducedList    = No
    | otherwise           = Some reducedList
    where 
        newForms      = map rFormula l
        reducedList   = filter (/=No) newForms
rFormula (Odd l)
    | null reducedList  = if positive then Yes else No
    | positive          = Odd $ notB (head reducedList) : tail reducedList
    | otherwise         = Odd reducedList
    where 
        newForms            = map rFormula l
        (trash,reducedList) = partition isTerminal newForms
        positive            = odd $ length $ filter ((==Yes).rFormula) trash
        isTerminal form = form' == No || form' == Yes
            where form' = rFormula form

rFormula (Not x)
    | x' == Yes = No
    | x' == No  = Yes
    | otherwise = Not x'
    where x' = rFormula x
rFormula x = x

data DFormula v 
    = DVar  (Lit v) 
    | DAll  [DFormula v]
    | DSome [DFormula v]
    | DOdd [DFormula v]
    deriving (Show, Eq, Ord)

demorgen :: Formula v -> DFormula v
demorgen Yes  = DAll  []
demorgen No   = DSome []
demorgen form = pdemorgen form
    where
        pdemorgen :: Formula v -> DFormula v
        pdemorgen (Var x)  = DVar $ Pos x
        pdemorgen (Not f)  = ndemorgen f
        pdemorgen (All f)  = DAll  $ map pdemorgen f
        pdemorgen (Some f) = DSome $ map pdemorgen f
        pdemorgen (Odd f)  = DOdd $ map pdemorgen f
        
        ndemorgen :: Formula v -> DFormula v
        ndemorgen (Var x)  = DVar $ Neg x
        ndemorgen (Not f)  = pdemorgen f
        ndemorgen (All f)  = DSome $ map ndemorgen f
        ndemorgen (Some f) = DAll  $ map ndemorgen f
        ndemorgen (Odd (x:xs)) = DOdd $ map pdemorgen $ notB x : xs

getHelperDefs :: forall v. Ord v => VarCache v -> DFormula v -> (Word, DFormula v, [(Var v, DFormula (Var v))])
getHelperDefs cache formula = undefined -- (numberHelper, main, helperDefs)
{-
    where
        numberHelper            = countHelper formula :: Word
        (newCache, _, h)        = helpersInNewSpace cache numberHelper
        f1                      = toVar cache         :: v1 -> v2
        f2                      = toHelper cache      :: Word -> v2
        (main, (_,helperDefs))  = runState (getHelperDefs' formula) (0, [])
        getHelperDefs' :: DFormula v1 -> State (Word,[(v2,DFormula v2)]) (DFormula v2)
        getHelperDefs' (DVar x)    = return $ DVar $ (f1 <$> x)
        getHelperDefs' (DAll l)    = DAll <$> foldrM f [] l
            where
                f current work = do
                    formOfCurrent          <- getHelperDefs' current
                    return $ formOfCurrent:work
        getHelperDefs' (DSome l)    = DSome <$> foldrM f [] l
            where
                f (DVar x) work  = return $ (DVar $ f1 <$> x) : work
                f (DSome x) work = do

                    formOfCurrent          <- getHelperDefs' (DSome x)
                    (counter, pastHelpers) <- get
                    return $ formOfCurrent:work
                f current work = do -- need Helper

                    formOfCurrent          <- getHelperDefs' current
                    (counter, pastHelpers) <- get
                    let helper     = h counter
                    put (counter+1, (helper,formOfCurrent) : pastHelpers)
                    return $  (DVar $ Pos helper) : work
        getHelperDefs' (DOdd l)    = DOdd <$> foldrM f [] l
            where
                f (DVar x) work  = return $ (DVar $ f1 <$> x) : work
                f (DOdd x) work = do
                    formOfCurrent          <- getHelperDefs' (DOdd x)
                    (counter, pastHelpers) <- get
                    return $ formOfCurrent:work
                f current work = do -- need Helper
                    formOfCurrent          <- getHelperDefs' current
                    (counter, pastHelpers) <- get
                    let helper     = h counter
                    put (counter+1, (helper,formOfCurrent) : pastHelpers)
                    return $  (DVar $ Pos helper) : work
-}
             




countHelper :: (Enum e,Num e) => DFormula v -> e
countHelper (DVar  x) = toEnum 0
countHelper (DAll  l) = sum $ map countHelper l
countHelper (DSome l) = sum (countHelper `map` normals) + toEnum (length others)
    where
        normals       = filter    (not . isDVar)  l
        others        = partition (not . isDSome) normals
countHelper (DOdd l) = sum (countHelper `map` normals) + toEnum (length others)
    where
        normals       = filter    (not . isDVar) l
        others        = partition (not . isDOdd) normals


isDAll  (DAll _ ) = True
isDAll _          = False
isDSome (DSome _) = True
isDSome _         = False
isDOdd  (DOdd _ ) = True
isDOdd _          = False
isDVar  (DVar _ ) = True
isDVar _          = False
  

        




{-
oddToCNF :: [Lit v] -> [[Lit v]]
oddToCNF clause = do
    let l = length clause
    k <- map (2*) [0..div l 2]
    clause `outOf` k
    where
        outOf :: [Lit v] -> Int -> [[Lit v]]
        outOf clause 0 = [clause]
        outOf []     _ = []
        outOf (x:xs) k = (map (neg x :) left) ++ (map ( x:) right)
            where
                left  = outOf xs (k-1)
                right = outOf xs k
-}    
-- ----------------------------------------------------------------------
-- * A monad for translation to CNF

-- | A monad for translation to CNF. This monad keeps track of two kinds
-- of state: an integer counter to provide a supply of fresh
-- variables, and a list of definitional clauses.
newtype Trans v a = Trans { getState :: State (VarCache v) (a, [Clause (Var v)]) }

instance Monad (Trans v) where
  return = Trans . return . (,[])
  (Trans f) >>= g = Trans $ do
      (a, c) <- f
      (a', c') <- getState $ g a
      return (a', c ++ c')

instance Applicative (Trans v) where
  pure = return
  (<*>) = ap
  
instance Functor (Trans v) where
  fmap = liftM

-- | Run the 'Trans' monad.
runTrans :: Trans v a -> (a, [Clause (Var v)])
runTrans = (`evalState` emptyCache) . getState

formulaToNormalform :: Ord v => Formula v -> NormalForm (Var v)
formulaToNormalform form = (or, xor)
    where
        (rest, clauses)   = runTrans $ transCnf $ demorgen $ rFormula form
        (or1,xor1)        = partitionClauses True rest
        (or2,xor2)        = partitionClauses True clauses
        or                = or1  ++  or2
        xor               = xor1 ++ xor2

normalformToCNF :: Ord v => NormalForm (Var v) -> CNF (Var v)
normalformToCNF (or,xor) = or ++ concat (map oddToCNF xor)

formulaToCNF :: Ord v => Formula v -> CNF (Var v)
formulaToCNF = normalformToCNF . formulaToNormalform

normalformToFormula :: forall v. Ord v => NormalForm (Var v) -> Formula (Var v)
normalformToFormula (or,xor)   = All $ orFormulas ++ xorFormulas
    where
        orFormulas  :: [Formula (Var v)]
        orFormulas   = [ Some $ map (transformLitOdd . (Var <$>)) clause | clause <-  or]
        xorFormulas :: [Formula (Var v)]
        xorFormulas  = [ transformLitOdd $ ((Odd . map Var) <$> clause) | clause <- xor]
        transformLitOdd :: Lit (Formula a) -> Formula a
        transformLitOdd (Pos form) = form
        transformLitOdd (Neg form) = Not form
        


-- | Return a fresh Lit.
freshLit :: Ord v => Trans v (Lit (Var v))
freshLit = Trans $ (,[]) . Pos <$> newHelper

-- | Add one clause.
addClause :: Ord v => Clause (Var v) -> Trans v ()
addClause clause = addClauses [clause]

-- | Add some clauses.
addClauses :: forall v. Ord v => [Clause (Var v)] -> Trans v ()
addClauses clauses = Trans $ return ((), ors ++ xors)
    where
        (orClauses, xorClauses) = partitionClauses False clauses
        ors  = [ Or  x | x <- orClauses ]
        xors = [ XOr x | x <- xorClauses ]

addCnf :: Ord v => [OrClause (Var v)] -> Trans v ()
addCnf cs = Trans $ return ((), map Or cs)

addXnf :: Ord v => [XOrClause (Var v)] -> Trans v ()
addXnf cs = Trans $ return ((), map XOr cs)

partitionList :: (DFormula v -> (Bool,[DFormula v])) -> [DFormula v] -> ([Lit v], [DFormula v])
partitionList f []          = ([],[])
partitionList f (DVar x:xs) = (x:lits, rest) 
    where
        (lits, rest)        = partitionList f xs
partitionList f (x:xs)
    | correctType           = (lits1++lits2, rest1++rest2) 
    | otherwise             = (lits2, x:rest2) 
    where
        (correctType,list)  = f x
        (lits1, rest1)      = partitionList f list
        (lits2, rest2)      = partitionList f xs

partitionAll  :: [DFormula v] -> ([Lit v], [DFormula v])
partitionAll  = partitionList checker
    where
        checker (DAll l)  = (True,l)
        checker _         = (False,[])

partitionSome :: [DFormula v] -> ([Lit v], [DFormula v])
partitionSome = partitionList checker
    where
        checker (DSome l) = (True,l)
        checker _         = (False,[])

partitionOdd :: [DFormula v] -> ([Lit v], [DFormula v])
partitionOdd = partitionList checker
    where
        checker (DOdd l)  = (True,l)
        checker _         = (False,[])

-- _____________________________________________________________

type Env v = Map.Map Integer (Lit v)

lit2ELit :: Lit v -> Lit (Var v)
lit2ELit (Pos x) = Pos $ Right x
lit2ELit (Neg x) = Neg $ Right x

transCnf :: Ord v => DFormula v -> Trans v [Clause (Var v)]
transCnf (DVar (Pos v) ) = return [Or [Pos (Right v)]]
transCnf (DVar (Neg v) ) = return [Or [Neg (Right v)]]

transCnf (DAll l) = do
    a :: [[Clause (Var v)]] <- mapM transCnf l 
--    addClauses a
    return $ concat a

transCnf (DSome l) = do
    let (lits, complexStuff) = partitionSome l
    helpers <- mapM transLit complexStuff
    let lits' = map lit2ELit lits
    return [Or $ lits' ++ helpers]

transCnf (DOdd l) = do
    let (lits, complexStuff) = partitionOdd l
    helpers <- mapM transLit complexStuff
    let lits' = map lit2ELit lits ++ helpers
    let s     = foldl xor True $ map (not.sign) lits'
    return [XOr $ (const (map extract lits') <$> fromBool s)]

transLit a = do
    cnf    <- transCnf a
    litOfNormalForm cnf

-- | Convert a CNF to a single Lit.
litOfNormalForm :: forall v. Ord v => [Clause (Var v)] -> Trans v (Lit (Var v))
litOfNormalForm clauses = do
    let (ors, xors) = partitionClauses False clauses
--    let orLits  = map getLits ors  :: [[ELit v]]
--    let xorLits = map getLits xors :: [Lit [Ext v]]
  
    orHelper  :: [Lit (Var v)] <- mapM litOfOr ors
    xorHelper :: [Lit (Var v)] <- mapM litOfXor xors

    litOfAnd $ orHelper ++ xorHelper

-- | Convert a CNF to a single Lit.
{-lit_of_cnf :: [[ELit v]] -> Trans v (ELit v)
lit_of_cnf ds = do
  xs <- sequence (map litOfOr ds)
  y <- litOfand xs
  return y
-}

-- | Convert a conjunction of Lits to a single Lit.
litOfAnd :: Ord v => [Lit (Var v)] -> Trans v (Lit (Var v))
litOfAnd [l] = return l
litOfAnd cs = do
    x <- freshLit
    -- Define x <-> c1 ∧ ... ∧ cn
    addCnf [[neg x, c] | c <- cs ]
    addCnf [x : [neg c | c <- cs]]
    return x

-- | Convert a disjunction of Lits to a single Lit.
litOfOr :: Ord v => [Lit (Var v)] -> Trans v (Lit (Var v))
litOfOr [l] = return l
litOfOr ds = do
    x <- freshLit
    -- Define x <-> d1 ∨ ... ∨ dn
    addCnf [ [x, neg d] | d <- ds]
    addCnf [neg x : ds]
    return x

-- | Convert an exclusive or of two Lits to a single Lit.
litOfXor :: Ord v => Lit [Var v] -> Trans v (Lit (Var v))
litOfXor (Pos [l]) = return $ Pos l
litOfXor (Neg [l]) = return $ Neg l
litOfXor ds = do
    z <- freshLit
    -- Define z <-> x1 ⊕ ... ⊕ xn 
    addXnf [neg $ ((extract z:) <$> ds)]
    return z


