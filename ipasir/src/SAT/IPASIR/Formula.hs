{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module SAT.IPASIR.Formula where

import Prelude hiding (all)

import Data.Bits
import Data.Maybe
import Data.List
import Data.String (IsString(..))
import Data.Foldable
import Data.Bifunctor
import Data.Traversable
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Comonad

import SAT.IPASIR.Literals
import SAT.IPASIR.Clauses
-- import SAT.IPASIR.Solver (HasVariables(..))
import SAT.IPASIR.VarCache


data Formula v 
  = Var v                     -- ^ A variable.
  | Yes                       -- ^ The formula /true/.
  | No                        -- ^ The formula /false/.
  | Not  (Formula v)          -- ^ Negation.
  | All  [Formula v]          -- ^ All are true.
  | Some [Formula v]          -- ^ At least one is true.
  | Odd  [Formula v]          -- ^ An odd number is /true/.
  deriving (Show, Eq, Ord, Functor)  

instance (IsString v) => IsString (Formula v) where
    fromString = Var . fromString

instance Foldable Formula where
    foldMap g (Var v)   = g v
    foldMap g (Not f)   = foldMap g f
    foldMap g (All  fs) = fold $ map (foldMap g) fs
    foldMap g (Some fs) = fold $ map (foldMap g) fs
    foldMap g (Odd  fs) = fold $ map (foldMap g) fs
    foldMap _ _         = mempty

instance Traversable Formula where
    traverse g (Var v)   = Var <$> g v
    traverse _ Yes       = Yes
    traverse _ No        = No
    traverse g (Not f)   = Not  <$> traverse g f
    traverse g (All  fs) = All  <$> sequenceA $ map (traverse g) fs
    traverse g (Some fs) = Some <$> sequenceA $ map (traverse g) fs
    traverse g (Odd  fs) = Odd  <$> sequenceA $ map (traverse g) fs

getVars :: (Applicative f, Monoid (f v)) => Formula v -> f v
getVars = foldMap pure 

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

type Trans v a = State (VarCache v, [Clause (Var v)], [(Integer, Formula (Var v))]) a

addHelperInfo :: Integer -> Formula (Var v) -> Trans v ()
addHelperInfo i f = do
    (cache, clauses, defs) <- get
    put (cache, clauses, (i,f):defs)
    return ()   

-- | Return a fresh Lit.
freshLit :: Ord v => Trans v (Lit (Var v))
freshLit = do
    (cache, clauses, defs) <- get
    let (newVar, newCache) = newHelper cache
    put (newCache, clauses ,defs)
    return $ Pos newVar

-- | Add one clause.
addClause :: Clause (Var v) -> Trans v ()
addClause clause = addClauses [clause]

-- | Add some clauses.
addClauses :: forall v. [Clause (Var v)] -> Trans v ()
addClauses clauses = do
    (cache, clauses', defs) <- get
    put (cache, clauses ++ clauses', defs)
    return ()
    where
        (orClauses, xorClauses) = partitionClauses False clauses
        ors  = [ Or  x | x <- orClauses ]
        xors = [ XOr x | x <- xorClauses ] 

runTrans :: VarCache v -> Trans v [Clause (Var v)] -> (VarCache v, NormalForm (Var v))
runTrans cache trans = (newCache, (or,xor) ) 
    where
        (mainCNF, (newCache, cnfs, _) ) = runState trans (cache, [], [])
        cnf      = mainCNF++cnfs
        (or,xor) = partitionClauses True cnf
        
-- -----------------------------------------------------------------------------
    

formulaToNormalform :: Ord v => VarCache v -> Formula v -> (VarCache v, NormalForm (Var v))
formulaToNormalform cache form =  runTrans cache $ transCnf $ demorgen $ rFormula form

normalformToCNF :: Eq v => NormalForm (Var v) -> CNF (Var v)
normalformToCNF (or,xor) = or ++ concat (map oddToCNF xor)

formulaToCNF :: Ord v => VarCache v -> Formula v -> (VarCache v , CNF (Var v))
formulaToCNF cache formula = second normalformToCNF $ formulaToNormalform cache formula

normalformToFormula :: forall v. NormalForm (Var v) -> Formula (Var v)
normalformToFormula (or,xor)   = All $ orFormulas ++ xorFormulas
    where
        orFormulas  :: [Formula (Var v)]
        orFormulas   = [ Some $ map (transformLitOdd . (Var <$>)) clause | clause <-  or]
        xorFormulas :: [Formula (Var v)]
        xorFormulas  = [ transformLitOdd $ ((Odd . map Var) <$> clause) | clause <- xor]
        transformLitOdd :: Lit (Formula a) -> Formula a
        transformLitOdd (Pos form) = form
        transformLitOdd (Neg form) = Not form
        



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
--    let orLits  = map getLits ors  :: [[Lit (Var v)]]
--    let xorLits = map getLits xors :: [Lit [Var v]]
  
    orHelper  :: [Lit (Var v)] <- mapM litOfOr ors
    xorHelper :: [Lit (Var v)] <- mapM litOfXor xors

    litOfAnd $ orHelper ++ xorHelper

-- | Convert a CNF to a single Lit.
{-lit_of_cnf :: [[Lit (Var v)]] -> Trans v (Lit (Var v))
lit_of_cnf ds = do
  xs <- sequence (map litOfOr ds)
  y <- litOfand xs
  return y
-}

-- | Convert a conjunction of Lits to a single Lit.
litOfAnd :: Ord v => [Lit (Var v)] -> Trans v (Lit (Var v))
litOfAnd [l] = return l
litOfAnd cs = do
    x :: Lit (Var v) <- freshLit
    -- Define x <-> c1 ∧ ... ∧ cn
    addClauses [ Or [neg x, c] | c <- cs ]
    addClause $ Or $ x : [neg c | c <- cs]
    return x

-- | Convert a disjunction of Lits to a single Lit.
litOfOr :: Ord v => [Lit (Var v)] -> Trans v (Lit (Var v))
litOfOr [l] = return l
litOfOr ds = do
    x <- freshLit
    -- Define x <-> d1 ∨ ... ∨ dn
    addClauses [ Or [x, neg d] | d <- ds]
    addClause $ Or $ neg x : ds
    return x

-- | Convert an exclusive or of two Lits to a single Lit.
litOfXor :: Ord v => Lit [Var v] -> Trans v (Lit (Var v))
litOfXor (Pos [l]) = return $ Pos l
litOfXor (Neg [l]) = return $ Neg l
litOfXor ds = do
    z <- freshLit
    -- Define z <-> x1 ⊕ ... ⊕ xn 
    addClause $ XOr $ neg $ ((extract z:) <$> ds)
    return z


