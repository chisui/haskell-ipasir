{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SAT.IPASIR.Formula where

import Prelude hiding (all)

import Data.Bits
import Data.Maybe
import Data.List
import Data.Either
import Data.String (IsString(..))
import Data.Foldable
import Data.Bifunctor
import Data.Traversable
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Comonad

import SAT.IPASIR.Literals
import SAT.IPASIR.Clauses
import SAT.IPASIR.Solver (HasVariables(..))
import SAT.IPASIR.VarCache

{-
data Formula v 
  = Var v                     -- ^ A variable.
  | Yes                       -- ^ The formula /true/.
  | No                        -- ^ The formula /false/.
  | Not  (Formula v)          -- ^ Negation.
  | All  [Formula v]          -- ^ All are true.
  | Some [Formula v]          -- ^ At least one is true.
  | Odd  [Formula v]          -- ^ An odd number is /true/.
  deriving (Show, Eq, Ord, Functor)  
-}

type  Formula v = GeneralFormula Normal v
type RFormula v = GeneralFormula Reduced v
type DFormula v = GeneralFormula Demorgened v

data Normal
data Reduced
data Demorgened

class Upper a where
instance Upper (Normal) where
instance Upper (Reduced) where

data GeneralFormula s v where 
    Var  :: Upper s => v -> GeneralFormula s v  -- ^ A variable.
    LVar :: Lit v -> GeneralFormula Demorgened v  -- ^ A variable.
    Yes  :: GeneralFormula Normal v       -- ^ The formula /true/.
    No   :: GeneralFormula Normal v       -- ^ The formula /false/.
    Not  :: Upper s => GeneralFormula s v ->  GeneralFormula s v  -- ^ Negation.
    All  :: [GeneralFormula s v] ->  GeneralFormula s v           -- ^ All are true.
    Some :: [GeneralFormula s v] ->  GeneralFormula s v          -- ^ At least one is true.
    Odd  :: [GeneralFormula s v] ->  GeneralFormula s v          -- ^ An odd number is /true/.
deriving instance Show v => Show (GeneralFormula s v)
deriving instance Ord v  => Ord  (GeneralFormula s v)
deriving instance Eq v   => Eq   (GeneralFormula s v)
deriving instance Functor (GeneralFormula s)

instance (IsString v) => IsString (Formula v) where
    fromString = Var . fromString

instance Foldable (GeneralFormula s) where
    foldMap g (Var  v)  = g v
    foldMap g (Not  f)  = foldMap g f
    foldMap g (All  fs) = deepFoldMap g fs
    foldMap g (Some fs) = deepFoldMap g fs
    foldMap g (Odd  fs) = deepFoldMap g fs
    foldMap _ _         = mempty
deepFoldMap g = fold . map (foldMap g)

instance Traversable (GeneralFormula s) where
    traverse g (Var  v)  = Var  <$> g v
    traverse g (Not  f)  = Not  <$> traverse g f
    traverse g (All  fs) = All  <$> deepTraverse g fs
    traverse g (Some fs) = Some <$> deepTraverse g fs
    traverse g (Odd  fs) = Odd  <$> deepTraverse g fs
    traverse _ Yes       = pure Yes
    traverse _ No        = pure No
deepTraverse g = traverse (traverse g)

instance (Ord v, FormulaOperation (GeneralFormula s)) => HasVariables (GeneralFormula s v) where
    type VariableType (GeneralFormula s v) = v
    getAllVariables f vc = map extract $ concat $ snd $ formulaToCNF vc f

    getVariables c vc = Set.map Left (getHelpers c vc) `Set.union` Set.map Right (getLabels c)
    getLabels         = Set.fromList . toList
    getHelpers c vc   = Set.fromList helper
        where
            (_,_,_,defs) = runTransComplete emptyCache $ transCnf $ demorgen c
            helper = map id $ lefts $ map fst defs

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
(Odd l) ++* a  = Odd $ a:l
a ++* (Odd l)  = Odd $ a:l
a ++* b        = Odd [a,b]

a ->*  b       = notB a ||* b
a <->* b       = notB $ a ++* b

infixl 1 &&*
infixl 2 ||*
infixl 3 ++*
infixl 4 ->*
infixl 5 <->*

class (Foldable fo, Traversable fo) => FormulaOperation (fo :: * -> *) where
    -- Removes all Yes and No from the Formulas
    rFormula :: Eq v => fo v -> RFormula v
    demorgen :: Eq v => fo v -> DFormula v

instance FormulaOperation (GeneralFormula Normal) where
    rFormula formula 
        | Yes == reduced = All []
        | No  == reduced = Some []
        | otherwise      = transformer reduced
        where
            reduced = rFormula' formula

            transformer :: Formula v -> RFormula v
            transformer (Var x)  = Var x
            transformer (Not x)  = Not $ transformer x
            transformer (All l)  = All $ map transformer l
            transformer (Some l) = Some $ map transformer l
            transformer (Odd l)  = Odd $ map transformer l

            rFormula' (All l)
                | No `elem` newForms  = No
                | null reducedList    = Yes
                | otherwise           = All reducedList
                where 
                    newForms      = map rFormula' l
                    reducedList   = filter (/=Yes) newForms
            rFormula' (Some l)
                | Yes `elem` newForms = Yes
                | null reducedList    = No
                | otherwise           = Some reducedList
                where 
                    newForms      = map rFormula' l
                    reducedList   = filter (/=No) newForms
            rFormula' (Odd l)
                | null reducedList  = if positive then Yes else No
                | positive          = Odd $ notB (head reducedList) : tail reducedList
                | otherwise         = Odd reducedList
                where 
                    newForms            = map rFormula' l
                    (trash,reducedList) = partition isTerminal newForms
                    positive            = odd $ length $ filter ((==Yes).rFormula') trash
                    isTerminal form = form' == No || form' == Yes
                        where form' = rFormula' form
            rFormula' (Not x)
                | x' == Yes = No
                | x' == No  = Yes
                | otherwise = Not x'
                where x' = rFormula' x
            rFormula' x = x
    demorgen = demorgen . rFormula

instance FormulaOperation (GeneralFormula Reduced) where
    rFormula = id
    demorgen form = pdemorgen form
        where
            pdemorgen :: RFormula v -> DFormula v
            pdemorgen (Var x)  = LVar $ Pos x
            pdemorgen (Not f)  = ndemorgen f
            pdemorgen (All f)  = All  $ map pdemorgen f
            pdemorgen (Some f) = Some $ map pdemorgen f
            pdemorgen (Odd f)  = Odd $ map pdemorgen f
            
            ndemorgen :: RFormula v -> DFormula v
            ndemorgen (Var x)  = LVar $ Neg x
            ndemorgen (Not f)  = pdemorgen f
            ndemorgen (All f)  = Some $ map ndemorgen f
            ndemorgen (Some f) = All  $ map ndemorgen f
            ndemorgen (Odd (x:xs)) = Odd $ map pdemorgen $ notB x : xs

instance FormulaOperation (GeneralFormula Demorgened) where
    rFormula (LVar (Pos x)) = Var x
    rFormula (LVar (Neg x)) = Not $ Var x
    rFormula (All l)        = All $ map rFormula l
    rFormula (Some l)       = Some $ map rFormula l
    rFormula (Odd l)        = Odd $ map rFormula l
    demorgen = id 

type Trans v a = State (VarCache v, [Clause (Var v)], [(Var v, DFormula (Var v))]) a

addDefinition :: Var v -> DFormula (Var v) -> Trans v ()
addDefinition i f = modify (\(cache, clauses, defs) -> (cache, clauses, (i,f):defs))

-- | Return a fresh Lit.
freshLit :: Ord v => Trans v (Lit (Var v))
freshLit = state (\(cache, clauses, defs) -> let (newVar, newCache) = newHelper cache
                                             in  ( Pos newVar,(newCache, clauses ,defs) ) )
                                       
-- | Add one clause.
addClause :: Clause (Var v) -> Trans v ()
addClause clause = addClauses [clause]

-- | Add some clauses.
addClauses :: forall v. [Clause (Var v)] -> Trans v ()
addClauses clauses = modify (\(cache, clauses', defs) -> (cache, clauses ++ clauses', defs)) 

runTrans :: VarCache v -> Trans v [Clause (Var v)] -> (VarCache v, NormalForm (Var v))
runTrans cache trans = (newCache, (or,xor) ) 
    where
        (mainCNF, (newCache, cnfs, _) ) = runState trans (cache, [], [])
        cnf      = mainCNF++cnfs
        (or,xor) = partitionClauses True cnf
        
runTransComplete :: VarCache v -> Trans v [Clause (Var v)] -> ([Clause (Var v)], VarCache v, [Clause (Var v)], [(Var v, DFormula (Var v))])
runTransComplete cache trans = (mainCNF, newCache, cnfs, defs)
    where
        (mainCNF, (newCache, cnfs, defs) ) = runState trans (cache, [], [])
        
-- -----------------------------------------------------------------------------

formulaToNormalform :: (Ord v, FormulaOperation (GeneralFormula s)) => VarCache v -> GeneralFormula s v -> (VarCache v, NormalForm (Var v))
formulaToNormalform cache form =  runTrans cache' $ transCnf $ demorgen form
    where
        cache' = snd $ newVars cache $ Set.toList $ getLabels form

normalformToCNF :: Eq v => NormalForm (Var v) -> CNF (Var v)
normalformToCNF (or,xor) = or ++ concat (map oddToCNF xor)

formulaToCNF :: (Ord v, FormulaOperation (GeneralFormula s)) => VarCache v -> GeneralFormula s v -> (VarCache v , CNF (Var v))
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
partitionList f (LVar x:xs) = (x:lits, rest) 
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
        checker (All l)  = (True,l)
        checker _         = (False,[])

partitionSome :: [DFormula v] -> ([Lit v], [DFormula v])
partitionSome = partitionList checker
    where
        checker (Some l) = (True,l)
        checker _         = (False,[])

partitionOdd :: [DFormula v] -> ([Lit v], [DFormula v])
partitionOdd = partitionList checker
    where
        checker (Odd l)  = (True,l)
        checker _         = (False,[])

-- _____________________________________________________________

type Env v = Map.Map Integer (Lit v)

lit2ELit :: Lit v -> Lit (Var v)
lit2ELit (Pos x) = Pos $ Right x
lit2ELit (Neg x) = Neg $ Right x

transCnf :: Ord v => DFormula v -> Trans v [Clause (Var v)]
transCnf (LVar (Pos v) ) = return [Or [Pos (Right v)]]
transCnf (LVar (Neg v) ) = return [Or [Neg (Right v)]]

transCnf (All l) = do
    a :: [[Clause (Var v)]] <- mapM transCnf l 
--    addClauses a
    return $ concat a

transCnf (Some l) = do
    let (lits, complexStuff) = partitionSome l
    helpers <- mapM transLit complexStuff
    let lits' = map lit2ELit lits
    return [Or $ lits' ++ helpers]

transCnf (Odd l) = do
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
litOfAnd ds = do
    x :: Lit (Var v) <- freshLit

    -- Define x <-> c1 ∧ ... ∧ cn
    addDefinition (extract x) (All $ map LVar ds) -- Just for printing
    
    addClauses [ Or [neg x, c] | c <- ds ]
    addClause $ Or $ x : [neg c | c <- ds]
    return x

-- | Convert a disjunction of Lits to a single Lit.
litOfOr :: Ord v => [Lit (Var v)] -> Trans v (Lit (Var v))
litOfOr [l] = return l
litOfOr ds = do
    x <- freshLit
    addDefinition (extract x) (Some $ map LVar ds) -- Just for printing
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
    
    let defForPring = case ds of
            (Pos l)      -> map Pos l
            (Neg (x:xs)) -> Neg x : map Pos xs
    addDefinition (extract z) (Odd $ map LVar defForPring) -- Just for printing
    
    -- Define z <-> x1 ⊕ ... ⊕ xn 
    addClause $ XOr $ neg $ ((extract z:) <$> ds)
    return z


