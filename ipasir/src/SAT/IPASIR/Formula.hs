{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SAT.IPASIR.Formula where

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


type  Formula v = GeneralFormula Normal v
type RFormula v = GeneralFormula Reduced v
type DFormula v = GeneralFormula Demorganed v

data Normal
data Reduced
data Demorganed

class Upper a where
instance Upper Normal where
instance Upper Reduced where

data GeneralFormula s v where 
    -- | A variable.
    Var  :: Upper s => v -> GeneralFormula s v
    -- | A Positive variable.
    PVar :: v -> GeneralFormula Demorganed v
    -- | A Negative variable.
    NVar :: v -> GeneralFormula Demorganed v
    -- | The formula @True@.
    Yes  :: GeneralFormula Normal v
    -- | The formula @False@.
    No   :: GeneralFormula Normal v
    -- | Negation.
    Not  :: Upper s => GeneralFormula s v -> GeneralFormula s v
    -- | All are @True@.
    All  :: [GeneralFormula s v] -> GeneralFormula s v
    -- | At least one is @True@.
    Some :: [GeneralFormula s v] -> GeneralFormula s v
    -- | An odd number is @True@.
    Odd  :: [GeneralFormula s v] -> GeneralFormula s v

deriving instance Show v => Show        (GeneralFormula s v)
deriving instance Ord v  => Ord         (GeneralFormula s v)
deriving instance Eq v   => Eq          (GeneralFormula s v)
deriving instance           Functor     (GeneralFormula s)
deriving instance           Foldable    (GeneralFormula s)
deriving instance           Traversable (GeneralFormula s)

instance FormulaOperation s => Applicative (GeneralFormula s) where
    pure  = return
    (<*>) = ap
instance FormulaOperation s => Monad (GeneralFormula s) where
    return = makeVar
    (>>=) (Var  v)  f = f v
    (>>=) (PVar v)  f = f v
    (>>=) (NVar v)  f = notB $ f v
    (>>=) (Not  v)  f = notB $ v >>= f
    (>>=) (All  vs) f = All  $ map (>>= f) vs
    (>>=) (Some vs) f = Some $ map (>>= f) vs
    (>>=) (Odd  vs) f = Odd  $ map (>>= f) vs
    (>>=) Yes       _ = Yes
    (>>=) No        _ = No

--fBind :: FormulaOperation s => GeneralFormula s a -> (a ->  GeneralFormula s b) -> GeneralFormula s b

instance (IsString v) => IsString (Formula v) where
    fromString = return . fromString

instance (Ord v, FormulaOperation s) => HasVariables (GeneralFormula s v) where
    type VariableType (GeneralFormula s v) = v
    getAllVariables f vc = map extract $ concat $ snd $ formulaToCNF vc f
    getVariables    f vc = Set.map Left (getHelpers f vc) `Set.union` Set.map Right (getLabels f)
    getLabels            = Set.fromList . toList
    getHelpers      f _  = Set.fromList $ lefts $ map fst defs
        where
            (_,_,_,defs) = runTransComplete emptyCache $ transCnf $ demorgan f

unpackVar :: GeneralFormula s v -> Maybe v
unpackVar (Var  v) = Just v
unpackVar (PVar v) = Just v
unpackVar (NVar v) = Just v
unpackVar _        = Nothing

isVar :: GeneralFormula s v -> Bool
isVar = isJust . unpackVar

isTerminal :: GeneralFormula s v -> Bool
isTerminal Yes = True
isTerminal No  = True
isTerminal f   = isVar f

asLVar :: Lit v -> DFormula v
asLVar (Pos v) = PVar v
asLVar (Neg v) = NVar v

asLit :: DFormula v -> Lit v
asLit (PVar v) = Pos v
asLit (NVar v) = Neg v

var :: v -> Formula v
var = return

(&&*) :: GeneralFormula s v -> GeneralFormula s v -> GeneralFormula s v
Yes &&* Yes = Yes
No  &&* _   = No
_   &&* No  = No
l   &&* r   = All $ list l ++ list r
    where
        list (All x) = x
        list Yes     = []
        list x       = [x]

(||*) :: GeneralFormula s v -> GeneralFormula s v -> GeneralFormula s v
Yes ||* _   = Yes
_   ||* Yes = Yes
l   ||* r   = Some $ list l ++ list r
    where
        list (Some x) = x
        list No       = []
        list x        = [x]

(++*) :: GeneralFormula s v -> GeneralFormula s v -> GeneralFormula s v
l ++* r = Odd $ list l ++ list r
    where
        list (Odd x) =  x
        list x       = [x]

a  ->* b = notB a   ||* b
a <->* b = notB $ a ++* b

infixl 6 &&*
infixl 5 ||*
infixl 4 ++*
infixl 3 ->*
infixl 1 <->*

class (Foldable (GeneralFormula s), Traversable (GeneralFormula s)) => FormulaOperation s where
    -- | create a var
    makeVar :: v -> GeneralFormula s v
    -- | Removes all occurances of @Yes@ and @No@ from the Formulas.
    rFormula :: Eq v => GeneralFormula s v -> RFormula v
    -- | Push all occurances of @Not@ down to the variables.
    demorgan :: Eq v => GeneralFormula s v -> DFormula v
    -- | Negate a formula.
    notB :: GeneralFormula s v -> GeneralFormula s v

instance FormulaOperation Normal where
    makeVar = Var
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
    demorgan = demorgan . rFormula
    notB (Not x) = x
    notB f       = Not f

instance FormulaOperation Reduced where
    makeVar = Var
    rFormula = id
    demorgan form = pdemorgan form
        where
            pdemorgan :: RFormula v -> DFormula v
            pdemorgan (Var x)  = PVar x
            pdemorgan (Not f)  = ndemorgan f
            pdemorgan (All f)  = All  $ map pdemorgan f
            pdemorgan (Some f) = Some $ map pdemorgan f
            pdemorgan (Odd f)  = Odd $ map pdemorgan f
            ndemorgan :: RFormula v -> DFormula v
            ndemorgan (Var x)  = NVar x
            ndemorgan (Not f)  = pdemorgan f
            ndemorgan (All f)  = Some $ map ndemorgan f
            ndemorgan (Some f) = All  $ map ndemorgan f
            ndemorgan (Odd (x:xs)) = Odd $ map pdemorgan $ notB x : xs
    notB (Not x) = x
    notB f       = Not f

instance FormulaOperation Demorganed where
    makeVar = PVar
    rFormula (PVar x) = Var x
    rFormula (NVar x) = Not $ Var x
    rFormula (All  l) = All  $ map rFormula l
    rFormula (Some l) = Some $ map rFormula l
    rFormula (Odd  l) = Odd  $ map rFormula l
    demorgan = id
    notB (PVar x) = NVar x
    notB (NVar x) = PVar x
    notB (All  l) = Some $ map notB l
    notB (Some l) = All  $ map notB l
    notB (Odd (x:xs)) = Odd $ notB x : xs

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

formulaToNormalform :: (Ord v, FormulaOperation s) => VarCache v -> GeneralFormula s v -> (VarCache v, NormalForm (Var v))
formulaToNormalform cache form =  runTrans cache' $ transCnf $ demorgan form
    where
        cache' = snd $ newVars cache $ Set.toList $ getLabels form

normalformToCNF :: Eq v => NormalForm (Var v) -> CNF (Var v)
normalformToCNF (or,xor) = or ++ concat (map oddToCNF xor)

formulaToCNF :: (Ord v, FormulaOperation s) => VarCache v -> GeneralFormula s v -> (VarCache v , CNF (Var v))
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
partitionList f [] = ([],[])
partitionList f (x:xs)
    | isVar x  = (lit x:lits2, rest2)
    | correctType  = (lits1++lits2, rest1++rest2) 
    | otherwise    = (lits2, x:rest2) 
    where
        lit :: DFormula v -> Lit v
        lit (PVar v) = Pos v
        lit (NVar v) = Neg v
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
transCnf (PVar v) = return [Or [Pos (Right v)]]
transCnf (NVar v) = return [Or [Neg (Right v)]]

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
    addDefinition (extract x) (All $ map asLVar ds) -- Just for printing
    
    addClauses [ Or [neg x, c] | c <- ds ]
    addClause $ Or $ x : [neg c | c <- ds]
    return x

-- | Convert a disjunction of Lits to a single Lit.
litOfOr :: Ord v => [Lit (Var v)] -> Trans v (Lit (Var v))
litOfOr [l] = return l
litOfOr ds = do
    x <- freshLit
    addDefinition (extract x) (Some $ map asLVar ds) -- Just for printing
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
    addDefinition (extract z) (Odd $ map asLVar defForPring) -- Just for printing
    
    -- Define z <-> x1 ⊕ ... ⊕ xn 
    addClause $ XOr $ neg $ ((extract z:) <$> ds)
    return z


