{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module SAT.IPASIR.Formula where

import Data.Maybe
import Data.List
import Control.Monad
import Prelude hiding (all)
import qualified Data.Map as Map

import SAT.IPASIR.Literals


data Formula v 
  = Var v                     -- ^ A variable.
  | Yes                       -- ^ The formula /true/.
  | No                        -- ^ The formula /false/.
  | Not (Formula v)           -- ^ Negation.
  | All  [Formula v]          -- ^ All are true.
  | Some [Formula v]          -- ^ At least one is true.
  | Even [Formula v]          -- ^ An even number is /true/.
  deriving (Show, Eq, Ord)

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

(Even l1) ++* (Even l2) = Even $ l1++l2
(Even l) ++* a = Even $ a:l
a ++* (Even l) = Even $ a:l
a ++* b        = Even [a,b]

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
rFormula (Even l)
    | null reducedList  = Yes
    | positive          = Even reducedList
    | otherwise         = Even $ notB (head reducedList) : tail reducedList
    where 
        newForms            = map rFormula l
        (trash,reducedList) = partition isTerminal newForms
        positive            = even $ length $ filter ((==Yes).rFormula) trash
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
    | DEven [DFormula v]
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
        pdemorgen (Even f) = DEven $ map pdemorgen f
        
        ndemorgen :: Formula v -> DFormula v
        ndemorgen (Var x)  = DVar $ Neg x
        ndemorgen (Not f)  = pdemorgen f
        ndemorgen (All f)  = DSome $ map ndemorgen f
        ndemorgen (Some f) = DAll  $ map ndemorgen f
        ndemorgen (Even (x:xs)) = DEven $ map pdemorgen $ notB x : xs

evenToCNF :: [Lit v] -> [[Lit v]]
evenToCNF clause = do
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
    
-- ----------------------------------------------------------------------
-- * A monad for translation to CNF

-- | A monad for translation to CNF. This monad keeps track of two kinds
-- of state: an integer counter to provide a supply of fresh
-- variables, and a list of definitional clauses.
data Trans v a = Trans (Integer -> (a, Integer, [Clause v] ))

instance Monad (Trans v) where
  return a = Trans (\n -> (a, n, []))
  (Trans f) >>= g = Trans (\n ->
                            let (a1, n1, l1) = f n in
                            let Trans h = g a1 in
                            let (a2, n2, l2) = h n1 in
                            (a2, n2, l1 ++ l2))
  
instance Applicative (Trans v) where
  pure = return
  (<*>) = ap
  
instance Functor (Trans v) where
  fmap = liftM

-- | Run the 'Trans' monad.
runTrans :: Trans v a -> (a, [Clause v])
runTrans (Trans f) = (a, clauses)
  where
    (a, _, clauses) = f 0


formulaToNormalform :: Eq v => Formula v -> ([[ELit v]], [[ELit v]])
formulaToNormalform form = (or, xor)
    where
        (rest, clauses)   = runTrans $ transCnf $ demorgen $ rFormula form
        (or1,xor1,both1)  = partitionClauses rest
        (or2,xor2,both2)  = partitionClauses clauses
        or                = map getLits $ or1  ++  or2 ++ both1 ++ both2
        xor               = map getLits $ xor1 ++ xor2

normalformToCNF :: Eq v => ([[ELit v]], [[ELit v]]) -> [[ELit v]]
normalformToCNF (or,xor) = or ++ concat (map evenToCNF xor)

formulaToCNF :: Eq v => Formula v -> [[ELit v]]
formulaToCNF = normalformToCNF . formulaToNormalform
   
normalformToFormula :: ([[ELit v]], [[ELit v]]) -> Formula (ELit v)
normalformToFormula (or,xor)   = All $ orFormulas ++ xorFormulas
    where
        orFormulas        = [ Some $ map Var lits | lits <-  or]
        xorFormulas       = [ Even $ map Var lits | lits <- xor]
        signedVar (Pos x) = Var x
        signedVar (Neg x) = Not $ Var x

-- | Return a fresh Lit.
freshLit :: Trans v (ELit v)
freshLit = Trans (\n -> (Pos (Left n), n+1, []))

-- | Add one clause.
addClause :: Clause v -> Trans v ()
addClause clause = addClauses [clause]

-- | Add some clauses.
addClauses :: forall v. [Clause v] -> Trans v ()
addClauses clauses = Trans (\n -> ((), n, ors ++ xors))
    where
        (orClauses, xorClauses, oneLitClauses) = partitionClauses clauses
        ors  = [ Or  $ getLits x | x <- orClauses ]
        xors = [ XOr $ getLits x | x <- xorClauses++oneLitClauses ]

addCnf :: [[ELit v]] -> Trans v ()
addCnf cs = Trans (\n -> ((), n, map Or cs))

addXnf :: [[ELit v]] -> Trans v ()
addXnf cs = Trans (\n -> ((), n, map XOr cs))

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


partitionSome :: [DFormula v] -> ([Lit v], [DFormula v])
partitionSome = partitionList checker
    where
        checker (DSome l) = (True,l)
        checker _         = (False,[])

partitionEven :: [DFormula v] -> ([Lit v], [DFormula v])
partitionEven = partitionList checker
    where
        checker (DEven l) = (True,l)
        checker _         = (False,[])

partitionClauses :: [Clause v] -> ([Clause v],[Clause v],[Clause v])
partitionClauses []     = ([],[],[])
partitionClauses (x:xs)
    | isOr x && isXOr x = (ors, xors, x:both)
    | isOr x            = (x:ors, xors, both)
    |           isXOr x = (ors, x:xors, both)
    where
        (ors, xors, both) = partitionClauses xs
        isOr :: Clause v -> Bool
        isOr (XOr [x]) = True
        isOr (Or _)    = True
        isOr _         = False
        isXOr :: Clause v -> Bool
        isXOr (Or [x]) = True
        isXOr (XOr _)  = True
        isXOr _        = False


-- _____________________________________________________________

type Env v = Map.Map Integer (Lit v)

lit2ELit :: Lit v -> ELit v
lit2ELit (Pos x) = Pos $ Right x
lit2ELit (Neg x) = Neg $ Right x

data Clause v = Or [ELit v] | XOr [ELit v]
    deriving (Show, Eq, Ord)

getLits :: Clause v -> [ELit v]
getLits ( Or a) = a
getLits (XOr a) = a

transCnf :: DFormula v -> Trans v [Clause v]
transCnf (DVar (Pos v) ) = return [Or [Pos (Right v)]]
transCnf (DVar (Neg v) ) = return [Or [Neg (Right v)]]

transCnf (DAll l) = do
    a :: [[Clause v]] <- mapM transCnf l 
--    addClauses a
    return $ concat a

transCnf (DSome l) = do
    let (lits, complexStuff) = partitionSome l
    helpers <- mapM transLit complexStuff
    let lits' = map lit2ELit lits
    return [Or $ lits' ++ helpers]

transCnf (DEven l) = do
    let (lits, complexStuff) = partitionEven l
    helpers <- mapM transLit complexStuff
    let lits' = map lit2ELit lits
    return [XOr $ lits' ++ helpers]

transLit a = do
    cnf    <- transCnf a
    litOfNormalForm cnf

-- | Convert a CNF to a single Lit.
litOfNormalForm :: forall v. [Clause v] -> Trans v (ELit v)
litOfNormalForm clauses = do
    let (ors, xors, both) = partitionClauses clauses
    let orLits  = map getLits ors              :: [[ELit v]]
    let xorLits = map getLits $ xors ++ both   :: [[ELit v]]
  
    orHelper  :: [ELit v] <- mapM litOfOr orLits
    xorHelper :: [ELit v] <- mapM litOfXor xorLits

    litOfAnd $ orHelper ++ xorHelper

-- | Convert a CNF to a single Lit.
{-lit_of_cnf :: [[ELit v]] -> Trans v (ELit v)
lit_of_cnf ds = do
  xs <- sequence (map litOfOr ds)
  y <- litOfand xs
  return y
-}

-- | Convert a conjunction of Lits to a single Lit.
litOfAnd :: [ELit v] -> Trans v (ELit v)
litOfAnd [l] = return l
litOfAnd cs = do
    x <- freshLit
    -- Define x <-> c1 ∧ ... ∧ cn
    addCnf [[neg x, c] | c <- cs ]
    addCnf [x : [neg c | c <- cs]]
    return x

-- | Convert a disjunction of Lits to a single Lit.
litOfOr :: [ELit v] -> Trans v (ELit v)
litOfOr [l] = return l
litOfOr ds = do
    x <- freshLit
    -- Define x <-> d1 ∨ ... ∨ dn
    addCnf [ [x, neg d] | d <- ds]
    addCnf [neg x : ds]
    return x

-- | Convert an exclusive or of two Lits to a single Lit.
litOfXor :: [ELit v] -> Trans v (ELit v)
litOfXor [l] = return l
litOfXor ds = do
    z <- freshLit
    -- Define z <-> x1 ⊕ ... ⊕ xn 
    addXnf [neg z : ds]
    return z


