module SAT.IPASIR.Clauses where

import SAT.IPASIR.Literals
import Control.Arrow
import Control.Comonad
import Data.Maybe

-- |Represents one clause in a XCNF
data Clause v      = Or (OrClause v) | XOr (XOrClause v)
    deriving (Show, Eq, Ord)
-- |Represents one clause in a CNF. That means all literals are concatenated by logical or \\( \\vee \\)
type OrClause v    = [Lit v]
-- |Represents one xclause in a XCNF. That means all variables are concatenated by logical exclusive or \\( \\oplus \\)
--  The sign can negate the whole clause.
type XOrClause v   = Lit [v]
-- |Represents a XCNF
type NormalForm v  = ([OrClause v], [XOrClause v])
-- |Represents a CNF
type CNF v         = [ OrClause v]

-- |gives all litarals inside of a clause. The literals are positive if its a xclause.
getLits :: Clause v -> [Lit v]
getLits ( Or a) = a
getLits (XOr a) = map return $ extract a

{- |Transforms a list of clauses into a 'NormalForm'. The boolean transforms the clauses of length one.

        * if the boolean is @True@,  then the clauses of length one will be an or-clause.
        * if the boolean is @False@, then the clauses of length one will be a xor-clause.
-}
partitionClauses :: Bool -> [Clause v] -> NormalForm v
partitionClauses _     []        = ([],[])
partitionClauses True  (Or x:xs) = first (x:) $ partitionClauses True xs
partitionClauses False (XOr x:xs)= second (x:) $ partitionClauses False xs
partitionClauses _ (Or [x]:xs)   = second ((return <$> x) :) $ partitionClauses False xs  -- ( ors, (return <$> x) : xors)
partitionClauses _ (Or x:xs)     = first (x:) $ partitionClauses False xs
partitionClauses _ ((XOr x):xs)          
    | isJust transformed = first (fromJust transformed:) $ partitionClauses True xs
    | otherwise          = second (x:) $ partitionClauses True xs
    where
        transformed    = transform vars
        vars = extract x
        transform :: [v] -> Maybe [Lit v]
        transform []  = Just []
        transform [a] = Just [ const a <$> (fromBool $ isPositive x) ]
        transform _   = Nothing
        
{- |Generates a CNF, which is equivalent to a xclause. The First parameter tells if the 
    xclause is nagated, the second is the length.

    The cnf will be constructed by the rule:

    The trivial case ís \\( \\bigoplus_{x\\in\\varnothing} x \\equiv False \\). And for all \\(n>0\\)$:

$$
\\bigoplus_{i=1}^{n}x_{i}	\\equiv x_{n}\\oplus\\bigoplus_{i=1}^{n-1}x_{i}\\equiv\\left(\\lnot x_{n}\\vee\\bigoplus_{i=1}^{n-1}x_{i}\\right)\\wedge\\left(x_{n}\\vee\\lnot\\bigoplus_{i=1}^{n-1}x_{i}\\right)
$$

respectivly

$$\\lnot\\bigoplus_{i=1}^{n}x_{i}\\equiv\\lnot x_{n}\\oplus\\bigoplus_{i=1}^{n-1}x_{i}\\equiv\\left(x_{n}\\vee\\bigoplus_{i=1}^{n-1}x_{i}\\right)\\wedge\\left(\\lnot x_{n}\\vee\\lnot\\bigoplus_{i=1}^{n-1}x_{i}\\right)$$
-}
oddToCNF' :: Bool -> Int -> [[Bool]]
oddToCNF' False 0 = [[]]
oddToCNF' True  0 = []
oddToCNF' positive numberVars = map (False:) positives ++ map (True:) negatives
    where
        negatives = oddToCNF' (not positive) (numberVars-1)
        positives = oddToCNF' positive (numberVars-1)

-- |Transforms a xclause into a CNF with same table of truth. A xclause of length \\(n\\) will
--  result in a CNF with \\( 2^{n-1} \\) clauses of length \\(n\\).
oddToCNF :: XOrClause a -> CNF a
oddToCNF xclause = map (zipWith (\v b -> const v <$> fromBool b) vars) bClauses
    where
        bClauses  = oddToCNF' (isPositive xclause) $ length vars
        vars      = extract xclause

-- |Uses 'oddToCNF' and concatenate the results. That means the resulting CNF has the same table of truth
--  as the cunjucntion of all xclauses. 
xclausesToCNF :: [XOrClause a] -> CNF a
xclausesToCNF = concatMap oddToCNF
