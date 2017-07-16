module SAT.IPASIR.Clauses where

import SAT.IPASIR.Literals
import Control.Arrow
import Control.Comonad
import Data.Maybe


data Clause v      = Or (OrClause v) | XOr (XOrClause v)
    deriving (Show, Eq, Ord)
type OrClause v    = [Lit v]
type XOrClause v   = Lit [v]
type NormalForm v  = ([OrClause v], [XOrClause v])
type CNF v         = [ OrClause v]

getLits :: Clause v -> [Lit v]
getLits ( Or a) = a
getLits (XOr a) = map return $ extract a

partitionClauses :: Bool -> [Clause v] -> ([OrClause v],[XOrClause v])
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
        transform [a] = Just [ const a <$> (fromBool $ sign x) ]
        transform _   = Nothing
        


oddToCNF' :: Bool -> Int -> [[Bool]]
oddToCNF' False 0 = [[]]
oddToCNF' True  0 = []
oddToCNF' positive numberVars = map (False:) positives ++ map (True:) negatives
    where
        negatives = oddToCNF' (not positive) (numberVars-1)
        positives = oddToCNF' positive (numberVars-1)

oddToCNF :: Lit [a] -> [[Lit a]]
oddToCNF xclause = map (zipWith (\v b -> const v <$> fromBool b) vars) bClauses
    where
        bClauses  = oddToCNF' (sign xclause) $ length vars
        vars      = extract xclause

xclausesToCNF :: [Lit [a]] -> [[Lit a]]
xclausesToCNF = concatMap oddToCNF
