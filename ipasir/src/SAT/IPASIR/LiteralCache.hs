{-# LANGUAGE RankNTypes, KindSignatures, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
module SAT.IPASIR.LiteralCache where

import qualified Data.Map    as Map
import qualified Data.Vector as Vec

import Control.Comonad
import Data.List
import SAT.IPASIR.Literals


class (Ord v) => LiteralCache lc v where
    emptyCache :: lc v
    insertVar  :: lc v ->  v  -> lc v
    insertVars :: lc v -> [v] -> lc v
    insertVars = foldl insertVar 
    numVars    :: Enum e => lc v -> e
    intToVar   :: Enum e => lc v -> e  -> v
    varToInt   :: Enum e => lc v -> v -> e
    clausesToIntClauses :: Enum e => lc v -> [[Lit v]] -> (lc v, [[Lit e]])
    clausesToIntClauses lcache clauses = (lcache', newClauses)
        where
            lcache' = insertVars lcache vars
            vars = extract `map` concat clauses
            newClauses = (map.map.(<$>)) (varToInt lcache') clauses

    showIntToVar :: Show v =>  lc v -> String
    showIntToVar lcache = intercalate "\n" (seperator:lines) ++ '\n':seperator
        where
            lastIndex    = numVars lcache :: Int 
            indices      = [1..lastIndex]
            lengthStrInd = length $ show lastIndex
            strIndices   = map resizeIndex indices
            resizeIndex i= replicate (lengthStrInd - length (show i)) ' ' ++ show i
            vars         = map (intToVar lcache) indices
            lengthStrVar = foldl max 0 $ map (length.show) vars
            strVars      = map (take lengthStrVar.(++ repeat ' ').show) vars
            line s1 s2   = "| " ++ s1 ++ " - " ++ s2 ++ " |"
            lines        = zipWith line strIndices strVars
            lineLength   = length $ head lines
            seperator    = '+': replicate (lineLength-2) '-' ++ "+"
    showVarToInt :: (Show v) => lc v -> String
    showVarToInt lcache  = seperator ++ '\n' : text ++ '\n' : seperator
        where
            lastIndex    = numVars lcache :: Int 
            tupels       = [ (i,intToVar lcache i) | i <- [1..lastIndex]]
            (index,var)  = unzip $ sortBy (\a b -> compare (snd a) (snd b)) tupels
            strIndex     = sameSizer False index
            strVars      = sameSizer True var
            line s1 s2   = "| " ++ s1 ++ " - " ++ s2 ++ " |"
            lines        = zipWith line strIndex strVars
            lineLength   = length $ head lines
            seperator    = '+': replicate (lineLength-2) '-' ++ "+"
            text         = intercalate "\n" lines
    
sameSizer :: Show a => Bool -> [a] -> [String]
sameSizer left elems
    | left       = zipWith (++) strElems spaces
    | otherwise  = zipWith (++) spaces strElems 
    where
        strElems = map show elems
        maxSize  = foldl max 0 $ map length strElems
        spaces   = map (\e -> replicate (maxSize - length e) ' ') strElems

data LitCache v = LitCache (Vec.Vector v) (Map.Map v Int)
    deriving (Show)

instance (Ord v) => LiteralCache LitCache v where
    emptyCache = LitCache Vec.empty Map.empty
    insertVar cache var
        | elem var vector = cache 
        | otherwise       = LitCache newVector newMap
        where
            LitCache vector map = cache
            newVector = Vec.snoc vector var
            newMap    = vectorToMap newVector
            
    numVars (LitCache vec _) = toEnum $ length vec
    
    intToVar (LitCache vec _) enum = vec Vec.! index
        where
            index = fromEnum enum - 1
    varToInt (LitCache _ map) var  = toEnum $ (+1) $ map Map.! var

vectorToMap :: (Ord v) => Vec.Vector v -> Map.Map v Int    
vectorToMap vec = Map.fromList $ zip (Vec.toList vec) [0..]


