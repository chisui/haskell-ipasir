{-# LANGUAGE RankNTypes, KindSignatures, ScopedTypeVariables #-}
module SAT.IPASIR.LiteralCache where

import qualified Data.Map    as Map
import qualified Data.Vector as Vec
import Data.List

class LiteralCache (a :: * -> *) where
    emptyCache :: a l
    insertVar  :: Ord l => a l -> l -> a l
    insertVars :: Ord l => a l -> [l] -> a l
    insertVars = foldl insertVar 
    numVars    :: Enum e => a l -> e
    intToVar   :: Enum e => a l -> e -> l
    varToInt   :: (Ord l,Enum e) => a l -> l -> e
    showIntToVar :: Show b =>  a b -> String
    showIntToVar lcache = intercalate "\n" (seperator:lines) ++ '\n':seperator
        where
            lastIndex    = numVars lcache - 1 :: Int 
            indices      = [0..lastIndex]
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
    showVarToInt :: (Ord b,Show b) => a b -> String
    showVarToInt lcache  = seperator ++ '\n' : text ++ '\n' : seperator
        where
            lastIndex    = numVars lcache - 1 :: Int 
            tupels       = [ (i,intToVar lcache i) | i <- [0..lastIndex]]
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

instance LiteralCache LitCache where
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
            index = fromEnum enum
            
    varToInt (LitCache _ map) var  = toEnum $ map Map.! var

vectorToMap :: (Ord v) => Vec.Vector v -> Map.Map v Int    
vectorToMap vec = Map.fromList $ zip (Vec.toList vec) [0..]
