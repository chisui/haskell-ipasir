module SAT.IPASIR.VarCache.Debug
    ( showIntToVar
    , showVarToInt
    ) where


import SAT.IPASIR.VarCache
import Control.Monad
import Control.Arrow

import Data.List

showIntToVar :: Show v => VarCache v -> String
showIntToVar lcache = intercalate "\n" (seperator:lines) ++ '\n':seperator
    where
        lastIndex    = numVars lcache
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
showVarToInt :: (Ord v, Show v) => VarCache v -> String
showVarToInt lcache  = seperator ++ '\n' : text ++ '\n' : seperator
    where
        lastIndex    = numVars lcache
        tupels       = [ (i,intToVar lcache i) | i <- [1..lastIndex]]
        (index,var)  = unzip $ sortBy (\a b -> compare (snd a) (snd b)) tupels
        strIndex     = sameSizer False index
        strVars      = sameSizer True var
        line s1 s2   = "| " ++ s1 ++ " - " ++ s2 ++ " |"
        lines        = zipWith line strIndex strVars
        lineLength   = length $ head lines
        seperator    = '+': replicate (lineLength-2) '-' ++ "+"
        text         = intercalate "\n" lines
{-
showTable :: [(String, String)] -> String
showTable rows = unlines $ borderRow : ( map toLine rows >>= (\ l -> [l, borderRow]) )
    where
        toLine (l, r) = "| " ++ padLeft l width0 ++ " - " ++ padRight r width1 ++ " |"
        borderRow = '+' : replicate (width0 + 2) '-' ++ "+"  ++ replicate (width1 + 2) '-' ++ "+"
        (col0, col1) = unzip rows
        width0 = width col0
        width1 = width col1
        width col = maximum (map length col) 0

padLeft, padRight :: String -> Int -> String
(padLeft, padRight) =  \ r w -> (r ++ padding r w, padding r w ++ r)
    where
        padding r w = replicate (w - length r) ' '
-}

sameSizer :: Show a => Bool -> [a] -> [String]
sameSizer left elems
    | left       = zipWith (++) strElems spaces
    | otherwise  = zipWith (++) spaces strElems 
    where
        strElems = map show elems
        maxSize  = foldl max 0 $ map length strElems
        spaces   = map (\e -> replicate (maxSize - length e) ' ') strElems
