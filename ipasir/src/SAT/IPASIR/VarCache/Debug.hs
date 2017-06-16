module SAT.IPASIR.VarCache.Debug where


import SAT.IPASIR.VarCache

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

sameSizer :: Show a => Bool -> [a] -> [String]
sameSizer left elems
    | left       = zipWith (++) strElems spaces
    | otherwise  = zipWith (++) spaces strElems 
    where
        strElems = map show elems
        maxSize  = foldl max 0 $ map length strElems
        spaces   = map (\e -> replicate (maxSize - length e) ' ') strElems
