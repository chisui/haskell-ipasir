module Main (main) where


import qualified Data.Map as Map

import SAT.IPASIR
import SAT.IPASIR.Cryptominisat
import Data.Bifunctor
import Data.Either
import Data.Maybe
import Data.Functor.Identity
import Control.Monad

main :: IO ()
main = mapM_ (mapM_ putStrLn . showSolution) $ cryptoMiniSat `solveAll` Odd [var "a", var "b", var "c"]

showSolution sol = "============" : (pad $ map (bimap showVar showRes) $ Map.toList sol)
    where
        pad :: [(String, String)] -> [String]
        pad ls =  map pad' ls
            where
                pad' (l, r) = take maxL (l ++ repeat ' ') ++ " = " ++ r
                maxL = maximum $ map length ls
        showVar (Right v) = v
        showVar (Left i) = "helper" ++ show i
        showRes = maybe "???" show