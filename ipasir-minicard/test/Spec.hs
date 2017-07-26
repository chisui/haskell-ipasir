--module Main (main) where
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

import qualified Data.Map as Map

import SAT.IPASIR
import SAT.IPASIR.Minicard
import Data.Bifunctor
import Data.Either
import Data.Maybe
import Data.Functor.Identity
import Control.Monad

--main :: IO ()
showAllSolutions f = mapM_ (mapM_ putStrLn . showSolution) $ minicard `solveAll` f

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

nvar = notB . var

formula1 = All [var "a", var "b", var "c"]
formula2 = All [Some [var "a",  var "b"]]
formula3 = All [Some [var "a", nvar "b"]]
formula4 = Odd [var "a", var "b"]

