{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Spec where


import qualified Data.Map as Map

import SAT.IPASIR
import SAT.IPASIR.Api
import SAT.IPASIR.Minicard
import SAT.IPASIR.Minicard.C
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

showSols ptr iList = mapM_ (\x -> putStr =<< ( (++"    ") <$> show <$> ipasirVal ptr x)) iList


testCards = do
    ptr <- ipasirInit :: IO Minicard
    let iList = [1..6]
    let list = map Pos iList
    ipasirAddClause ptr list
    firstSolution <- ipasirSolve ptr
    putStrLn $ "B FirstSolution: " ++ show firstSolution
    showSols ptr iList
    putStrLn ""

    addAtMostK ptr (map Neg iList) 1
    secondSolution <- ipasirSolve ptr
    putStrLn $ "B SecondSolution: " ++ show secondSolution
    showSols ptr iList
    putStrLn ""

    addAtMostK ptr (map Neg iList) 0
    thirdSolution <- ipasirSolve ptr
    putStrLn $ "B ThirdSolution: " ++ show thirdSolution
    showSols ptr iList
    putStrLn ""
    

    return ptr

