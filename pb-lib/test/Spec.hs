{-# LANGUAGE TupleSections #-}
module Main (main) where

import qualified Data.Map as Map
import Data.Functor.Identity
import Data.Word
import Data.Proxy
import Data.Bifunctor

import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import SAT.IPASIR hiding (vars)
import SAT.IPASIR.Cryptominisat
import SAT.PseudoBoolean
import SAT.IPASIR.PseudoBoolean

main :: IO ()
main = runSolver cryptoMiniSat doSolve
    where
        doSolve :: StateT (Identity (IpasirSolver CryptoMiniSat String)) IO ()
        doSolve = do
            addClauses $ Odd $ map var ["a", "b", "c", "d", "e"]
            (Identity (conf, sol)) <- minimizeOverVars pbConstraint
            lift $ mapM_ (mapM_ putStrLn . showSolution) sol
            lift $ print conf
            return ()
        pbConstraint :: PBConstraint AtMostK String
        pbConstraint = PBConstraint {
                pbConfig = defaultConfig { config = Just AmkCard },
                wordToVar = \i -> "helper" ++ show i,
                vars = Map.fromList $ map (,1) ["a", "b", "c", "d", "e"],
                comp = CLeq,
                lower = 0,
                upper = 5
            }


showSolution sol = "============" : (pad $ map (bimap showVar showRes) $ Map.toList sol)
    where
        pad :: [(String, String)] -> [String]
        pad ls =  map pad' ls
            where
                pad' (l, r) = take maxL (l ++ repeat ' ') ++ " = " ++ r
                maxL = maximum $ map (length . fst) ls
        showVar (Right v) = v
        showVar (Left i) = "helper" ++ show i
        showRes = maybe "???" show
{-
main :: IO ()
main = evalEncoder (defaultConfig :: Config PseudoBoolean) lits CBoth 5 100 4 $ do
    clauses0 <- getClauses
    clauses1 <- encodeNewGeq 10
    clauses2 <- encodeNewLeq 50
    lift $ print clauses0
    lift $ print clauses1
    lift $ print clauses2
    return ()


lits =
    [ 1 $-$ 5
    , 2 $-$ 1
    , 3 $-$ 5
    ]
-}