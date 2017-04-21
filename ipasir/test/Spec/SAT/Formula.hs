module Spec.SAT.Formula where

-- ----------------------------------------------------------------------
-- * Boolean formulas

tester1 = Var 1 &&* Not (Var 2) &&* (Var 1 ||* Var 2)
tester2 = Var 1 &&* Var 2 &&* (Var 1 ->* Var 2) &&* Not (Var 2 ->* Var 1) 
tester3 = Var 1 &&* Var 2 &&* (Var 1 ->* Not ((Var 6 &&* Var 4) ->* Var 3) )
tester4 = Var 1 &&* Var 2 &&* (Var 1 ->* Not ((Var 6 <->* Var 4) ->* Var 3) )
tester5 = Var 1 &&* ( Not (Var 2) ||* (Var 1 &&* Var 2))

spliter = "-----------------------------------------------"
putForTest :: (Show v, Eq v) => Formula v -> IO()
putForTest formula = do
    putStrLn spliter
    putStrLn "XXX INPUT XXX"
    makeFormulaGreatAgain formula
    let formula' = rFormula formula
    putStrLn spliter
    putStrLn "XXX REDUCED FORMULA XXX"
    makeFormulaGreatAgain formula'
    let formula'' = demorgen formula'
    putStrLn spliter
    putStrLn "XXX AFTER DEMORGEN XXX"
    makeFormulaGreatAgain formula''
    let formula''' = transToFormula $ trans_cnf formula''
    putStrLn spliter
    putStrLn "XXX LAST RESULT XXX"
    makeFormulaGreatAgain formula'''
    putStrLn spliter
