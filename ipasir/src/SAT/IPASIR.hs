module SAT.IPASIR
    ( module Export
    ) where

import SAT.IPASIR.Solver as Export
import SAT.IPASIR.VarCache as Export
    ( Var
    , VarCache
    , emptyCache
    , newVar
    , newVars
    , newHelper
    , newHelpers
    , numVars
    , vars
    , varToInt
    , clausesToInt
    , intToVar
    , showIntToVar
    , showVarToInt
    )
import SAT.IPASIR.Formula as Export 
    ( Formula
    , GeneralFormula
        ( Yes
        , No
        , Not
        , All
        , Some
        , Odd 
        )
    , var
    , notB
    , (&&*)
    , (||*)
    , (++*)
    , (->*)
    , (<->*)
    , formulaToNormalform
    , normalformToCNF
    , formulaToCNF
    , normalformToFormula
    , unpackVar
    , isVar
    , isTerminal
    , asLVar
    , asLit
    )
import SAT.IPASIR.FormulaPrinting as Export
    ( TransformationStep (..)
    , showFormulaStatistics
    , showFormulaTransformation
    , showFormula
    , showFormulaEither
    )
import SAT.IPASIR.IpasirSolver as Export
    ( IpasirSolver(..) )


tester1 = var 1 &&* Not (var 2) &&* (var 1 ||* var 2) 
tester2 = var 1 &&* var 2 &&* (var 1 ->* var 2) &&* Not (var 2 ->* var 1) 
tester3 = var 1 &&* var 2 &&* (var 1 ->* Not ((var 6 &&* var 4) ->* var 3) )
tester4 = var 1 &&* var 2 &&* (var 1 ->* Not ((var 6 <->* var 4) ->* var 3) )
tester5 = var 1 &&* ( Not (var 2) ||* (var 1 &&* var 2))
tester6 = var 1 &&* ( Not (var 2) ||* (var 1 &&* No))
tester7 = Not $ Odd $ map var "ab"
tester8 = Odd $ map var "abc"
tester9 = Odd [ Not $ var 'a', Not $ var 'b', Not $ var 'c']
tester0 = Not ( Some [ Some [ No ] , var 'a', Odd [var 'a', var 'b'], Not (All [ var 'b'] ), Not (Some [ var 'a', All [var 'a',var 'b']]) ] )
tester10 = Not ( Some [ Some [ No ] , var 'a', Odd [var 'a', var 'b'], Not (All [ var 'b'] ), Not (Some [ var 'a', Odd [var 'a', Some [All [var 'x', var 'c'], var 'c']]]) ] ) 

tester11 = Some [Odd [Some [var 3, var 4], var 2],  var 1]
tester12 = Some [Odd [Some [var 3, var 4], var 2],  Odd [Some [var 3, var 4], var 2]]
