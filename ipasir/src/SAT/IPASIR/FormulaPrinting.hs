{-# LANGUAGE KindSignatures, ScopedTypeVariables #-}

module SAT.IPASIR.FormulaPrinting where

import Data.List
import Data.Maybe
import Data.Bits
import Data.List.Split
import Control.Arrow
import qualified Data.List.Split as Split

import SAT.IPASIR.Formula
import SAT.IPASIR.Clauses
import SAT.IPASIR.Literals
import SAT.IPASIR.HelperVarCache

tester1 = Var 1 &&* Not (Var 2) &&* (Var 1 ||* Var 2)
tester2 = Var 1 &&* Var 2 &&* (Var 1 ->* Var 2) &&* Not (Var 2 ->* Var 1) 
tester3 = Var 1 &&* Var 2 &&* (Var 1 ->* Not ((Var 6 &&* Var 4) ->* Var 3) )
tester4 = Var 1 &&* Var 2 &&* (Var 1 ->* Not ((Var 6 <->* Var 4) ->* Var 3) )
tester5 = Var 1 &&* ( Not (Var 2) ||* (Var 1 &&* Var 2))
tester6 = Var 1 &&* ( Not (Var 2) ||* (Var 1 &&* No))
tester7 = Not $ Odd $ map Var "ab" 
tester8 = Odd $ map Var "abc"
tester9 = Odd [ Not $ Var 'a', Not $ Var 'b', Not $Var 'c']
tester0 = Not ( Some [ Some [ No ] , Var 'a', Odd [Var 'a', Var 'b'], Not (All [ Var 'b'] ), Not (Some [ Var 'a', All [Var 'a',Var 'b']]) ] )
tester10 = Not ( Some [ Some [ No ] , Var 'a', Odd [Var 'a', Var 'b'], Not (All [ Var 'b'] ), Not (Some [ Var 'a', Odd [Var 'a', Some [All [Var 'x', Var 'c'], Var 'c']]]) ] )

tester11 = Some [Odd [Some [Var 3, Var 4], Var 2],  Var 1]
tester12 = Some [Odd [Some [Var 3, Var 4], Var 2],  Odd [Some [Var 3, Var 4], Var 2]]

data TransformationStep = TSNormal | TSReduced | TSAfterDemorgen | TSHelperDefs | TSHelperWise | TSHelperCNF | TSXCNF | TSCNF

class TraversableFormula (f :: * -> *) where
    isNegation  :: f v -> Bool
    isList      :: f v -> Bool
    -- Yes and No are also Terminals.
    isTerminal  :: f v -> Bool
    isTerminal x = not (isNegation x) && not (isList x)
    unpackVar   :: f v -> Maybe v
    isVar       :: f v -> Bool
    isVar       = isJust . unpackVar
    getInnerFormulas :: f v -> [f v]
    showElem    :: Show v => f v -> String
    foldFormula :: ( a -> f v -> a) -> a -> f v ->  a
    foldFormula f starter form = foldl (foldFormula f) next $ getInnerFormulas form
        where
            next = f starter form

instance TraversableFormula Formula where
    isNegation (Not _) = True
    isNegation _       = False
    isList (All _)     = True
    isList (Some _)    = True
    isList (Odd _)    = True
    isList _           = False
    getInnerFormulas (Not f)   = [f]
    getInnerFormulas (All l)   = l
    getInnerFormulas (Some l)  = l
    getInnerFormulas (Odd l)  = l
    getInnerFormulas _         = []
    showElem Yes       = "YES  "
    showElem No        = "NO   "
    showElem (All l)   = "ALL  "
    showElem (Some l)  = "SOME "
    showElem (Odd l)   = "ODD  "
    showElem (Not f)   = "-"
    showElem (Var x)   = show x
    unpackVar (Var x)  = Just x
    unpackVar _        = Nothing

instance TraversableFormula DFormula where
    isNegation _       = False
    isList (DAll _)    = True
    isList (DSome _)   = True
    isList (DOdd _)    = True
    isList _           = False
    getInnerFormulas (DAll l)  = l
    getInnerFormulas (DSome l) = l
    getInnerFormulas (DOdd l) = l
    getInnerFormulas _         = []
    showElem (DAll l)  = "ALL  "
    showElem (DSome l) = "SOME "
    showElem (DOdd l)  = "ODD  "
    showElem (DVar x)  = show x
    unpackVar (DVar (Pos x)) = Just x
    unpackVar (DVar (Neg x)) = Just x
    unpackVar _        = Nothing

getVars :: (TraversableFormula f) => f v -> [v]
getVars = foldFormula f []
    where
        f list form
            | isVar form = (fromJust . unpackVar) form : list
            | otherwise  = list

tab = "    "

{-
showFormulaStatistics formula = "Incoming Formula:\n"                      ++ toText part1 ++ 
                                "\nAfter Reduction:\n"                     ++ toText part2 ++ 
                                "\nFinal Form general information:\n"      ++ toText part3 ++
                                "\nIn Final Form (or and xor seperated):\n"++ toText part4 ++
                                "or-clauses:\n"                            ++ toText part5 ++
                                "xor-clauses:\n"                           ++ toText part6 ++
                                "\nIn Final Form (xors transformed):\n"    ++ toText part7 ++ 
                                "clauses:\n"                               ++ toText part8
    where
        toText :: [String] -> String
        toText l = tab ++ intercalate ('\n':tab) l ++ "\n"
        
        part1 = ["Number of different Vars:              " ++ show varsNormalCount, 
                 "Incedence of Vars:                     " ++ show inceNormalCount,
                 "Incedence of Yes/No:                   " ++ show yesNoInce ]
        part2 = ["Number of different Vars:              " ++ show varsReducedCount,
                 "Incedence of Vars:                     " ++ show inceReducedCount,
                 "Number of removed Vars:                " ++ show (varsNormalCount-varsReducedCount),
                 "Number of removed incedences:          " ++ show (inceNormalCount-inceReducedCount)]
        part3 = ["Number of Vars:                        " ++ show varsFinalCount ,
                 "Number of new Vars:                    " ++ show (varsFinalCount - varsReducedCount)]
        part4 = ["Number of or-clauses:                  " ++ show lors,
                 "Number of horn-clauses                 " ++ show lhorn,
                 "Number of xor-clauses:                 " ++ show lxors,
                 "Incedence of Vars in or:               " ++ (show $ sum $ lengthClauses ors),
                 "Incedence of Vars in xor:              " ++ (show $ sum $ lengthClauses xors) ]
        part5 = ["Length "++ show n ++ ":  " ++ show count ++ " clauses with " 
                  ++ show hornCount ++ " of them Horn."
                | n <- [0..maxLength ors], let (count, hornCount) = statsClauses ors n ]
        part6 = ["Length "++ show n ++ ":  " ++ show count ++ " clauses" | n <- [0..maxLength xors], let (count, _) = statsClauses xors n ]
        part7 = ["Number of or-clauses:                  " ++ show (lors + ltrans),
                 "Number of horn-clauses                 " ++ show (lhorn + ltransHorn),
                 "Incedence of Vars:                     " ++ (show $ sum (lengthClauses transformed) + sum (lengthClauses ors) )]
        part8 = ["Length "++ show n ++ ":  " ++ show count ++ " clauses with " 
                  ++ show hornCount ++ " of them Horn."
                | n <- [0..maxLength (transformed++ors)], let (count, hornCount) = statsClauses (transformed++ors) n ]

        lengthClauses c  = map length c
        isHornClauses c  = map isHorn c
        calcedClauses c  = zip (lengthClauses c) (isHornClauses c)
        maxLength        = foldl max 0 . lengthClauses
        statsClauses c n = (length relevant, length $ filter snd relevant )
            where relevant = filter ((==n).fst) $ calcedClauses c
        
        reduced    = rFormula formula
        (ors,xors) = formulaToNormalform formula
        horn       = filter isHorn ors
        transformed= concat $ map oddToCNF xors
        transHorn  = filter isHorn ors
        lors       = length ors
        lxors      = length xors
        lhorn      = length horn
        ltrans     = length transformed
        ltransHorn = length transHorn
        
        isHorn :: [Lit v] -> Bool
        isHorn = (<=1) . length . filter sign

        yesNoInce        = foldFormula yesNoCounter 0 formula
            where
                yesNoCounter n Yes = n+1
                yesNoCounter n No  = n+1
                yesNoCounter n form= n

        varsNormal       = getVars formula
        varsNormalCount  = length $ nub varsNormal
        inceNormalCount  = length $ varsNormal
        varsReduced      = getVars $ reduced
        varsReducedCount = length $ nub varsReduced
        inceReducedCount = length $ varsReduced
        varsFinalCount   = length $ nub $ concat $ ors++xors
-}

showFormulaTransformation :: forall v. (Show v,Ord v) => TransformationStep -> Formula v -> String
showFormulaTransformation TSNormal        formula = showFormula formula
showFormulaTransformation TSReduced       formula = showFormula $ rFormula formula
showFormulaTransformation TSAfterDemorgen formula = showFormula $ demorgen $ rFormula formula
showFormulaTransformation TSXCNF          formula = showFormula normalized
    where
        normalized = normalformToFormula normalform
        normalform = formulaToNormalform formula
showFormulaTransformation TSCNF           formula = showFormula cnf
    where
        cnf        = normalformToFormula normalform
        normalform = (formulaToCNF formula,[])
showFormulaTransformation TSHelperDefs formula = concat elems'''
    where
        cache         = newHelperVarCache Left Right :: HVC v (Either v Word)
        (_,main,defs) = getHelperDefs cache $ demorgen $ rFormula formula
        elems         = ("MAIN: \n", main) : first ( (++" :<=> \n") . show) `map` defs 
        elems'        = second showFormula `map` elems
        elems''       = second ((tab++) . intercalate ('\n':tab) . splitOn "\n") `map` elems'
        elems'''      = map (\e -> '\n' : fst e ++ "\n" ++ snd e ++ "\n") elems''

showFormula :: (TraversableFormula f, Show v) => f v -> String
showFormula = showFormula' tab showElem

showFormulaEither :: Show v => (Integer -> String) -> Formula (Ext v) -> String
showFormulaEither showHelper = showFormula' tab shower
    where
    --    show :: Formula (ELit v) -> String
        shower (Var (Left i))  =  showHelper i
        shower (Var (Right e)) = show e
        shower x                     = showElem x

showFormula' :: forall f v. (TraversableFormula f) =>  String -> (f v -> String) -> f v -> String
showFormula' tab showFunction f
    | isList f     = showFunction f ++ listHelper (getInnerFormulas f)
    | isNegation f = showFunction f ++ showFormula' tab showFunction (head $ getInnerFormulas f)
    | isTerminal f = showFunction f
        where
            listHelper :: [f v] -> String
            listHelper list
                | allLits = "[" ++ concat lines ++ tab ++ "]"
                | otherwise   = "[\n" ++ intercalate "\n" lines ++ "\n]"
                where
                    innerFormsStr     = map (showFormula' tab showFunction) list
                    lines             = map (tab++) $ Split.splitOn "\n" $ intercalate "\n" innerFormsStr
                    allLits           = all isLit list
                    isLit f
                        | isTerminal f        = True
                        | isNegation f        = isLit $ head $ getInnerFormulas f
                        | otherwise           = False

