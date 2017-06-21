{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module SAT.IPASIR.FormulaPrinting where

import Data.List
import Data.Maybe
import Data.Bits
import Data.List.Split
import Control.Comonad
import Data.Foldable
import qualified Data.List.Split as Split
import qualified Data.Set as Set

import Control.Monad.Trans.State

import SAT.IPASIR.Formula
import SAT.IPASIR.Clauses
import SAT.IPASIR.Literals
import SAT.IPASIR.VarCache
import SAT.IPASIR.Solver

tester1 = Var 1 &&* Not (Var 2) &&* (Var 1 ||* Var 2) :: Formula Int
tester2 = Var 1 &&* Var 2 &&* (Var 1 ->* Var 2) &&* Not (Var 2 ->* Var 1) :: Formula Int
tester3 = Var 1 &&* Var 2 &&* (Var 1 ->* Not ((Var 6 &&* Var 4) ->* Var 3) ):: Formula Int
tester4 = Var 1 &&* Var 2 &&* (Var 1 ->* Not ((Var 6 <->* Var 4) ->* Var 3) ):: Formula Int
tester5 = Var 1 &&* ( Not (Var 2) ||* (Var 1 &&* Var 2)):: Formula Int
tester6 = Var 1 &&* ( Not (Var 2) ||* (Var 1 &&* No)):: Formula Int
tester7 = Not $ Odd $ map Var "ab"  :: Formula Char
tester8 = Odd $ map Var "abc" :: Formula Char
tester9 = Odd [ Not $ Var 'a', Not $ Var 'b', Not $Var 'c'] :: Formula Char
tester0 = Not ( Some [ Some [ No ] , Var 'a', Odd [Var 'a', Var 'b'], Not (All [ Var 'b'] ), Not (Some [ Var 'a', All [Var 'a',Var 'b']]) ] ) :: Formula Char
tester10 = Not ( Some [ Some [ No ] , Var 'a', Odd [Var 'a', Var 'b'], Not (All [ Var 'b'] ), Not (Some [ Var 'a', Odd [Var 'a', Some [All [Var 'x', Var 'c'], Var 'c']]]) ] ) 

tester11 = Some [Odd [Some [Var 3, Var 4], Var 2],  Var 1] :: Formula Int
tester12 = Some [Odd [Some [Var 3, Var 4], Var 2],  Odd [Some [Var 3, Var 4], Var 2]] :: Formula Int

data TransformationStep = TSNormal | TSReduced | TSDemorgen | TSHelperDefs | TSHelperForm | TSXCNF | TSCNF

    -- Yes and No are also Terminals.
isTerminal  :: GeneralFormula s v -> Bool
isTerminal x = not (isNegation x) && not (isList x)
isVar       :: GeneralFormula s v -> Bool
isVar       = isJust . unpackVar
foldFormula :: ( a -> GeneralFormula s v -> a) -> a -> GeneralFormula s v ->  a
foldFormula f starter form = foldl (foldFormula f) next $ getInnerFormulas form
    where
        next = f starter form
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
showElem (LVar (Pos x)) = '+' : show x
showElem (LVar (Neg x)) = '-' : show x
unpackVar (Var x)  = Just x
unpackVar (LVar x) = Just $ extract x
unpackVar _        = Nothing

getVars :: GeneralFormula s v -> [v]
getVars = foldFormula f []
    where
        f list form
            | isVar form = (fromJust . unpackVar) form : list
            | otherwise  = list

tab = "    "

showFormulaStatistics :: (Ord v) => Formula v -> String
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
                 "Occurences of Vars:                    " ++ show occNormalCount,
                 "Occurences of Yes/No:                  " ++ show yesNoOccCount ]
        part2 = ["Number of different Vars:              " ++ show varsReducedCount,
                 "Occurences of Vars:                    " ++ show occReducedCount,
                 "Number of removed Vars:                " ++ show (varsNormalCount-varsReducedCount),
                 "Number of removed occurences:          " ++ show (occNormalCount-occReducedCount)]
        part3 = ["Number of Vars:                        " ++ show varsFinalCount ,
                 "Number of new Vars:                    " ++ show (varsFinalCount - varsReducedCount)]
        part4 = ["Number of or-clauses:                  " ++ show lors,
                 "or-clauses, which are in horn form:    " ++ show lhornX,
                 "Number of xor-clauses:                 " ++ show lxors,
                 "Occurences of Vars in or:              " ++ show occVarsOr,
                 "Occurences of Vars in xor:             " ++ show occVarsXOr ]
        part5 = ["Length "++ show n ++ ":  " ++ show count ++ " clauses with " 
                  ++ show hornCount ++ " of them Horn."
                | n <- [0..maxLength ors], let (count, hornCount) = statsClauses n ors ]
        part6 = ["Length "++ show n ++ ":  " ++ show count ++ " clauses" | n <- [0..maxLengthX xors], let count = statsXClauses n xors ]
        part7 = ["Number or:                             " ++ show lcnf,
                 "Clauses, which are in horn form:       " ++ show lhorn,
                 "Occurences of Vars:                    " ++ show occVarsCnf]
        part8 = ["Length "++ show n ++ ":  " ++ show count ++ " clauses with " 
                  ++ show hornCount ++ " of them Horn."
                | n <- [0..maxLength cnf'], let (count, hornCount) = statsClauses n cnf' ]

        reduced'      = rFormula formula
        demorgen'     = demorgen reduced'
        (cache,xcnf') = formulaToNormalform emptyCache formula
        cnf'          = normalformToCNF xcnf'

    -- Normal Formula 
        varsNormalCount= length $ nub $ toList formula
        occNormalCount = length $ toList formula
        yesNoOccCount  = yesNoCounter formula :: Integer
            where
                yesNoCounter Yes     = 1
                yesNoCounter No      = 1
                yesNoCounter f       = sum $ map yesNoCounter $ getInnerFormulas f

    -- Reduced Formula 
        varsReducedCount = length $ nub $ toList reduced'
        occReducedCount = length $ toList reduced'
        
    -- Final Form general information
        varsFinalCount   = Set.size $ getVariables formula emptyCache
        
    --XCNF
        (ors,xors) = xcnf'
        hornX  = filter isHorn ors
        lors   = length ors
        lhornX = length hornX
        lxors  = length xors
        occVarsOr  = sum $ map length ors
        occVarsXOr = sum $ map length xors
        
    -- CNF
        horn         = filter isHorn cnf'
        lcnf         = length cnf'
        lhorn        = length horn
        occVarsCnf  = sum $ map length cnf'

        
        maxLength :: [[a]] -> Int
        maxLength = foldl max 0 . map length

        maxLengthX :: [Lit [a]] -> Int
        maxLengthX = foldl max 0 . map length . map extract

        statsClauses :: Int -> [OrClause v] -> (Int, Int)
        statsClauses clauseLength allClauses = (length clauses, length horns)
            where
                clauses = filter ((==clauseLength).length) allClauses
                horns   = filter isHorn clauses

        statsXClauses :: Int -> [XOrClause v] -> Int
        statsXClauses clauseLength allClauses = length clauses
            where
                clauses = filter ((==clauseLength).length) allClauses
                
        isHorn :: [Lit v] -> Bool
        isHorn = (<=1) . length . filter sign

showFormulaTransformation :: (Show v, Ord v) => TransformationStep -> Formula v -> String
showFormulaTransformation = showFormulaTransformation' (\i->"Helper"++show i)

showFormulaTransformation' :: forall v. (Show v,Ord v) => (Word -> String) -> TransformationStep -> Formula v -> String
showFormulaTransformation' showE TSNormal     formula = showFormula formula
showFormulaTransformation' showE TSReduced    formula = showFormula $ rFormula formula
showFormulaTransformation' showE TSDemorgen   formula = showFormula $ demorgen $ rFormula formula
showFormulaTransformation' showE TSXCNF       formula = showFormulaEither showE $ normalformToFormula $ snd $ formulaToNormalform emptyCache formula
showFormulaTransformation' showE TSCNF        formula = showFormulaEither showE $ normalformToFormula $ (snd $ formulaToCNF emptyCache formula,[])
showFormulaTransformation' showE TSHelperForm formula = printDefs' True  showE formula
showFormulaTransformation' showE TSHelperDefs formula = printDefs' False showE formula

printDefs' :: forall v. (Show v,Ord v) => Bool -> (Word -> String) -> Formula v -> String
printDefs' withFormula showE formula = mainCNFString ++ concat helperStrings
    where
        (main, newCache, cnfs, defs) = runTransComplete cache trans
        def'          = reverse defs :: [(Var v, DFormula (Var v))]
        cache         = emptyCache
        trans         = transCnf $ demorgen $ rFormula formula
        helperStrings :: [String]
        helperStrings = zipWith (\(name,formula) str -> makeLine name formula str) def' helperFormulas
            where
                makeLine ::  Var v -> DFormula (Var v) -> String -> String
                makeLine name formula string = firstPart ++ "\n" ++ secondPart
                    where
                        firstPart  = either showE undefined name ++ "  :<=>  " ++ showFormulaEither showE formula
                        secondPart = if withFormula then indent tab "By:" string ++ "\n\n" else []
                
        mainCNFString = (showFormulaEither showE $ normalformToFormula $ partitionClauses True main) ++ "\n\n\n"
        
        helperFormulas= map (uncurry transHelperForm) def' :: [String]
        
        printDef :: String -> Formula (Var v) -> String
        printDef name form = name ++ "\n" ++ i (showFormulaEither showE form) ++ "\n\n" 
            where
                i = indent (tab++tab) (take (length (tab++tab)) $ " :<=>"++repeat ' ')
        indent s sFirst string = intercalate "\n" $ (sFirst' ++ head lines) : (map (s++) $ tail lines)
            where
                lines   = splitOn "\n" string
                sFirst' = take (length s) $ sFirst ++ repeat ' '
                
        transHelperForm :: Var v -> DFormula (Var v) -> String
        transHelperForm helper formula = showFormulaEither showE $ normalformToFormula (or',xor')
            where
                replacer :: Var v -> Var v -> Var v -> Var v
                replacer a b c      = if a == c then b else c
                or'                 = replacer hName' helper 💩 or  :: [[Lit (Var v)]]
                xor'                = replacer hName' helper 💩 xor :: [Lit [ Var v ]]
                (or,xor)            = partitionClauses True xcnf :: ([[Lit (Var v)]],[Lit [Var v]])
                (hName,(_,xcnf,_))  = runState (innerForm formula) (newCache,[],[])
                hName'              = extract hName
                unpacked            = map (\(LVar v) -> v) $ getInnerFormulas formula
                innerForm :: (Show v, Eq v) => DFormula (Var v) -> Trans v (Lit (Var v))
                innerForm (All l)  = litOfAnd unpacked
                innerForm (Some l) = litOfOr  unpacked
                innerForm (Odd l)  = litOfXor $ map extract unpacked <$ head unpacked
                (💩) :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
                (💩)                 = (<$>).(<$>).(<$>)
                


showFormula :: (Show v) => GeneralFormula s v -> String
showFormula = showFormula' tab showElem

showFormulaEither :: Show v => (Word -> String) -> GeneralFormula s (Var v) -> String
showFormulaEither showHelper = showFormula' tab shower
    where
    --    show :: Formula (ELit v) -> String
        shower x = maybe (showElem x) (either showHelper show) (unpackVar x)

showFormula' :: forall fo v s. String -> (GeneralFormula s v -> String) -> GeneralFormula s v -> String
showFormula' tab showFunction f
    | isList f     = showFunction f ++ listHelper (getInnerFormulas f)
    | isNegation f = showFunction f ++ showFormula' tab showFunction (head $ getInnerFormulas f)
    | isTerminal f = showFunction f
        where
    --        listHelper :: [f v] -> String
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

