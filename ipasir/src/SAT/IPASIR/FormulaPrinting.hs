{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

{- |This module support a bunch of functions, to show formulas and to get statistics. 
    Very helpful for debugging and to increate perfromance. 
-}
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

{- |Represents the different transformation steps. See "SAT.IPASIR.Formula" to get more
    information about the steps.

>+----+----------------+-------------------------------------------------------+
>|Enum| Marker         | Description                                           |
>+====+================+=======================================================+
>| 0. | @TSNormal@     | The original formula.                                 |
>+----+----------------+-------------------------------------------------------+
>| 1. | @TSReduced@    | The formula after reduction. See 'rFormula'.          |
>+----+----------------+-------------------------------------------------------+
>| 2. | @TSDemorgan@   | The reduced formula after using the rules of          |
>|    |                | De Morgan. See 'demorgan'.                            |
>+----+----------------+-------------------------------------------------------+
>| 3. | @TSXCNF@       | The XCNF of the formula. See 'formulaToNormalform'.   |
>+----+----------------+-------------------------------------------------------+
>| 4. | @TSHelperDefs@ | Same step as @TSXCNF@, but doesn't print clauses      |
>|    |                | (or xclauses), which just define helper varianes.     |
>|    |                | Its prints the definition of the helper variables     |
>|    |                | instead.                                              |
>+----+----------------+-------------------------------------------------------+
>| 5. | @TSHelperForm@ | Same as @TSHelperDefs@, but it also prints the        |
>|    |                | clauses, which define the helper variables.           |
>+----+----------------+-------------------------------------------------------+
>| 6. | @TSCNF@        | The CNF of the formula. See 'formulaToCNF'.           |
>+----+----------------+-------------------------------------------------------+

    The transformation of a formula passes \\( 0 \\to 1 \\to 2 \\to 3,4,5 \\to 6 \\). 

    3, 4 and 5 represent the same transformation step.
-}
data TransformationStep = TSNormal 
                        | TSReduced 
                        | TSDemorgan 
                        | TSXCNF 
                        | TSHelperDefs 
                        | TSHelperForm 
                        | TSCNF
                        deriving (Show, Eq, Read, Ord, Bounded, Enum)

-- | True, iff the formula is a @Not@. 
isNegation :: GeneralFormula t v -> Bool
isNegation (Not _) = True
isNegation _       = False

-- | True, iff the formula is an @All@, @Some@ or @Odd@. 
isList :: GeneralFormula t v -> Bool
isList (All _)     = True
isList (Some _)    = True
isList (Odd _)     = True
isList _           = False

-- | If the formula is a @Not@, @All@, @Some@ or @Odd@, it gives the inner formulas. Else an empty list. 
getInnerFormulas :: GeneralFormula t v -> [GeneralFormula t v] 
getInnerFormulas (Not f)  = [f]
getInnerFormulas (All l)  = l
getInnerFormulas (Some l) = l
getInnerFormulas (Odd l)  = l
getInnerFormulas _        = []

-- | Gives a representing text for the type of the formula. 
showElem :: Show v => GeneralFormula t v -> String 
showElem Yes       = "YES  "
showElem No        = "NO   "
showElem (All l)   = "ALL  "
showElem (Some l)  = "SOME "
showElem (Odd l)   = "ODD  "
showElem (Not f)   = "-"
showElem (Var x)   = show x
showElem (PVar x)  = '+' : show x
showElem (NVar x)  = '-' : show x

-- | Four spaces. For real, its that simple.
tab = "    " :: String

-- |Prints the text of 'showFormulaStatistics'.
printFormulaStatistics :: (Ord v) => Formula v -> IO ()
printFormulaStatistics = putStrLn . showFormulaStatistics

-- |Gives a String with lots of informations about the formula. This includes number of variables
--  after every transformation step, the number and length of clauses and more.
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
        demorgan'     = demorgan reduced'
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

-- |Prints the String of 'showFormulaTransformation'
printFormulaTransformation :: (Show v, Ord v) => TransformationStep -> Formula v -> IO () 
printFormulaTransformation s v = putStrLn $ showFormulaTransformation s v

-- |Shows the formula after a given 'TransformationStep'.
showFormulaTransformation :: (Show v, Ord v) => TransformationStep -> Formula v -> String
showFormulaTransformation = showFormulaTransformation' (\i->"Helper"++show i)

-- |Almost the same as 'showFormulaTransformation', but you can give your own show function for the helper variables.
showFormulaTransformation' :: forall v. (Show v,Ord v) => (Word -> String) -> TransformationStep -> Formula v -> String
showFormulaTransformation' showE TSNormal     formula = showFormula formula
showFormulaTransformation' showE TSReduced    formula = showFormula $ rFormula formula
showFormulaTransformation' showE TSDemorgan   formula = showFormula $ demorgan $ rFormula formula
showFormulaTransformation' showE TSXCNF       formula = showFormulaEither showE $ normalformToFormula $ snd $ formulaToNormalform emptyCache formula
showFormulaTransformation' showE TSCNF        formula = showFormulaEither showE $ normalformToFormula $ (snd $ formulaToCNF emptyCache formula,[])
showFormulaTransformation' showE TSHelperForm formula = showDefs' True  showE formula
showFormulaTransformation' showE TSHelperDefs formula = showDefs' False showE formula

-- |Super evil function. Please dont look at it. 
showDefs' :: forall v. (Show v,Ord v) => Bool -> (Word -> String) -> Formula v -> String
showDefs' withFormula showE formula = mainCNFString ++ concat helperStrings
    where
        (main, newCache, cnfs, defs) = runTransComplete cache trans
        def'          = reverse defs :: [(Var v, DFormula (Var v))]
        cache         = emptyCache
        trans         = transCnf $ demorgan $ rFormula formula
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
                unpacked            = map asLit $ getInnerFormulas formula
                innerForm :: (Show v, Eq v) => DFormula (Var v) -> Trans v (Lit (Var v))
                innerForm (All l)  = litOfAnd unpacked
                innerForm (Some l) = litOfOr  unpacked
                innerForm (Odd l)  = litOfXor $ map extract unpacked <$ head unpacked
                (💩) :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
                (💩)                 = (<$>).(<$>).(<$>)
                

-- |Prints the String of 'showFormula'.
printFormula :: (Show v) => GeneralFormula s v -> IO ()
printFormula = putStrLn . showFormula

{- |Same as 

    > showFormulaTransformation TSNormal
-}
showFormula :: (Show v) => GeneralFormula s v -> String
showFormula = showFormula' tab showElem

-- |Prints the String of 'showFormulaEither'
printFormulaEither :: Show v => (Word -> String) -> GeneralFormula s (Var v) ->  IO ()
printFormulaEither g f = putStrLn $ showFormulaEither g f

{- |Prints a formula, which can have helper variables. The first parameter is a 
    print function for the helper variables. -}
showFormulaEither :: Show v => (Word -> String) -> GeneralFormula s (Var v) -> String
showFormulaEither showHelper = showFormula' tab shower
    where
    --    show :: Formula (ELit v) -> String
        shower x = maybe (showElem x) (either showHelper show) (unpackVar x)

-- |Just important for the implementarion of the show functions. Might be hidden in later version.
showFormula' :: String -> (GeneralFormula s v -> String) -> GeneralFormula s v -> String
showFormula' tab showFunction f
    | isList f     = showFunction f ++ listHelper (getInnerFormulas f)
    | isNegation f = showFunction f ++ showFormula' tab showFunction (head $ getInnerFormulas f)
    | isTerminal f = showFunction f
        where
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
