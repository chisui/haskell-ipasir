{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module SAT.IPASIR.ManagedLitCache where

import SAT.IPASIR.HelperVarCache
import SAT.IPASIR.LiteralCache
import SAT.IPASIR.Literals

data ManagedLitCache (lc :: * -> *) (hvc :: * -> * -> *) v1 v2 = ManagedLitCache {
                                            getLC  :: lc v2 ,
                                            getHVC :: hvc v1 v2
                                        }


instance (LiteralCache lc v2, HelperVarCache hvc v1 v2) => HelperVarCache (ManagedLitCache lc hvc) v1 v2 where
    newHelperVarCache f1 f2      = ManagedLitCache emptyCache $ newHelperVarCache f1 f2
    newSpace cache               = (ManagedLitCache lc newHVC, space)
      where
        ManagedLitCache lc hvc   = cache
        (newHVC, space)          = newSpace hvc
    newHelper cache space        = (ManagedLitCache newLC newHVC, helper)
      where
        ManagedLitCache lc hvc   = cache
        (newHVC, helper)         = newHelper hvc space
        newLC                    = insertVar lc helper
    newHelpers cache space number= (ManagedLitCache newLC newHVC, helpers)
      where
        ManagedLitCache lc hvc   = cache
        (newHVC, helpers)        = newHelpers hvc space number
        newLC                    = insertVars lc helpers
    getHelpers cache space       = getHelpers (getHVC cache) space

instance {-# OVERLAPPABLE #-} (LiteralCache lc v2, HelperVarCache hvc v1 v2) => LiteralCache (ManagedLitCache lc hvc v1) v2 where
    emptyCache           = error "Not possible to create a ManagedLitCache by emptyCache. Use newHelperVarCache instead."
    insertVar           = insertVar'
    insertVars          = insertVars'
    numVars             = numVars'
    intToVar            = intToVar'
    varToInt            = varToInt'
    clausesToIntClauses = clausesToIntClauses'
    showIntToVar        = showIntToVar'
    showVarToInt        = showVarToInt'

instance (LiteralCache lc (Either v1 Word), HelperVarCache hvc v1 (Either v1 Word)) => LiteralCache (ManagedLitCache lc hvc v1) (Either v1 Word) where
    emptyCache          = newHelperVarCache Left Right
    insertVar           = insertVar'
    insertVars          = insertVars'
    numVars             = numVars'
    intToVar            = intToVar'
    varToInt            = varToInt'
    clausesToIntClauses = clausesToIntClauses'
    showIntToVar        = showIntToVar'
    showVarToInt        = showVarToInt'

insertVar' :: (LiteralCache lc v2, HelperVarCache hvc v1 v2) => ManagedLitCache lc hvc v1 v2 -> v2 -> ManagedLitCache lc hvc v1 v2
insertVar' cache var = ManagedLitCache (insertVar lc var) hvc
    where
        ManagedLitCache lc hvc = cache

insertVars' :: (LiteralCache lc v2, HelperVarCache hvc v1 v2) => ManagedLitCache lc hvc v1 v2 -> [v2] -> ManagedLitCache lc hvc v1 v2
insertVars' cache var = ManagedLitCache (insertVars lc var) hvc
    where
        ManagedLitCache lc hvc = cache

numVars' :: (Enum e, LiteralCache lc v2, HelperVarCache hvc v1 v2) => ManagedLitCache lc hvc v1 v2 -> e
numVars'              = numVars  . getLC

intToVar' :: (Enum e, LiteralCache lc v2, HelperVarCache hvc v1 v2) => ManagedLitCache lc hvc v1 v2 -> e -> v2
intToVar' cache enum  = intToVar (getLC cache) enum 


varToInt' :: (Enum e, LiteralCache lc v2, HelperVarCache hvc v1 v2) => ManagedLitCache lc hvc v1 v2 -> v2 -> e
varToInt' cache var   = varToInt (getLC cache) var

clausesToIntClauses' :: (LiteralCache lc v2, Enum e) => ManagedLitCache lc hvc v1 v2 -> [[Lit v2]] -> (ManagedLitCache lc hvc v1 v2, [[Lit e]])
clausesToIntClauses' cache clauses = (ManagedLitCache newLC hvc, enums)
    where
        (newLC, enums)         = clausesToIntClauses lc clauses
        ManagedLitCache lc hvc = cache

showIntToVar' :: (LiteralCache lc v2, Show v2) => ManagedLitCache lc hvc v1 v2 -> String
showIntToVar' = showIntToVar . getLC

showVarToInt' :: (LiteralCache lc v2, Show v2) => ManagedLitCache lc hvc v1 v2 -> String
showVarToInt' = showVarToInt . getLC


data Tuep = T1 Int | T2 String | Helper Int deriving (Show, Ord, Eq)

hvc1 = newHelperVarCache f (Helper . fromEnum) :: ManagedLitCache LitCache HVC Tuep Tuep


f (Helper _) = error "xx"
f x          = x
(hvc2, s1)   = newSpace hvc1
(hvc3, _ )   = newHelpers hvc2 s1 5
hvc4         = insertVars hvc3 [T1 99, T2 "Hallo", T2 "Welt"]
(hvc5, s2)   = newSpace hvc4
(hvc6, _)    = newHelpers hvc5 s2 2
(hvc7, _)    = newHelper hvc6 s1

tester       = insertVars hvc2 [T1 99, T2 "Welt", T2 "Hallo"]
(tester',_)  = newHelpers tester s1 2

