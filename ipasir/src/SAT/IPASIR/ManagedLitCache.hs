{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SAT.IPASIR.ManagedLitCache where

import SAT.IPASIR.HelperVarCache
import SAT.IPASIR.LiteralCache

data ManagedLitCache v1 v2 = ManagedLitCache {
                            litCache :: LitCache v2 ,
                            hvc      :: HVC v1 v2 
                          } deriving (Show)

instance (Ord v2) => HelperVarCache ManagedLitCache v1 v2 where
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
    getHelpers cache space       = getHelpers (hvc cache) space
                          
instance (Ord v2) => LiteralCache (ManagedLitCache v1) v2 where
    emptyCache           = error "Not possible to create a ManagedLitCache by emptyCache. Use newHelperVarCache instead."
    insertVar cache var  = ManagedLitCache (insertVar lc var) hvc
        where
            ManagedLitCache lc hvc = cache
    insertVars cache var = ManagedLitCache (insertVars lc var) hvc
        where
            ManagedLitCache lc hvc = cache
    numVars              = numVars  . litCache
    intToVar cache enum  = intToVar (litCache cache) enum 
    varToInt cache var   = varToInt (litCache cache) var
    clausesToIntClauses cache clauses = (ManagedLitCache newLC hvc, enums)
        where
            (newLC, enums)         = clausesToIntClauses lc clauses
            ManagedLitCache lc hvc = cache
    showIntToVar         = showIntToVar . litCache
    showVarToInt         = showVarToInt . litCache
    
data Tuep = T1 Int | T2 String | Helper Int deriving (Show, Ord, Eq)

hvc1 = newHelperVarCache f (Helper . fromEnum) :: ManagedLitCache Tuep Tuep


f (Helper _) = error "xx"
f x          = x
(hvc2, s1) = newSpace hvc1
(hvc3, _ ) = newHelpers hvc2 s1 5
hvc4       = insertVars hvc3 [T1 99, T2 "Hallo", T2 "Welt"]
(hvc5, s2) = newSpace hvc4
(hvc6, _)  = newHelpers hvc5 s2 2
(hvc7, _)  = newHelper hvc6 s1

tester     = insertVars hvc2 [T1 99, T2 "Hallo", T2 "Welt"]
(tester',_) = newHelpers tester s1 2