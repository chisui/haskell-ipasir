{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module SAT.IPASIR.ClausesCache where

import SAT.IPASIR.Formula (Formula, formulaToNormalform, formulaToCNF)
import SAT.IPASIR.Literals
import SAT.IPASIR.LiteralCache
import SAT.IPASIR.Clauses

import Data.Kind

data XCnfClausesCache v = XCnfClausesCache (LitCache v) [[Lit Word]] [Lit [Word]] 

class ClausesForm cf where
    type VarType cf
    getOrs  :: (VarType cf ~ v) => cf -> [[Lit v]]
    getXOrs :: (VarType cf ~ v) => cf -> [Lit [v]]
    getCNF  :: (VarType cf ~ v) => cf -> [[Lit v]]
    getCNF cc = ors ++ xclausesToCNF xors
        where
            ors  = getOrs  cc
            xors = getXOrs cc

instance ClausesForm (XCnfClausesCache v) where
    type VarType (XCnfClausesCache v) = v
    getOrs  (XCnfClausesCache lc clauses _)  = map (map (intToVar lc <$>) ) clauses  -- [ [ intToVar lc <$> lit | lit <- c ]  | c <- clauses]
    getXOrs (XCnfClausesCache lc _ xclauses) = map (map (intToVar lc) <$>) xclauses

instance Eq v => ClausesForm (Formula v) where
    type VarType (Formula v) = Ext v
    getOrs  = fst . formulaToNormalform
    getXOrs = snd . formulaToNormalform
    getCNF  = formulaToCNF

instance ClausesForm [[Lit v]] where
    type VarType [[Lit v]] = v
    getOrs  = id
    getXOrs = const []
    getCNF  = id

instance ClausesForm [Lit [v]] where
    type VarType [Lit [v]] = v
    getOrs  = const []
    getXOrs = id

instance ClausesForm ([[Lit v]],[Lit [v]]) where
    type VarType ([[Lit v]],[Lit [v]]) = v
    getOrs  = fst
    getXOrs = snd

class ClausesCache (cc :: * -> *) where
    getLiteralCache :: cc v -> LitCache v
    getOrsInt          :: cc v -> [[Lit Word]]
    getXOrsInt         :: cc v -> [Lit [Word]]
    getCNFInt          :: cc v -> [[Lit Word]]
    getCNFInt cc = ors ++ xclausesToCNF xors
        where
            ors  = getOrsInt  cc
            xors = getXOrsInt cc
    addClause       :: (Ord v) => cc v ->  [Lit v] -> cc v
    addClause cc = addClauses cc . return
    addClauses      :: (Ord v) => cc v -> [[Lit v]] -> cc v
    addClauses   = foldl addClause 
    addXClause      :: (Ord v) => cc v -> Lit [v] -> cc v
    addXClause cc = addXClauses cc . return
    addXClauses     :: (Ord v) => cc v -> [Lit [v]] -> cc v
    addXClauses   = foldl addXClause 
    addClausesForm :: (ClausesForm cf, Ord (VarType cf)) => cc (VarType cf) -> cf -> cc (VarType cf)
    addClausesForm cc cf = addXClauses (addClauses cc $ getOrs cf) $ getXOrs cf

instance ClausesCache (XCnfClausesCache) where
    getLiteralCache (XCnfClausesCache lc _ _)       = lc
    getOrsInt      (XCnfClausesCache _ clauses _)      = clauses
    getXOrsInt     (XCnfClausesCache _ _ xclauses)     = xclauses
    addClause (XCnfClausesCache lc oc xc) clause    = XCnfClausesCache newLC (newOC:oc) xc
        where
            (newLC, [newOC]) = clausesToIntClauses lc [clause]
    addXClause (XCnfClausesCache lc oc xc) xclause  = XCnfClausesCache newLC oc (newXC:xc)
        where
            newLC = insertVars lc $ ordinal xclause
            newXC = map (varToInt newLC) <$> xclause


