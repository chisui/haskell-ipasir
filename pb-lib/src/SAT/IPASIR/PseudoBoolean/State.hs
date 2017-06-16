{-# LANGUAGE ScopedTypeVariables #-}
module SAT.IPASIR.PseudoBoolean.State
    ( WeightedLits
    , PBConstraint(..)
    , PBEncoder
    , newPBEncoder
    , pushLowerBound
    , pushUpperBound
    ) where

import Foreign.ForeignPtr

import Control.Comonad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Data.Word
import Data.Int
import Data.Monoid
import qualified Data.Map as Map

import qualified SAT.IPASIR as I

import SAT.PseudoBoolean
import SAT.PseudoBoolean.Config 
import qualified SAT.PseudoBoolean.C as C


type WeightedLits v = Map.Map (I.Var v) Integer

data PBConstraint c v = PBConstraint
    { pbConfig :: Config c
    , vars   :: WeightedLits v
    , comp   :: C.Comp
    , lower  :: Word
    , upper  :: Word
    } deriving (Eq, Show)

type PBEncoder v r = StateT (I.VarCache v, Last (ForeignPtr C.C_Encoder)) IO r

type Cls v = [[I.Lit (I.Var v)]]

newPBEncoder :: forall c v. (C.CardinalityMethod c, Ord v) => PBConstraint c v -> PBEncoder v (Cls v)
newPBEncoder c = do
    (vc, encoder) <- get
    encoder' <- lift $ C.encoder (pbConfig c) (weightedLits vc) (comp c) (cn $ lower c) (cn $ upper c) (nVars vc)
    rawClauses <- lift $ C.getClauses encoder'
    let (clauses, vc') = runState (toClauses vc rawClauses) vc
    put (vc', encoder <> return encoder')
    return clauses
    where
        toClauses :: I.VarCache v -> [[I.Lit Word]] -> State (I.VarCache v) [[I.Lit (I.Var v)]] 
        toClauses vc = mapM $ mapM $ resolveVar $ nVars vc
        nVars vc = length $ weightedLits vc
        weightedLits vc = asLit vc <$> Map.toList (vars c)
        asLit vc (v, i) = I.varToInt vc v $-$ fromInteger i

resolveVar :: Ord v => Int -> I.Lit Word -> State (I.VarCache v) (I.Lit (I.Var v))
resolveVar nVars i
    | nVars < cn (extract i) = (<$ i) <$> state I.newHelper
    | otherwise              = gets $ (<$> i) . I.intToVar

pushLowerBound, pushUpperBound :: Word -> PBEncoder v (Cls v)
(pushLowerBound, pushUpperBound) = (wrap encodeNewGeq, wrap encodeNewLeq)
    where
        wrap pbf i = do
            (vc, encoder) <- get
            rawClauses <- lift $ mapM (pbf $ cn i) encoder
            case getLast rawClauses of
                Just rawClauses -> do
                    let (clauses, vc') = runState (toClauses vc rawClauses) vc
                    put (vc', encoder)
                    return clauses
                Nothing -> return []
        toClauses :: I.VarCache v -> [[I.Lit Word]] -> State (I.VarCache v) [[I.Lit (I.Var v)]] 
        toClauses vc = mapM $ mapM $ resolveVar $ nVars vc
        nVars vc = length $ weightedLits vc
                




cn :: (Enum a, Enum b) => a -> b
cn = toEnum . fromEnum