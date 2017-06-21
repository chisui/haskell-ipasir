{-# LANGUAGE ScopedTypeVariables #-}
module SAT.IPASIR.PseudoBoolean.State
    ( WeightedLits
    , Enc
    , PBConstraint(..)
    , PBEncoder
    , newPBEncoder
    , pushLowerBound
    , pushUpperBound
    ) where

import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr

import Control.Comonad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Data.Word
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vec

import qualified SAT.IPASIR as I

import SAT.PseudoBoolean
import SAT.PseudoBoolean.Config 
import qualified SAT.PseudoBoolean.C as C

import Debug.Trace


type WeightedLits v = Map.Map v Integer

data PBConstraint c v = PBConstraint
    { pbConfig  :: Config c
    , wordToVar :: Word -> v
    , vars      :: WeightedLits v
    , comp      :: C.Comp
    , lower     :: Int
    , upper     :: Int
    }

type Vars v = Vec.Vector v
type Enc c v = (PBConstraint c v, Vars v, ForeignPtr C.C_Encoder)
type PBEncoder c v r = StateT (Maybe (Enc c v)) IO r

type Cls v = [[I.Lit v]]

newPBEncoder :: forall c v. (C.CardinalityMethod c, Ord v) => PBConstraint c v -> PBEncoder c v (Cls v)
newPBEncoder c = do
    encoder <- lift $ C.encoder (pbConfig c) (weightedLits c) (comp c) (cn $ lower c) (cn $ upper c) (nVars c)
    rawClauses <- lift $ C.getClauses encoder
    let (clauses, vs) = runState (toClauses (wordToVar c) rawClauses) initVars
    put $ Just (c, vs, encoder)
    return clauses
    where
        initVars = Vec.fromList $ Set.toList $ Map.keysSet $ vars c

pushLowerBound :: Ord v => Int -> PBEncoder c v (Cls v) 
pushLowerBound = wrap C.encodeNewGeq
pushUpperBound :: Ord v => Int -> PBEncoder c v (Cls v)
pushUpperBound = wrap C.encodeNewLeq

wrap :: Ord v => (ForeignPtr C.C_Encoder -> Int64 -> IO ()) -> Int -> PBEncoder c v (Cls v)
wrap pbf i = do
    (c, vs, encoder) <- gets $ fromMaybe $ error "PBEncoder not initialized"
    lift $ pbf encoder $ cn i
    rawClauses <- lift $ C.getClauses encoder
    let (clauses, vs') = runState (toClauses (wordToVar c) rawClauses) vs
    put $ Just (c, vs', encoder)
    return clauses

resolveVar :: Ord v => (Word -> v) -> I.Lit Word -> State (Vars v) (I.Lit v)
resolveVar f lit = do
    maybeV <- gets (Vec.!? i)
    v <- case maybeV of
        Just v  -> return v
        Nothing -> do
            let v' = f $ extract lit
            modify ( `Vec.snoc` v')
            return v'
    return $ v <$ lit
    where
        i = cn $ extract lit

toClauses :: Ord v => (Word -> v) -> [[I.Lit Word]] -> State (Vars v) [[I.Lit v]]
toClauses f = mapM $ mapM $ resolveVar f
nVars = length . Map.keys . vars
weightedLits :: (C.CardinalityMethod c, Ord v) => PBConstraint c v -> [WeightedLit]
weightedLits = map (\(v, i) -> v $-$ fromInteger i) . Map.toList . unwrappedVars
unwrappedVars :: (C.CardinalityMethod c, Ord v) => PBConstraint c v -> Map.Map Word Integer
unwrappedVars = Map.fromList . zipWith (\i (v, w) -> (i, w)) [0..] . sortOn fst . Map.toList . vars

cn :: (Enum a, Enum b) => a -> b
cn = toEnum . fromEnum