module SAT.PseudoBoolean
    ( module Export
    , C.CardinalityMethod
    , C.Comp(..)
    , Encoder
    , runEncoder
    , evalEncoder
    , encodeNewGeq
    , encodeNewLeq
    , getClauses
    ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import System.IO.Unsafe

import Foreign.ForeignPtr

import Data.Word
import Data.Int

import SAT.IPASIR
import SAT.IPASIR.Literals as Export
import qualified SAT.PseudoBoolean.C as C
import SAT.PseudoBoolean.Config as Export
import SAT.PseudoBoolean.C.Types.WeightedLit as Export

type Encoder a = StateT (ForeignPtr C.C_Encoder) IO a

evalEncoder :: C.CardinalityMethod a => Config a -> [C.WeightedLit] -> C.Comp -> Int64 -> Int64 -> Int -> Encoder b -> IO b
evalEncoder config lits comp lower upper firstFree body = fst <$> runEncoder config lits comp lower upper firstFree body

runEncoder :: C.CardinalityMethod a => Config a -> [C.WeightedLit] -> C.Comp -> Int64 -> Int64 -> Int -> Encoder b -> IO (b, ForeignPtr C.C_Encoder)
runEncoder config lits comp lower upper firstFree body = do 
    e <- C.encoder config lits comp lower upper firstFree
    runStateT body e

withEncoder body = do
    encoder <- get
    lift $ body encoder
    lift $ C.getClauses encoder

encodeNewGeq :: Int64 -> Encoder [[Lit Word]]
encodeNewGeq bound = withEncoder (`C.encodeNewGeq` bound)
encodeNewLeq :: Int64 -> Encoder [[Lit Word]]
encodeNewLeq bound = withEncoder (`C.encodeNewLeq` bound)

getClauses :: Encoder [[Lit Word]]
getClauses = withEncoder return
