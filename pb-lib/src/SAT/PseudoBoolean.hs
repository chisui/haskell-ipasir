module SAT.PseudoBoolean
    ( module Export
    , C.CardinalityMethod
    , C.Comp(..)
    , runEncoder
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

import qualified SAT.PseudoBoolean.C as C
import SAT.PseudoBoolean.Config as Export
import SAT.PseudoBoolean.C.Types.WeightedLit as Export


type Encoder a = StateT (ForeignPtr C.C_Encoder) IO a

runEncoder :: C.CardinalityMethod a => Config a -> [C.WeightedLit] -> C.Comp -> Word64 -> Word64 -> Int -> Encoder b -> IO b
runEncoder config lits comp lower upper firstFree body = do 
    e <- C.encoder config lits comp lower upper firstFree
    evalStateT body e

withEncoder body = do
    encoder <- get
    lift $ body encoder
    lift $ C.getClauses encoder

encodeNewGeq :: Word64 -> Encoder [[Int32]]
encodeNewGeq bound = withEncoder (`C.encodeNewGeq` bound)
encodeNewLeq :: Word64 -> Encoder [[Int32]]
encodeNewLeq bound = withEncoder (`C.encodeNewLeq` bound)

getClauses :: Encoder [[Int32]]
getClauses = withEncoder return
