module SAT.PseudoBoolean
    ( module Export
    , C.CardinalityMethod
    , C.Comp(..)
    , runEncoder
    , encodeNewGeq
    , encodeNewLeq
    , getClauses
    ) where

import Control.Monad.Trans.State
import System.IO.Unsafe

import Foreign.ForeignPtr

import Data.Word
import Data.Int

import qualified SAT.PseudoBoolean.C as C
import SAT.PseudoBoolean.Config as Export
import SAT.PseudoBoolean.C.Types.WeightedLit as Export


type Encoder a = State (ForeignPtr C.C_Encoder) a

runEncoder :: C.CardinalityMethod a => Config a -> [C.WeightedLit] -> C.Comp -> Word64 -> Word64 -> Int -> Encoder b -> b
runEncoder config lits comp lower upper firstFree body = unsafePerformIO $ do 
    e <- C.encoder config lits comp lower upper firstFree
    return $ evalState body e

withEncoder body = do
    modify' perform
    enc <- get
    return $ unsafePerformIO $ C.getClauses enc
    where
        perform enc = body' enc `seq` enc
        body' = unsafePerformIO . body

encodeNewGeq :: Word64 -> Encoder [[Int32]]
encodeNewGeq bound = withEncoder (`C.encodeNewGeq` bound)
encodeNewLeq :: Word64 -> Encoder [[Int32]]
encodeNewLeq bound = withEncoder (`C.encodeNewLeq` bound)

getClauses :: Encoder [[Int32]]
getClauses = withEncoder return
