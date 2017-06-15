module SAT.IPASIR.PseudoBoolean.State
    () where

import Control.Monad.Trans.State.Lazy

import qualified Data.Map as Map

import SAT.IPASIR
import SAT.IPASIR.HelperVarCache

import SAT.PseudoBoolean.Config 
import qualified SAT.PseudoBoolean.C as C


type WeightedLits v = Map.Map v Integer

data PBConstraint v a = PBConstraint
    { config :: Config a
    , vars   :: WeightedLits
    , comp   :: C.Comp
    , lower  :: Word
    , upper  :: Word
    } deriving (Eq, Show)

type PBEncoder cache l v r = StateT (cache l v, ForeignPtr C.C_Encoder) IO r

newEncoder :: (Ord v, HelperVarCache cache l v) => PBConstraint v a -> cache l v -> PBEncoder cache l v r 
