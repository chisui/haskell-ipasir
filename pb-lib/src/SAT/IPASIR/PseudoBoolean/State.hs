{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module SAT.IPASIR.PseudoBoolean.State
    ( WeightedVariable(..)
    ) where

import SAT.IPASIR

import Control.Monad.Trans.State.Lazy

import SAT.PseudoBoolean.Config 
import qualified SAT.PseudoBoolean.C as C

data WeightedVariable v where
    (:%:) :: Ord v => v -> Integer -> WeightedVariable v

deriving instance Eq   (WeightedVariable v)
deriving instance Ord  (WeightedVariable v)
deriving instance Show v => Show (WeightedVariable v)
