-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

-- | This module provides a type of literals. 

{-# LANGUAGE DeriveFunctor #-}
module SAT.IPASIR.Literals where

import Data.Bits (xor)

import Control.Comonad
import Control.Monad

-- | A literal is a positive or negative atom.
data Lit a
    = Pos a
    | Neg a
        deriving (Eq, Functor)

type Ext l = Either Integer l
type ELit l = Lit (Ext l)

-- | We order literals first by variable, then by sign, so that dual
-- literals are neighbors in the ordering.
instance (Ord a) => Ord (Lit a) where
    compare x y = shim x `compare` shim y
        where
            shim l = (extract l, sign l)

instance Applicative Lit where
    pure = Pos
    (<*>) = ap

instance Monad Lit where
    l >>= f = fromBool (sign l `xor` sign l') $> extract l'
        where
            l' = f $ extract l

instance Comonad Lit where
    extract (Pos a) = a
    extract (Neg a) = a

    duplicate (Pos a) = Pos $ Pos a
    duplicate (Neg a) = Neg $ Neg a

instance Show a => Show (Lit a) where
    show (Pos a) = '+' : show a
    show (Neg a) = '-' : show a

fromBool :: Bool -> Lit ()
fromBool True  = Pos ()
fromBool False = Neg ()

-- | Negate a literal.
neg :: Lit a -> Lit a
neg (Pos a) = Neg a
neg (Neg a) = Pos a

sign :: Lit a -> Bool
sign (Pos _) = True
sign (Neg _) = False
