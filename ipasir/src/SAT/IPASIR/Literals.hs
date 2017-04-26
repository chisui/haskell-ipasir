-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

-- | This module provides a type of literals. 

{-# LANGUAGE DeriveFunctor #-}
module SAT.IPASIR.Literals where

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
            shim l = (ordinal l, sign l)

instance Show a => Show (Lit a) where
    show (Pos a) = '+' : show a
    show (Neg a) = '-' : show a

-- | Negate a literal.
neg :: Lit a -> Lit a
neg (Pos a) = Neg a
neg (Neg a) = Pos a

ordinal :: Lit a -> a
ordinal (Pos a) = a
ordinal (Neg a) = a

sign :: Lit a -> Bool
sign (Pos _) = True
sign (Neg _) = False
