-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

-- | This module provides a type of literals. 

{-# LANGUAGE DeriveFunctor #-}
module SAT.IPASIR.Literals
    ( Lit (..)
    , lit
    , fromBool
    , neg
    , sign
    , toInt
    ) where

import Data.String (IsString(..))
import Data.Bits (xor)
import Data.Traversable

import Control.Comonad
import Control.Monad

-- | A literal is a positive or negative atom.
data Lit a
    = Pos a
    | Neg a
        deriving (Eq, Functor)

-- | We order literals first by variable, then by sign, so that dual
-- literals are neighbors in the ordering.
instance (Ord a) => Ord (Lit a) where
    compare x y = shim x `compare` shim y
        where
            shim l = (extract l, sign l)

instance Foldable Lit where
    foldMap f = f . extract

instance Traversable Lit where
    traverse f l = (l $>) <$> f (extract l)

instance Applicative Lit where
    pure = Pos
    (<*>) = ap

instance Monad Lit where
    l >>= f = s `lit` extract l'
        where
            s = sign l `xor` sign l'
            l' = f $ extract l

instance Comonad Lit where
    extract (Pos a) = a
    extract (Neg a) = a

    duplicate (Pos a) = Pos $ Pos a
    duplicate (Neg a) = Neg $ Neg a

instance Show a => Show (Lit a) where
    show (Pos a) = '+' : show a
    show (Neg a) = '-' : show a

instance IsString a => IsString (Lit a) where
    fromString = return . fromString

-- | Create a Literal with given sign
lit :: Bool -> a -> Lit a
lit True  = Pos
lit False = Neg

-- | Create an Empty Literal with given sign
fromBool :: Bool -> Lit ()
fromBool = (`lit` ())

-- | Negate a literal.
neg :: Lit a -> Lit a
neg (Pos a) = Neg a
neg (Neg a) = Pos a

-- | Get the sign of a Literal
sign :: Lit a -> Bool
sign (Pos _) = True
sign (Neg _) = False

-- | Coerces a @Literal@ of a @Num@ @n@ to a signed @Int@ where the sign is the literals sign.
-- for this to work @n > 0@ has to hold.
toInt :: (Ord a, Integral a, Show a) => Lit a -> Int
toInt l = s * let n = extract l in
        if n > 0
            then fromEnum $ toInteger n
            else error $ "can not coerce Literals with numbers <= 0 to Int: " ++ show n
    where
        s = if sign l
            then 1
            else -1
