-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

-- | This module provides a type of literals. 

{-# LANGUAGE DeriveFunctor #-}
module SAT.IPASIR.Literals
    ( Lit (..)
    , LBool (..)
    , lit
    , fromBool
    , neg
    , isPositive
    , intToLit
    , litToInt
    , lBool2MBool
    , mBool2LBool
    ) where

import Data.String (IsString(..))
import Data.Bits (xor)
import Data.Traversable

import Control.Comonad
import Control.Monad

-- | A solution for a single variable.
-- @Just a@ indicates that the variable is @a@ in the solution
-- @Nothing@ indicates that the variable is not important for the solution.
-- both @True@ and @False@ are valid assignments.
-- 
-- Working with this representation may be cumbersome. If you do not want to
-- deal with unimportant variables pass your solutions through @expandSolution@.
data LBool = LFalse | LUndef | LTrue deriving (Eq,Ord,Bounded)

instance Show (LBool) where
    show LTrue  = "1"
    show LFalse = "0"
    show LUndef = "?"
    
instance Enum (LBool) where
    fromEnum LTrue  =  1
    fromEnum LFalse = -1
    fromEnum LUndef = 0
    
    toEnum i
        | i == 0    = LUndef
        | i <  0    = LFalse
        | otherwise = LTrue
    
lBool2MBool LUndef = Nothing
lBool2MBool LTrue  = Just True
lBool2MBool _      = Just False

mBool2LBool Nothing     = LUndef
mBool2LBool (Just True) = LTrue
mBool2LBool _           = LFalse

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
            shim l = (extract l, isPositive l)

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
            s = isPositive l `xor` isPositive l'
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

-- | Get the sign of a Literal (True for positive)
isPositive :: Lit a -> Bool
isPositive (Pos _) = True
isPositive (Neg _) = False

intToLit :: Int -> Lit Word
intToLit i
    | i < 0 = Neg $ toEnum (-i)
    | i > 0 = Pos $ toEnum i
    
litToInt :: Lit Word -> Int
litToInt (Pos x) = fromEnum x
litToInt (Neg x) = negate $ fromEnum x

instance (Enum a) => Enum (Lit a) where
    -- | Coerces a @Literal@ of a @Num@ @n@ to a signed @Int@ where the sign is the literals sign.
    -- for this to work @n > 0@ has to hold.
    fromEnum l = s * let n = fromEnum $ extract l in
            if n > 0
                then fromEnum n
                else error $ "can not coerce Literals with numbers <= 0 to Int"
        where
            s = if isPositive l
                then 1
                else -1

    toEnum 0 = error "Can't transform 0 into a Lit. (function toEnum)."
    toEnum x = lit (x>0) (toEnum $ abs x)

