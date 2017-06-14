{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SAT.IPASIR.HelperVarCache where

import qualified Data.Map as M

newtype Space = Space Int deriving (Eq, Ord, Show, Num)

class (Ord v2) => HelperVarCache cache v1 v2 where
    newHelperVarCache :: (v1 -> v2) -> (Word -> v2) -> cache v1 v2
    newSpace   :: cache v1 v2 -> (cache v1 v2, Space)
    newHelper  :: cache v1 v2 -> Space -> (cache v1 v2, v2)
    newHelpers :: cache v1 v2 -> Space -> Word -> (cache v1 v2, [v2])
    getHelpers :: Enum e => cache v1 v2 -> Space -> e -> v2
    
data HVC v1 v2 = HVC {
                        toVar     :: v1 -> v2 ,
                        toHelper  :: Word -> v2 ,
                        nextVar   :: Word ,
                        nextSpace :: Space ,
                        table     :: M.Map Space [Word] 
                     }

instance Show (HVC v1 v2) where
    show hvc = show $ table hvc

instance (Ord v2) => HelperVarCache HVC v1 v2 where
    newHelperVarCache f1 f2 = HVC f1 f2 0 0 M.empty
    newSpace hvc        = (HVC f1' f2' var (nSpace+1) (M.insert nSpace [] table), nSpace)
        where
            HVC f1' f2' var nSpace table = hvc
    newHelper hvc space = (HVC f1' f2' (var+1) nSpace (M.insert space (table M.! space ++ [var]) table), f2' var)
        where
            HVC f1' f2' var nSpace table = hvc
    newHelpers hvc space number = (HVC f1' f2' (var+number) nSpace (M.insert space (table M.! space ++ numbers) table), map f2' numbers)
        where
            HVC f1' f2' var nSpace table = hvc
            numbers = [var..var+number-1]
    getHelpers hvc space pos = toHelper hvc $ (table hvc M.! space) !! fromEnum pos

