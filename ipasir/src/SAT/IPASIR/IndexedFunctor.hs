{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Data.Monoid
import Control.Monad.ST
import qualified Data.Vector.Mutable as MVec

import qualified Data.List as List
import qualified Data.Vector as Vec
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import qualified Data.List.NonEmpty as NE
import qualified Control.Monad.Identity as ID
import qualified Data.Array as Array

class Functor f => IndexedFunctor i f | f -> i where
    imap      :: (i -> a -> b) -> f a -> f b
    (!)       :: f a -> i -> a
    union     :: f a -> f a -> f a
    fromKeyList :: [(i,a)] -> f a

instance IndexedFunctor Int [] where
    imap f       = zipWith f [0..]
    (!)          = (!!)
    union l1 l2  = l1 ++ drop (length l1) l2
    fromKeyList     = fromKeyList' 0
        where
--            fromKeyList' :: Int -> [(Int,a)] -> f a
            fromKeyList' i []   = []
            fromKeyList' i l@(x:xs)
                | i == fst x = snd x : fromKeyList' (i+1) xs
                | otherwise  = case List.find ((==i) . fst) l of
                    Just (_,x) -> x : fromKeyList' (i+1) l
                    Nothing    -> []

instance Eq i => IndexedFunctor i ((->) i) where
    imap f1 f2 x  = f1 x $ f2 x
    (!)           = ($)
    union f1 f2   = f1
    fromKeyList l x  = case List.find ((==x) . fst) l of
                Just (_,x) -> x
                Nothing    -> error "Index not in the list."

instance IndexedFunctor Int NE.NonEmpty where
    imap f       = NE.zipWith f $ NE.fromList [0..]
    (!)          = (NE.!!)
    union l1 l2  = NE.fromList $ union (NE.toList l1) (NE.toList l2)
    fromKeyList     = NE.fromList . fromKeyList

instance IndexedFunctor Int IMap.IntMap where
    imap     = IMap.mapWithKey
    (!)      = (IMap.!)
    union    = IMap.union
    fromKeyList = IMap.fromList
 
instance IndexedFunctor Int Vec.Vector where
    imap     = Vec.imap
    (!)      = (Vec.!)
    union v1 v2 = v1 Vec.++ Vec.drop (Vec.length v1) v2
    fromKeyList l = fmap Maybe.fromJust $ Vec.takeWhile Maybe.isJust $ runST $ do
        let len = length l
        mvector <- MVec.replicate len Nothing 
        mapM_ (\(i,x) -> if i < len
                            then MVec.write mvector i (Just x) 
                            else return ()
                  ) l
        Vec.freeze mvector

instance IndexedFunctor () Maybe where
    imap f mb = f () <$> mb
    mb ! ()   = Maybe.fromJust mb
    union Nothing mb = mb
    union mb      _  = mb
    fromKeyList (((),x):_) = Just x
    fromKeyList _ = Nothing

instance IndexedFunctor () ID.Identity where
    imap f id   = f () <$> id
    id ! ()     = ID.runIdentity id
    union i1 i2 = i1
    fromKeyList (((),x):_) = return x

instance Array.Ix i => IndexedFunctor i (Array.Array i) where
    imap f a    = Array.listArray (Array.bounds a) $ map (uncurry f) $ toKeyList a
    (!)         = (Array.!)
    union a1 a2
        | Array.rangeSize (l,r) > Array.rangeSize (l1,r1) + Array.rangeSize (l2,r2) = error "Array not initialized in every element."
        | otherwise = Array.listArray (l,r) [ if l1 <= i && i <= r1 then a1 ! i else a2 ! i | i <- Array.range (l,r) ]
        where
            (l1, r1) = Array.bounds a1
            (l2, r2) = Array.bounds a2
            l = min l1 l2
            r = max r1 r2
    fromKeyList []     = Array.array undefined []
    fromKeyList list@((x,_):xs) = Array.array (l,r) list
        where
            fold f = foldl (\t (i,_) -> f x i) x xs
            l = fold min
            r = fold max

instance Ord k => IndexedFunctor k (Map.Map k) where
    imap     = Map.mapWithKey
    (!)      = (Map.!)
    union    = Map.union
    fromKeyList = Map.fromList

instance Eq k => IndexedFunctor k ((,) k) where
    imap f (i,x) = (i, f i x)
    (i,x) ! j
        | i == j    = x
        | otherwise = error "Index not found"
    union a b = a
    fromKeyList = head

--IndexedFunctor [Int] Tree

class IndexedFunctor i f => IndexSpaceFunctor i f where
    toKeyList   :: f a -> [(i,a)]
    indices     :: f a -> [i]
    indices = map fst . toKeyList
    elems       :: f a -> [a]
    elems = map snd . toKeyList
    size :: Enum e => f a -> e

instance IndexSpaceFunctor Int [] where
    toKeyList = zip [0..] 
    indices l = [0..length l]
    elems     = id
    size      = toEnum . length

instance IndexSpaceFunctor Int NE.NonEmpty where
    toKeyList = toKeyList . NE.toList
    indices l = [0..NE.length l]
    elems     = NE.toList
    size      = toEnum . NE.length
    
instance IndexSpaceFunctor Int IMap.IntMap where
    toKeyList = IMap.toList
    indices   = IMap.keys
    elems     = IMap.elems
    size      = toEnum . IMap.size
 
instance IndexSpaceFunctor Int Vec.Vector where
    toKeyList = Vec.toList . imap (,)
    indices l = [0..Vec.length l]
    elems     = Vec.toList
    size      = toEnum . Vec.length

instance IndexSpaceFunctor () Maybe where
    toKeyList mb = ((),) <$> Maybe.maybeToList mb
    indices Nothing = []
    indices _       = [()]   
    elems        = Maybe.maybeToList
    size Nothing = toEnum 0
    size _       = toEnum 1

instance IndexSpaceFunctor () ID.Identity where
    toKeyList id = [((),ID.runIdentity id)]
    indices _ = [()]
    elems     = return . ID.runIdentity
    size _    = toEnum 1

instance Array.Ix i => IndexSpaceFunctor i (Array.Array i) where
    toKeyList = Array.assocs
    indices a = Array.range $ Array.bounds a
    elems     = Array.elems
    size      = toEnum . Array.rangeSize . Array.bounds

instance Ord k => IndexSpaceFunctor k (Map.Map k) where
    toKeyList = Map.toList
    indices   = Map.keys
    elems     = Map.elems
    size      = toEnum . Map.size

instance Eq k => IndexSpaceFunctor k ((,) k) where
    toKeyList = return
    indices   = return . fst
    elems     = return . snd
    size _    = toEnum 1


class IndexedFunctor Int f => IntIndexedFunctor f where
    generate :: Int -> (Int -> a) -> f a
    add      :: a -> f a -> f a

    
    



