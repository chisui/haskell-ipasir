{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module SAT.IPASIR.EnvironmentVariable where

import Prelude hiding (lookup)
import Data.Map
import Data.IORef
import Control.Monad

import System.IO.Unsafe

import Debug.Trace

class Ord a => Variable a where
    type family Data a  :: *

data Env k a = Env !(IORef (Map k (IORef a)))

{-# NOINLINE newEnv #-}
newEnv :: Ord k => IO (Env k a)
newEnv =  Env <$> newIORef empty

saveReadVar :: Ord k => Env k a -> k -> IO (Maybe a)
saveReadVar (Env mref) var = do
    m <- readIORef mref
    case lookup var m of
        Just ref -> Just <$> readIORef ref
        _        -> return Nothing

readVar :: Ord k => Env k a -> k -> IO a
readVar (Env mref) var = do
    m <- readIORef mref
    case lookup var m of
        Just ref -> readIORef ref
        _        -> return $ error "Can't read an undefined variable"

writeVar :: Ord k => Env k a -> k -> a -> IO ()
writeVar (Env mref) var val = do
    m <- readIORef mref
    case lookup var m of
        Just ref -> writeIORef ref val
        _ -> do
            new <- newIORef val
            writeIORef mref (insert var new m)

modifyDeclareVar :: Ord k => Env k a -> k -> a -> (a -> a) -> IO ()
modifyDeclareVar env var decl f = do
    x <- saveReadVar env var
    case x of
        Just x  -> trace "Fall 1: " $ modifyVar env var f
        Nothing -> trace "Fall 2: " $ writeVar  env var decl

modifyVar :: Ord k => Env k a -> k -> (a -> a) -> IO ()
modifyVar (Env mref) var f = do
    m <- readIORef mref
    case lookup var m of
        Just ref -> modifyIORef ref f
        _        -> return $ error "Can't modify an undefined variable"

atomicModifyVar :: Ord k => Env k a -> k -> (a -> (a,b)) -> IO b
atomicModifyVar (Env mref) var f = do
    m <- readIORef mref
    case lookup var m of
        Just ref -> atomicModifyIORef ref f
        _        -> return $ error "Can't atomicModify an undefined variable"

