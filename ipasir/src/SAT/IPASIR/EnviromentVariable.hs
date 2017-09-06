{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module SAT.IPASIR.EnvironmentVariable where

import Prelude hiding (lookup)
import Data.Map
import Data.IORef

class Ord a => Variable a where
    type family Data   a (Member a) :: *

Env a = Env (IORef (Map a (IORef (Data a))))

newEnv :: (Variable (Var a)) => IO (Env a)
newEnv = Env <$> newIORef empty

readVar :: (Variable (Var a)) => Env a -> Var a -> IO (Data a)
readVar x (Env mref) = do
    m <- readIORef env
    case lookup var m of
        Just ref -> readIORef ref
        _        -> return $ error "Can't read an undefined variable"

writeVar :: (Variable (Var a)) => Env a -> Var a -> Data a -> IO ()
writeVar env var val = do
    m <- readIORef env
    case lookup var m of
        Just ref -> writeIORef ref val
        _ -> do
            new <- newIORef val
            writeIORef mref (insert var new m)

modifyVar :: (Variable (Var a)) => Env a -> Var a -> (Data a -> Data a) -> IO ()
modifyVar env var f = do
    m <- readIORef env
    case lookup var m of
        Just ref -> modifyIORef ref f
        _        -> return $ error "Can't modify an undefined variable"

atomicModifyVar :: (Variable (Var a)) => Env a -> Var a -> (Data a -> (Data a,b)) -> IO b
atomicModifyVar env var f = do
    m <- readIORef env
    case lookup var m of
        Just ref -> atomicModifyIORef ref f
        _        -> return $ error "Can't atomicModify an undefined variable"

