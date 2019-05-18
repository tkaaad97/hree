{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Hree.Component
    ( ComponentStore(..)
    , ComponentStoreState(..)
    , Entity(..)
    , componentSize
    , newComponentStore
    , addComponent
    , removeComponent
    , readComponent
    , writeComponent
    , modifyComponent
    ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.ST (RealWorld)
import Data.Hashable (Hashable(..))
import qualified Data.HashTable.IO as HT
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Generic.Mutable as MV
import Foreign (Storable(..))

newtype Entity = Entity
    { unEntity :: Int
    } deriving (Show, Eq, Hashable, Storable)

newtype ComponentStore v a = ComponentStore
    { unComponentStore :: IORef (ComponentStoreState v a)
    }

type HashTable k v = HT.BasicHashTable k v

data ComponentStoreState v a = ComponentStoreState
    { componentStoreSize      :: !Int
    , componentStoreVec       :: !(v RealWorld a)
    , componentStoreEntityVec :: !(v RealWorld Entity)
    , componentStoreEntityMap :: !(HashTable Entity Int)
    }

newComponentStore :: (MV.MVector v a, MV.MVector v Entity) => Int -> Proxy (v RealWorld a) -> IO (ComponentStore v a)
newComponentStore preserve _ = do
    v <- MV.new preserve
    ev <- MV.new preserve
    em <- HT.newSized preserve
    ref <- newIORef (ComponentStoreState 0 v ev em)
    return (ComponentStore ref)

componentSize :: ComponentStore v a -> IO Int
componentSize =
    fmap componentStoreSize . readIORef . unComponentStore

addComponent :: (MV.MVector v a, MV.MVector v Entity) => Entity -> a -> ComponentStore v a -> IO ()
addComponent e a store = do
    s <- readIORef (unComponentStore store)
    maybeIndex <- HT.lookup (componentStoreEntityMap s) e
    maybe addNewEntity (\i -> writeComponentAt i a store) maybeIndex
    where
    addNewEntity = do
        (i, s) <- genComponentIndex store
        let vec = componentStoreVec s
            evec = componentStoreEntityVec s
            emap = componentStoreEntityMap s
        MV.unsafeWrite vec i a
        MV.unsafeWrite evec i e
        HT.insert emap e i

    genComponentIndex store = do
        s <- readIORef ref
        let currentSize = componentStoreSize s
            reservedSize = MV.length $ componentStoreVec s
        when (reservedSize <= currentSize) $
            extendComponentStore store =<< decideSize currentSize
        atomicModifyIORef' ref (genIdentifier currentSize)
        where
        ref = unComponentStore store
        genIdentifier i s =
            let s' = s { componentStoreSize = i + 1 }
            in (s', (i, s))

    decideSize :: Int -> IO Int
    decideSize current = do
        let limit = maxBound :: Int
        when (current >= fromIntegral limit)
            (throwIO . userError $ "max bound")
        return $ if current > limit `div` 2
            then limit
            else current * 2

removeComponent :: (MV.MVector v a, MV.MVector v Entity) => Entity -> ComponentStore v a -> IO Bool
removeComponent e store = do
    s <- readIORef (unComponentStore store)
    maybeIndex <- HT.lookup (componentStoreEntityMap s) e
    maybe (return False) relocate maybeIndex

    where
    ref = unComponentStore store
    decrementSize s =
        let currentSize = componentStoreSize s
            s' = s { componentStoreSize = currentSize - 1 }
        in (s', s')
    relocate i = do
        s <- atomicModifyIORef' ref decrementSize
        let j = componentStoreSize s
            vec = componentStoreVec s
            evec = componentStoreEntityVec s
            emap = componentStoreEntityMap s
        last <- MV.unsafeRead evec j
        MV.unsafeSwap vec i j
        MV.unsafeSwap evec i j
        HT.mutate emap last (const (Just i, ()))
        HT.delete emap e
        return True

readComponent :: (MV.MVector v a, MV.MVector v Entity) => Entity -> ComponentStore v a -> IO (Maybe a)
readComponent e store = do
    s <- readIORef (unComponentStore store)
    let vec = componentStoreVec s
        emap = componentStoreEntityMap s
    maybeIndex <- HT.lookup emap e
    maybe (return Nothing)
        (fmap Just . MV.unsafeRead vec) maybeIndex

writeComponent :: (MV.MVector v a, MV.MVector v Entity) => Entity -> a -> ComponentStore v a -> IO Bool
writeComponent e a store = do
    s <- readIORef (unComponentStore store)
    maybeIndex <- HT.lookup (componentStoreEntityMap s) e
    maybe (return False)
        (\i -> MV.unsafeWrite (componentStoreVec s) i a >> return True)
        maybeIndex

writeComponentAt :: (MV.MVector v a, MV.MVector v Entity) => Int -> a -> ComponentStore v a -> IO ()
writeComponentAt i a store = do
    vec <- componentStoreVec <$> readIORef (unComponentStore store)
    MV.unsafeWrite vec i a

modifyComponent :: (MV.MVector v a, MV.MVector v Entity) => Entity -> (a -> a) -> ComponentStore v a -> IO Bool
modifyComponent e f store = do
    s <- readIORef (unComponentStore store)
    maybeIndex <- HT.lookup (componentStoreEntityMap s) e
    maybe (return False)
        (\i -> MV.unsafeModify (componentStoreVec s) f i >> return True)
        maybeIndex

extendComponentStore :: (MV.MVector v a, MV.MVector v Entity) => ComponentStore v a -> Int -> IO ()
extendComponentStore (ComponentStore ref) newSize = do
    s <- readIORef ref
    let currentVec = componentStoreVec s
        currentEVec = componentStoreEntityVec s
        currentSize = MV.length currentVec
    when (currentSize < newSize) $ do
        vec <- MV.new newSize
        MV.unsafeCopy (MV.unsafeTake currentSize vec) currentVec
        evec <- MV.new newSize
        MV.unsafeCopy (MV.unsafeTake currentSize evec) currentEVec
        atomicModifyIORef' ref (extendSize newSize vec evec)
    where
    extendSize n v ev state =
        let state' = state
                { componentStoreSize = n
                , componentStoreVec = v
                , componentStoreEntityVec = ev
                }
        in (state', ())
