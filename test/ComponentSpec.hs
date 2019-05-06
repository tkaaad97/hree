module ComponentSpec
    ( spec
    ) where

import qualified Data.HashTable.IO as HT
import Data.IORef (readIORef)
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Storable.Mutable as MV
import Graphics.Hree.Component
import Linear (V3(..))
import Test.Hspec

spec :: Spec
spec = do
    describe "newComponentStore" $ do
        it "preserve store vectors" $ do
            let preserve = 10
            store <- newComponentStore preserve (Proxy :: Proxy (V3 Double))
            state <- readIORef (unComponentStore store)
            componentStoreSize state `shouldBe` 0
            MV.length (componentStoreVec state) `shouldBe` preserve
            MV.length (componentStoreEntityVec state) `shouldBe` preserve

    describe "addComponent" $ do
        it "add new component" $ do
            let preserve = 10
                entity1 = Entity 1
                component1 = V3 1 2 3
                entity2 = Entity 2
                component2 = V3 4 5 6
            store <- newComponentStore preserve (Proxy :: Proxy (V3 Double))
            addComponent entity1 component1 store
            addComponent entity2 component2 store
            c1 <- readComponent entity1 store
            c1 `shouldBe` Just component1
            c2 <- readComponent entity2 store
            c2 `shouldBe` Just component2
            size <- componentSize store
            size `shouldBe` 2

        it "extend size when no space" $ do
            let preserve = 1
                entity1 = Entity 1
                component1 = V3 1 2 3
                entity2 = Entity 2
                component2 = V3 4 5 6
            store <- newComponentStore preserve (Proxy :: Proxy (V3 Double))

            addComponent entity1 component1 store
            c1 <- readComponent entity1 store
            c1 `shouldBe` Just component1
            state1 <- readIORef (unComponentStore store)
            MV.length (componentStoreVec state1) `shouldBe` preserve

            addComponent entity2 component2 store
            state2 <- readIORef (unComponentStore store)
            MV.length (componentStoreVec state2) `shouldSatisfy` (> preserve)
            c2 <- readComponent entity2 store
            c2 `shouldBe` Just component2

    describe "removeComponent" $ do
        it "remove component" $ do
            let preserve = 10
                entity1 = Entity 1
                component1 = V3 1 2 3
            store <- newComponentStore preserve (Proxy :: Proxy (V3 Double))
            addComponent entity1 component1 store
            c1 <- readComponent entity1 store
            c1 `shouldBe` Just component1
            removed <- removeComponent entity1 store
            removed `shouldBe` True
            size <- componentSize store
            size `shouldBe` 0

        it "relocate component" $ do
            let preserve = 10
                entity1 = Entity 1
                component1 = V3 1 2 3
                entity2 = Entity 2
                component2 = V3 4 5 6
                entity3 = Entity 3
                component3 = V3 7 8 9
            store <- newComponentStore preserve (Proxy :: Proxy (V3 Double))

            addComponent entity1 component1 store
            addComponent entity2 component2 store
            addComponent entity3 component3 store
            removeComponent entity1 store

            state <- readIORef (unComponentStore store)
            let size = componentStoreSize state
                vec = componentStoreVec state
                evec = componentStoreEntityVec state
                emap = componentStoreEntityMap state
            size `shouldBe` 2
            flip shouldBe component3 =<< MV.read vec 0
            flip shouldBe component2 =<< MV.read vec 1
            flip shouldBe entity3 =<< MV.read evec 0
            flip shouldBe entity2 =<< MV.read evec 1
            flip shouldBe (Just 0) =<< HT.lookup emap entity3
