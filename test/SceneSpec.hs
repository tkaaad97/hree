module SceneSpec
    ( spec
    ) where

import qualified Data.IntMap as IntMap
import Data.IORef (readIORef)
import qualified Data.Vector.Storable as Vector
import GLContext
import qualified GLW
import qualified Graphics.GL as GL
import qualified Graphics.Hree.GL.Vertex as Hree
import qualified Graphics.Hree.Material as Hree
import qualified Graphics.Hree.Mesh as Hree (Mesh(..))
import qualified Graphics.Hree.Scene as Hree
import Linear (V2(..), V3(..), V4(..))
import Test.Hspec

spec :: Spec
spec = do
    describe "addMesh" $ do
        it "change scene state" . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.geometryFromVertexVector (GLW.BindingIndex 0) vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material
            meshId <- Hree.addMesh scene mesh
            meshId `shouldBe` 1
            counter <- readIORef . Hree.sceneMeshCounter $ scene
            counter `shouldBe` 2
            bufferRefCounter <- readIORef . Hree.sceneBufferRefCounter $ scene
            bufferRefCounter `shouldBe` IntMap.fromList [(1, 1)]

        specify "use same geometry twice"  . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.geometryFromVertexVector (GLW.BindingIndex 0) vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material
            meshId1 <- Hree.addMesh scene mesh
            meshId1 `shouldBe` 1
            meshId2 <- Hree.addMesh scene mesh
            meshId2 `shouldBe` 2
            counter <- readIORef . Hree.sceneMeshCounter $ scene
            counter `shouldBe` 3
            bufferRefCounter <- readIORef . Hree.sceneBufferRefCounter $ scene
            bufferRefCounter `shouldBe` IntMap.fromList [(1, 2)]

    describe "removeMesh" $ do
        it "remove mesh and delete buffer" . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.geometryFromVertexVector (GLW.BindingIndex 0) vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material
            meshId <- Hree.addMesh scene mesh
            bufferRefCounter <- readIORef . Hree.sceneBufferRefCounter $ scene
            bufferRefCounter `shouldBe` IntMap.fromList [(1, 1)]
            Hree.removeMesh scene meshId
            bufferRefCounter <- readIORef . Hree.sceneBufferRefCounter $ scene
            bufferRefCounter `shouldBe` IntMap.fromList []

        specify "buffer will be released when all reference removed"  . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.geometryFromVertexVector (GLW.BindingIndex 0) vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material
            meshId1 <- Hree.addMesh scene mesh
            meshId2 <- Hree.addMesh scene mesh
            (`shouldBe` IntMap.fromList [(1, 2)]) =<< readIORef (Hree.sceneBufferRefCounter scene)
            Hree.removeMesh scene meshId1
            (`shouldBe` IntMap.fromList [(1, 1)]) =<< readIORef (Hree.sceneBufferRefCounter scene)
            Hree.removeMesh scene meshId2
            (`shouldBe` IntMap.fromList []) =<< readIORef (Hree.sceneBufferRefCounter scene)

    where
    width = 1
    height = 1

    x = 1
    vs = Vector.fromList
        [ Hree.BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , Hree.BasicVertex (V3 x x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , Hree.BasicVertex (V3 0 x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        ]

    material = Hree.testMaterial
