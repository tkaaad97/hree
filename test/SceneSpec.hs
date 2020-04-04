{-# LANGUAGE OverloadedStrings #-}
module SceneSpec
    ( spec
    ) where

import qualified Data.Component as Component
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as BV
import qualified Data.Vector.Storable as SV
import Data.Word (Word8)
import Foreign (Ptr)
import qualified Foreign (castPtr, withArray)
import GLContext
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified GLW.Internal.Objects as GLW (Buffer(..))
import qualified Graphics.GL as GL
import qualified Graphics.Hree.Geometry as Hree
import qualified Graphics.Hree.GL.Vertex as Hree
import qualified Graphics.Hree.Material as Hree
import qualified Graphics.Hree.Mesh as Hree (Mesh(..))
import qualified Graphics.Hree.Scene as Hree
import qualified Graphics.Hree.Texture as Hree
import qualified Graphics.Hree.Types as Hree
import Linear (V2(..), V3(..), V4(..))
import Test.Hspec

getSceneProp :: Hree.Scene -> (Hree.SceneState -> a) -> IO a
getSceneProp scene property = property <$> readIORef (Hree.sceneState scene)

spec :: Spec
spec = do
    describe "addMesh" $ do
        it "change scene state" . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.addVerticesToGeometry Hree.newGeometry vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material Nothing
            meshId <- Hree.addMesh scene mesh
            meshId `shouldBe` Hree.MeshId 1
            counter <- getSceneProp scene Hree.ssMeshCounter
            counter `shouldBe` 2
            buffers <- getSceneProp scene Hree.ssBuffers
            buffers `shouldBe` [GLW.Buffer 1]

        specify "use same geometry twice"  . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.addVerticesToGeometry Hree.newGeometry vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material Nothing
            meshId1 <- Hree.addMesh scene mesh
            meshId1 `shouldBe` Hree.MeshId 1
            meshId2 <- Hree.addMesh scene mesh
            meshId2 `shouldBe` Hree.MeshId 2
            counter <- getSceneProp scene Hree.ssMeshCounter
            counter `shouldBe` 3
            buffers <- getSceneProp scene Hree.ssBuffers
            buffers `shouldBe` [GLW.Buffer 1]

    describe "removeMesh" $ do
        it "remove mesh" . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.addVerticesToGeometry Hree.newGeometry vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material Nothing
            meshId <- Hree.addMesh scene mesh
            (`shouldBe` [GLW.Buffer 1]) =<< getSceneProp scene Hree.ssBuffers
            Hree.removeMesh scene meshId
            (`shouldBe` [GLW.Buffer 1]) =<< getSceneProp scene Hree.ssBuffers

    describe "addTexture" $ do
        it "change scene state" . runOnOSMesaContext width height . Foreign.withArray [0, 0, 0, 0] $ \p -> do
            scene <- Hree.newScene
            let textureSettings = Hree.TextureSettings 0 GL.GL_RGBA8 1 1 False
                source = Hree.TextureSourceData 1 1 PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr Word8))
            r <- Hree.addTexture scene "texture1" textureSettings source
            textures <- getSceneProp scene Hree.ssTextures
            textures `shouldBe` Map.fromList [r]

        it "use a random name on name conflict" . runOnOSMesaContext width height . Foreign.withArray [0, 0, 0, 0] $ \p -> do
            scene <- Hree.newScene
            let textureSettings = Hree.TextureSettings 0 GL.GL_RGBA8 1 1 False
                source = Hree.TextureSourceData 1 1 PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr Word8))
            r1 <- Hree.addTexture scene "texture" textureSettings source
            r2 <- Hree.addTexture scene "texture" textureSettings source
            textures <- getSceneProp scene Hree.ssTextures
            Map.size textures `shouldBe` 2
            textures `shouldBe` Map.fromList [r1, r2]

    describe "deleteScene" $ do
        it "delete meshes" . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry1 <- Hree.addVerticesToGeometry Hree.newGeometry vs GL.GL_STREAM_DRAW scene
            geometry2 <- Hree.addVerticesToGeometry Hree.newGeometry vs GL.GL_STREAM_DRAW scene
            let mesh1 = Hree.Mesh geometry1 material Nothing
                mesh2 = Hree.Mesh geometry2 material Nothing
            _ <- Hree.addMesh scene mesh1
            _ <- Hree.addMesh scene mesh2
            meshSize <- Component.componentSize $ Hree.sceneMeshStore scene
            meshSize `shouldBe` 2
            (`shouldBe` [GLW.Buffer 2, GLW.Buffer 1]) =<< getSceneProp scene Hree.ssBuffers
            Hree.deleteScene scene
            meshSizeAfter <- Component.componentSize $ Hree.sceneMeshStore scene
            meshSizeAfter `shouldBe` 0
            (`shouldBe` []) =<< getSceneProp scene Hree.ssBuffers

        it "delete textures" . runOnOSMesaContext width height . Foreign.withArray [0, 0, 0, 0] $ \p -> do
            scene <- Hree.newScene
            let textureSettings = Hree.TextureSettings 0 GL.GL_RGBA8 1 1 False
                source = Hree.TextureSourceData 1 1 PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr Word8))
            r <- Hree.addTexture scene "texture1" textureSettings source
            (`shouldBe` Map.fromList [r]) =<< getSceneProp scene Hree.ssTextures
            Hree.deleteScene scene
            (`shouldBe` Map.empty) =<< getSceneProp scene Hree.ssTextures

    describe "addNode" $ do
        it "add node" . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            let node1 = Hree.newNode { Hree.nodeTranslation = V3 0.5 0 0 }
            node1Id <- Hree.addNode scene node1 False
            let node2 = Hree.newNode { Hree.nodeChildren = BV.singleton node1Id }
            node2Id <- Hree.addNode scene node2 True
            (`shouldBe` Just node1) =<< Hree.readNode scene node1Id
            (`shouldBe` Just node2) =<< Hree.readNode scene node2Id
            (`shouldBe` BV.singleton node2Id) =<< getSceneProp scene Hree.ssRootNodes

    describe "removeNode" $ do
        it "remoove node" . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            let node1 = Hree.newNode { Hree.nodeTranslation = V3 0.5 0 0 }
            node1Id <- Hree.addNode scene node1 False
            let node2 = Hree.newNode { Hree.nodeChildren = BV.singleton node1Id }
            node2Id <- Hree.addNode scene node2 True
            node3Id <- Hree.addNode scene Hree.newNode True
            (`shouldBe` Just node1) =<< Hree.readNode scene node1Id
            (`shouldBe` Just node2) =<< Hree.readNode scene node2Id
            (`shouldBe` BV.fromList [node2Id, node3Id]) =<< getSceneProp scene Hree.ssRootNodes
            Hree.removeNode scene node2Id
            (`shouldBe` Nothing) =<< Hree.readNode scene node1Id
            (`shouldBe` Nothing) =<< Hree.readNode scene node2Id
            (`shouldBe` Just Hree.newNode) =<< Hree.readNode scene node3Id
            (`shouldBe` BV.fromList [node3Id]) =<< getSceneProp scene Hree.ssRootNodes
    where
    width = 1
    height = 1

    x = 1
    vs = SV.fromList
        [ Hree.BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , Hree.BasicVertex (V3 x x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , Hree.BasicVertex (V3 0 x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        ]

    material = Hree.testMaterial
