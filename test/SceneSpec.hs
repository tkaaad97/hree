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
import qualified Foreign (alloca, castPtr, withArray)
import GLContext
import qualified GLW
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified GLW.Internal.Objects as GLW (Buffer(..))
import qualified Graphics.GL as GL
import qualified Graphics.Hree.Geometry as Hree
import qualified Graphics.Hree.GL.Vertex as Hree
import qualified Graphics.Hree.Material.TestMaterial as Hree
import qualified Graphics.Hree.Mesh as Hree (Mesh(..))
import qualified Graphics.Hree.Scene as Hree
import qualified Graphics.Hree.Texture as Hree
import qualified Graphics.Hree.Types as Hree
import Linear (V2(..), V3(..), V4(..))
import Test.Hspec

getSceneProp :: Hree.Scene -> (Hree.SceneState -> a) -> IO a
getSceneProp scene property = property <$> readIORef (Hree.sceneState scene)

assertBufferAlive :: GLW.Buffer -> IO ()
assertBufferAlive buffer = do
    GL.glGetError >>= (`shouldBe` GL.GL_NO_ERROR)
    Foreign.alloca $ GLW.glGetNamedBufferParameteriv buffer GL.GL_BUFFER_USAGE
    GL.glGetError >>= (`shouldBe` GL.GL_NO_ERROR)

assertBufferDead :: GLW.Buffer -> IO ()
assertBufferDead buffer = do
    GL.glGetError >>= (`shouldBe` GL.GL_NO_ERROR)
    Foreign.alloca $ GLW.glGetNamedBufferParameteriv buffer GL.GL_BUFFER_USAGE
    GL.glGetError >>= (`shouldBe` GL.GL_INVALID_OPERATION)

spec :: Spec
spec = do
    describe "addMesh" $ do
        runOnOSMesaContext width height . it "change scene state" $ do
            scene <- Hree.newScene
            let geometry = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                mesh = Hree.Mesh geometry material Nothing
            meshId <- Hree.addedMeshId <$> Hree.addMesh scene mesh
            meshId `shouldBe` Hree.MeshId 1
            counter <- getSceneProp scene Hree.ssMeshCounter
            counter `shouldBe` 2
            assertBufferAlive (GLW.Buffer 1)
            assertBufferAlive (GLW.Buffer 2)

        runOnOSMesaContext width height . specify "use same geometry twice" $ do
            scene <- Hree.newScene
            let geometry = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                mesh = Hree.Mesh geometry material Nothing
            meshId1 <- Hree.addedMeshId <$> Hree.addMesh scene mesh
            meshId1 `shouldBe` Hree.MeshId 1
            meshId2 <- Hree.addedMeshId <$> Hree.addMesh scene mesh
            meshId2 `shouldBe` Hree.MeshId 2
            counter <- getSceneProp scene Hree.ssMeshCounter
            counter `shouldBe` 3
            assertBufferAlive (GLW.Buffer 1)
            assertBufferAlive (GLW.Buffer 2)
            assertBufferAlive (GLW.Buffer 3)
            assertBufferAlive (GLW.Buffer 4)

    describe "removeMesh" $ do
        runOnOSMesaContext width height . it "remove mesh" $ do
            scene <- Hree.newScene
            let geometry = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                mesh = Hree.Mesh geometry material Nothing
            meshId <- Hree.addedMeshId <$> Hree.addMesh scene mesh
            assertBufferAlive (GLW.Buffer 1)
            assertBufferAlive (GLW.Buffer 2)
            Hree.removeMesh scene meshId
            assertBufferDead (GLW.Buffer 1)
            assertBufferDead (GLW.Buffer 2)

    describe "addTexture" $ do
        runOnOSMesaContext width height . it "change scene state" . Foreign.withArray [0, 0, 0, 0] $ \p -> do
            scene <- Hree.newScene
            let textureSettings = Hree.TextureSettings 0 GL.GL_RGBA8 1 1 False
                source = Hree.TextureSourceData 1 1 PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr Word8))
            r <- Hree.addTexture scene "texture1" textureSettings source
            textures <- getSceneProp scene Hree.ssTextures
            textures `shouldBe` Map.fromList [r]

        runOnOSMesaContext width height . it "use a random name on name conflict" . Foreign.withArray [0, 0, 0, 0] $ \p -> do
            scene <- Hree.newScene
            let textureSettings = Hree.TextureSettings 0 GL.GL_RGBA8 1 1 False
                source = Hree.TextureSourceData 1 1 PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr Word8))
            r1 <- Hree.addTexture scene "texture" textureSettings source
            r2 <- Hree.addTexture scene "texture" textureSettings source
            textures <- getSceneProp scene Hree.ssTextures
            Map.size textures `shouldBe` 2
            textures `shouldBe` Map.fromList [r1, r2]

    describe "deleteScene" $ do
        runOnOSMesaContext width height . it "delete meshes" $ do
            scene <- Hree.newScene
            let geometry1 = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                geometry2 = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                mesh1 = Hree.Mesh geometry1 material Nothing
                mesh2 = Hree.Mesh geometry2 material Nothing
            _ <- Hree.addMesh scene mesh1
            _ <- Hree.addMesh scene mesh2
            meshSize <- Component.componentSize $ Hree.sceneMeshStore scene
            meshSize `shouldBe` 2
            assertBufferAlive (GLW.Buffer 1)
            assertBufferAlive (GLW.Buffer 2)
            assertBufferAlive (GLW.Buffer 3)
            assertBufferAlive (GLW.Buffer 4)
            Hree.deleteScene scene
            meshSizeAfter <- Component.componentSize $ Hree.sceneMeshStore scene
            meshSizeAfter `shouldBe` 0
            assertBufferDead (GLW.Buffer 1)
            assertBufferDead (GLW.Buffer 2)
            assertBufferDead (GLW.Buffer 3)
            assertBufferDead (GLW.Buffer 4)

        runOnOSMesaContext width height . it "delete textures" . Foreign.withArray [0, 0, 0, 0] $ \p -> do
            scene <- Hree.newScene
            let textureSettings = Hree.TextureSettings 0 GL.GL_RGBA8 1 1 False
                source = Hree.TextureSourceData 1 1 PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr Word8))
            r <- Hree.addTexture scene "texture1" textureSettings source
            (`shouldBe` Map.fromList [r]) =<< getSceneProp scene Hree.ssTextures
            Hree.deleteScene scene
            (`shouldBe` Map.empty) =<< getSceneProp scene Hree.ssTextures

    describe "addNode" $ do
        runOnOSMesaContext width height . it "add node" $ do
            scene <- Hree.newScene
            let node1 = Hree.newNode { Hree.nodeTranslation = V3 0.5 0 0 }
            node1Id <- Hree.addNode scene node1 False
            let node2 = Hree.newNode { Hree.nodeChildren = BV.singleton node1Id }
            node2Id <- Hree.addNode scene node2 True
            (`shouldBe` Just node1) =<< Hree.readNode scene node1Id
            (`shouldBe` Just node2) =<< Hree.readNode scene node2Id
            (`shouldBe` BV.singleton node2Id) =<< getSceneProp scene Hree.ssRootNodes

    describe "removeNode" $ do
        runOnOSMesaContext width height . it "remoove node" $ do
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
