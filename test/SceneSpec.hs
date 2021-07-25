{-# LANGUAGE DataKinds         #-}
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
import qualified Foreign
import GLContext
import qualified GLW
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified GLW.Internal.Objects as GLW (Buffer(..), Texture(..))
import qualified Graphics.GL as GL
import qualified Graphics.Hree as Hree
import qualified Graphics.Hree.GL.Texture as Hree
import qualified Graphics.Hree.GL.Vertex as Hree
import qualified Graphics.Hree.Material.TestMaterial as Hree
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

assertTextureAlive :: GLW.Texture 'GLW.GL_TEXTURE_2D -> IO ()
assertTextureAlive texture = do
    GL.glGetError >>= (`shouldBe` GL.GL_NO_ERROR)
    _ <- Hree.getTextureParameter texture Hree.glTextureBaseLevel
    GL.glGetError >>= (`shouldBe` GL.GL_NO_ERROR)

assertTextureDead :: GLW.Texture 'GLW.GL_TEXTURE_2D -> IO ()
assertTextureDead texture = do
    GL.glGetError >>= (`shouldBe` GL.GL_NO_ERROR)
    _ <- Hree.getTextureParameter texture Hree.glTextureBaseLevel
    GL.glGetError >>= (`shouldBe` GL.GL_INVALID_OPERATION)

spec :: Spec
spec = do
    describe "addMaterial" $ do
        runOnOSMesaContext width height . it "change scene state" $ do
            scene <- Hree.newScene
            materialId <- Hree.addMaterial scene material
            materialId `shouldBe` Hree.MaterialId 1
            counter <- getSceneProp scene Hree.ssMaterialCounter
            counter `shouldBe` 2

        runOnOSMesaContext width height . specify "allocate texture if mapping exists" $ do
            scene <- Hree.newScene
            mapping <- mkMapping
            materialId <- Hree.addMaterial scene material { Hree.materialMappings = pure (Hree.BaseColorMapping, mapping) }
            assertTextureAlive (GLW.Texture 1)

            Hree.removeMaterialIfUnused scene materialId
            assertTextureDead (GLW.Texture 1)

    describe "deleteMaterialIfUnused" $ do
        runOnOSMesaContext width height . it "should not delete if used" $ do
            scene <- Hree.newScene
            mapping <- mkMapping
            materialId <- Hree.addMaterial scene material { Hree.materialMappings = pure (Hree.BaseColorMapping, mapping) }
            materialSize <- Component.componentSize $ Hree.sceneMaterialStore scene
            materialSize `shouldBe` 1
            let geometry = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                mesh = Hree.mesh geometry materialId
            meshId <- Hree.addMesh scene mesh

            -- should not be deleted at here
            Hree.removeMaterialIfUnused scene materialId
            materialSize <- Component.componentSize $ Hree.sceneMaterialStore scene
            materialSize `shouldBe` 1
            assertTextureAlive (GLW.Texture 1)

            Hree.removeMesh scene meshId
            Hree.removeMaterialIfUnused scene materialId

            -- should be deleted at here
            materialSize <- Component.componentSize $ Hree.sceneMaterialStore scene
            materialSize `shouldBe` 0
            assertTextureDead (GLW.Texture 1)

    describe "addMesh" $ do
        runOnOSMesaContext width height . it "change scene state" $ do
            scene <- Hree.newScene
            materialId <- Hree.addMaterial scene material
            let geometry = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                mesh = Hree.mesh geometry materialId
            meshId <- Hree.addMesh scene mesh
            meshId `shouldBe` Hree.MeshId 1
            counter <- getSceneProp scene Hree.ssMeshCounter
            counter `shouldBe` 2
            assertBufferAlive (GLW.Buffer 1)
            assertBufferAlive (GLW.Buffer 2)

        runOnOSMesaContext width height . specify "use same geometry twice" $ do
            scene <- Hree.newScene
            materialId <- Hree.addMaterial scene material
            let geometry = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                mesh = Hree.mesh geometry materialId
            meshId1 <- Hree.addMesh scene mesh
            meshId1 `shouldBe` Hree.MeshId 1
            meshId2 <- Hree.addMesh scene mesh
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
            materialId <- Hree.addMaterial scene material
            let geometry = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                mesh = Hree.mesh geometry materialId
            meshId <- Hree.addMesh scene mesh
            assertBufferAlive (GLW.Buffer 1)
            assertBufferAlive (GLW.Buffer 2)
            Hree.removeMesh scene meshId
            assertBufferDead (GLW.Buffer 1)
            assertBufferDead (GLW.Buffer 2)

    describe "addSkin" $ do
        runOnOSMesaContext width height . it "allocate two buffers" $ do
            scene <- Hree.newScene

            let node1 = Hree.node { Hree.nodeTranslation = V3 0.5 0 0 }
            node1Id <- Hree.addNode scene node1 Nothing False
            let node2 = Hree.node { Hree.nodeChildren = pure node1Id }
            node2Id <- Hree.addNode scene node2 Nothing True

            skinId <- Hree.addSkin scene node2Id (SV.singleton node1Id) mempty

            counter <- getSceneProp scene Hree.ssSkinCounter
            counter `shouldBe` 2
            assertBufferAlive (GLW.Buffer 1)
            assertBufferAlive (GLW.Buffer 2)

            Hree.removeSkinIfUnused scene skinId
            assertBufferDead (GLW.Buffer 1)
            assertBufferDead (GLW.Buffer 2)

    describe "deleteScene" $ do
        runOnOSMesaContext width height . it "delete meshes and materials and default texture" $ do
            scene <- Hree.newScene

            -- default texture
            _ <- Hree.mkDefaultTextureIfNotExists scene
            assertTextureAlive (GLW.Texture 1)

            -- add a material
            mapping <- mkMapping
            materialId <- Hree.addMaterial scene material { Hree.materialMappings = pure (Hree.BaseColorMapping, mapping) }
            assertTextureAlive (GLW.Texture 2)
            materialSize <- Component.componentSize $ Hree.sceneMaterialStore scene
            materialSize `shouldBe` 1

            -- add meshes
            let geometry1 = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                geometry2 = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                mesh1 = Hree.mesh geometry1 materialId
                mesh2 = Hree.mesh geometry2 materialId
            mesh1Id <- Hree.addMesh scene mesh1
            _ <- Hree.addMesh scene mesh2
            meshSize <- Component.componentSize $ Hree.sceneMeshStore scene
            meshSize `shouldBe` 2
            assertBufferAlive (GLW.Buffer 1)
            assertBufferAlive (GLW.Buffer 2)
            assertBufferAlive (GLW.Buffer 3)
            assertBufferAlive (GLW.Buffer 4)

            -- add a node
            node1Id <- Hree.addNode scene Hree.node (Just mesh1Id) True
            _ <- Hree.addNodeUniformBlock scene node1Id (Hree.UniformBufferBindingIndex 8) ()
            assertBufferAlive (GLW.Buffer 5)
            node2Id <- Hree.addNode scene Hree.node { Hree.nodeChildren = pure node1Id } Nothing True

            -- add a skin
            _ <- Hree.addSkin scene node2Id (SV.singleton node1Id) mempty
            assertBufferAlive (GLW.Buffer 6)
            assertBufferAlive (GLW.Buffer 7)

            Hree.deleteScene scene

            materialSizeAfter <- Component.componentSize $ Hree.sceneMaterialStore scene
            materialSizeAfter `shouldBe` 0
            meshSizeAfter <- Component.componentSize $ Hree.sceneMeshStore scene
            meshSizeAfter `shouldBe` 0

            assertBufferDead (GLW.Buffer 1)
            assertBufferDead (GLW.Buffer 2)
            assertBufferDead (GLW.Buffer 3)
            assertBufferDead (GLW.Buffer 4)
            assertBufferDead (GLW.Buffer 5)
            assertBufferDead (GLW.Buffer 6)
            assertBufferDead (GLW.Buffer 7)

            assertTextureDead (GLW.Texture 1)
            assertTextureDead (GLW.Texture 2)

    describe "addNode" $ do
        runOnOSMesaContext width height . it "add node" $ do
            scene <- Hree.newScene
            let node1 = Hree.node { Hree.nodeTranslation = V3 0.5 0 0 }
            node1Id <- Hree.addNode scene node1 Nothing False
            let node2 = Hree.node { Hree.nodeChildren = BV.singleton node1Id }
            node2Id <- Hree.addNode scene node2 Nothing True
            (`shouldBe` Just node1) =<< Hree.readNode scene node1Id
            (`shouldBe` Just node2) =<< Hree.readNode scene node2Id
            (`shouldBe` BV.singleton node2Id) =<< getSceneProp scene Hree.ssRootNodes

    describe "removeNode" $ do
        runOnOSMesaContext width height . it "remoove node" $ do
            scene <- Hree.newScene
            let node1 = Hree.node { Hree.nodeTranslation = V3 0.5 0 0 }
            node1Id <- Hree.addNode scene node1 Nothing False
            let node2 = Hree.node { Hree.nodeChildren = BV.singleton node1Id }
            node2Id <- Hree.addNode scene node2 Nothing True
            node3Id <- Hree.addNode scene Hree.node Nothing True
            (`shouldBe` Just node1) =<< Hree.readNode scene node1Id
            (`shouldBe` Just node2) =<< Hree.readNode scene node2Id
            (`shouldBe` BV.fromList [node2Id, node3Id]) =<< getSceneProp scene Hree.ssRootNodes
            Hree.removeNode scene node2Id
            (`shouldBe` Nothing) =<< Hree.readNode scene node1Id
            (`shouldBe` Nothing) =<< Hree.readNode scene node2Id
            (`shouldBe` Just Hree.node) =<< Hree.readNode scene node3Id
            (`shouldBe` BV.fromList [node3Id]) =<< getSceneProp scene Hree.ssRootNodes

    describe "renderScene" $ do
        runOnOSMesaContext width height . it "allocate camera and light uniform block" $ do
            renderer <- Hree.newRenderer
            scene <- Hree.newScene

            let
                proj = Hree.orthographic (-100) 100 (-1.0) 1.0 0.01 10.0
                la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)
            camera <- Hree.newCamera proj la

            materialId <- Hree.addMaterial scene material
            let geometry1 = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                mesh1 = Hree.mesh geometry1 materialId
            mesh1Id <- Hree.addMesh scene mesh1
            assertBufferAlive (GLW.Buffer 1)
            assertBufferAlive (GLW.Buffer 2)
            assertBufferDead (GLW.Buffer 3)
            assertBufferDead (GLW.Buffer 4)

            node1Id <- Hree.addNode scene Hree.node (Just mesh1Id) True

            Hree.renderScene renderer scene camera
            assertBufferAlive (GLW.Buffer 3)
            assertBufferAlive (GLW.Buffer 4)

            Hree.deleteScene scene
            assertBufferDead (GLW.Buffer 1)
            assertBufferDead (GLW.Buffer 2)
            assertBufferDead (GLW.Buffer 3)
            assertBufferDead (GLW.Buffer 4)
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

    mkMapping = do
        p <- Foreign.newArray [255, 255, 255, 255]
        pixels <- Foreign.newForeignPtr_ $ Foreign.castPtr (p :: Ptr Word8)
        let settings = Hree.TextureSettings 1 GL.GL_RGBA8 1 1 False
            sourceData = Hree.TextureSourceData 1 1 PixelFormat.glRgba GL.GL_UNSIGNED_BYTE pixels
        return (Hree.MappingSource settings sourceData [])
