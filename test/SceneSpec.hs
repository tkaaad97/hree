{-# LANGUAGE OverloadedStrings #-}
module SceneSpec
    ( spec
    ) where

import qualified Data.IntMap as IntMap
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import Foreign (Ptr)
import qualified Foreign (castPtr, withArray)
import GLContext
import qualified GLW
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Graphics.Hree.Geometry as Hree
import qualified Graphics.Hree.GL.Vertex as Hree
import qualified Graphics.Hree.Material as Hree
import qualified Graphics.Hree.Mesh as Hree (Mesh(..))
import qualified Graphics.Hree.Scene as Hree
import qualified Graphics.Hree.Texture as Hree
import Linear (V2(..), V3(..), V4(..))
import Test.Hspec

getSceneProp :: Hree.Scene -> (Hree.SceneState -> a) -> IO a
getSceneProp scene property = property <$> readIORef (Hree.sceneState scene)

spec :: Spec
spec = do
    describe "addMesh" $ do
        it "change scene state" . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.addVerticesToGeometry (Hree.newGeometry . Vector.length $ vs) vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material
            meshId <- Hree.addMesh scene mesh
            meshId `shouldBe` Hree.MeshId 1
            counter <- getSceneProp scene Hree.ssMeshCounter
            counter `shouldBe` 2
            bufferRefCounter <- getSceneProp scene Hree.ssBufferRefCounter
            bufferRefCounter `shouldBe` IntMap.fromList [(1, 1)]

        specify "use same geometry twice"  . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.addVerticesToGeometry (Hree.newGeometry . Vector.length $ vs) vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material
            meshId1 <- Hree.addMesh scene mesh
            meshId1 `shouldBe` Hree.MeshId 1
            meshId2 <- Hree.addMesh scene mesh
            meshId2 `shouldBe` Hree.MeshId 2
            counter <- getSceneProp scene Hree.ssMeshCounter
            counter `shouldBe` 3
            bufferRefCounter <- getSceneProp scene Hree.ssBufferRefCounter
            bufferRefCounter `shouldBe` IntMap.fromList [(1, 2)]

    describe "removeMesh" $ do
        it "remove mesh and delete buffer" . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.addVerticesToGeometry (Hree.newGeometry . Vector.length $ vs) vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material
            meshId <- Hree.addMesh scene mesh
            bufferRefCounter <- getSceneProp scene Hree.ssBufferRefCounter
            bufferRefCounter `shouldBe` IntMap.fromList [(1, 1)]
            Hree.removeMesh scene meshId
            bufferRefCounter <- getSceneProp scene Hree.ssBufferRefCounter
            bufferRefCounter `shouldBe` IntMap.fromList []

        specify "buffer will be released when all reference removed" . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.addVerticesToGeometry (Hree.newGeometry . Vector.length $ vs) vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material
            meshId1 <- Hree.addMesh scene mesh
            meshId2 <- Hree.addMesh scene mesh
            (`shouldBe` IntMap.fromList [(1, 2)]) =<< getSceneProp scene Hree.ssBufferRefCounter
            Hree.removeMesh scene meshId1
            (`shouldBe` IntMap.fromList [(1, 1)]) =<< getSceneProp scene Hree.ssBufferRefCounter
            Hree.removeMesh scene meshId2
            (`shouldBe` IntMap.fromList []) =<< getSceneProp scene Hree.ssBufferRefCounter

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
            geometry1 <- Hree.addVerticesToGeometry (Hree.newGeometry . Vector.length $ vs) vs GL.GL_STREAM_DRAW scene
            geometry2 <- Hree.addVerticesToGeometry (Hree.newGeometry . Vector.length $ vs) vs GL.GL_STREAM_DRAW scene
            let mesh1 = Hree.Mesh geometry1 material
                mesh2 = Hree.Mesh geometry2 material
            meshId1 <- Hree.addMesh scene mesh1
            meshId2 <- Hree.addMesh scene mesh2
            meshes <- getSceneProp scene Hree.ssMeshes
            IntMap.size meshes `shouldBe` 2
            bufferRefCounter <- getSceneProp scene Hree.ssBufferRefCounter
            bufferRefCounter `shouldBe` IntMap.fromList [(1, 1), (2, 1)]
            Hree.deleteScene scene
            meshesAfter <- getSceneProp scene Hree.ssMeshes
            IntMap.size meshesAfter `shouldBe` 0
            bufferRefCounterAfter <- getSceneProp scene Hree.ssBufferRefCounter
            bufferRefCounterAfter `shouldBe` IntMap.empty

        it "delete unreferenced orphan buffers" . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.addVerticesToGeometry (Hree.newGeometry . Vector.length $ vs) vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material
            bufferRefCounter <- getSceneProp scene Hree.ssBufferRefCounter
            bufferRefCounter `shouldBe` IntMap.fromList [(1, 0)]
            Hree.deleteScene scene
            bufferRefCounterAfter <- getSceneProp scene Hree.ssBufferRefCounter
            bufferRefCounterAfter `shouldBe` IntMap.empty

        it "delete textures" . runOnOSMesaContext width height . Foreign.withArray [0, 0, 0, 0] $ \p -> do
            scene <- Hree.newScene
            let textureSettings = Hree.TextureSettings 0 GL.GL_RGBA8 1 1 False
                source = Hree.TextureSourceData 1 1 PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr Word8))
            r <- Hree.addTexture scene "texture1" textureSettings source
            textures <- getSceneProp scene Hree.ssTextures
            textures `shouldBe` Map.fromList [r]
            Hree.deleteScene scene
            textures <- getSceneProp scene Hree.ssTextures
            textures `shouldBe` Map.empty

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
