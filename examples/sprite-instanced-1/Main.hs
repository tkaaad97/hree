{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import Example
import Foreign (Ptr)
import qualified Foreign (castPtr)
import qualified GLW
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import qualified Graphics.Hree.Geometry as Geometry
import Graphics.Hree.Geometry.Box
import Graphics.Hree.GL.Types
import Graphics.Hree.GL.Vertex
import qualified Graphics.Hree.Material as Material
import Graphics.Hree.Mesh (Mesh(..))
import Graphics.Hree.Scene
import qualified Graphics.Hree.Texture as Texture
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..), V4(..))

main :: IO ()
main = do
    withWindow width height "sprite-instanced-1" init onDisplay

    where
    width  = 640
    height = 480
    aspect = fromIntegral width / fromIntegral height

    delta :: Float
    delta = 0.2

    positions = Vector.fromList . map (\x -> V3 (fromIntegral x * delta) 0 0) $ [0..100]

    offsets = Vector.fromList
        [ V3 0 0 0
        , V3 delta 0 0
        , V3 0 delta 0
        , V3 delta delta 0
        , V3 0 delta 0
        , V3 delta 0 0
        ]

    uvs = Vector.fromList
        [ Uv $ V2 0 0
        , Uv $ V2 1 0
        , Uv $ V2 0 1
        , Uv $ V2 1 1
        , Uv $ V2 0 1
        , Uv $ V2 1 0
        ]

    proj = perspective 90 aspect 0.1 10.0

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    offsetBbs = BindBufferSetting 0 12 0
    uvBbs = BindBufferSetting 0 8 0
    positionBbs = BindBufferSetting 0 12 1

    init w = do
        GL.glEnable GL.GL_BLEND
        GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA
        scene <- newScene
        offsetBuffer <- addBuffer scene (BufferSource offsets GL.GL_STATIC_READ)
        uvBuffer <- addBuffer scene (BufferSource uvs GL.GL_STATIC_READ)
        positionBuffer <- addBuffer scene (BufferSource positions GL.GL_STATIC_READ)

        let geo0 = Geometry.newGeometry
            geo1 = Geometry.addAttribBindings geo0 0 (Map.singleton "positionOffset" (AttribBinding (GLW.BindingIndex 0) (AttribFormat 3 GL.GL_FLOAT False 0))) (offsetBuffer, offsetBbs)
            geo2 = Geometry.addAttribBindings geo1 1 (Map.singleton "uv" (AttribBinding (GLW.BindingIndex 1) (AttribFormat 2 GL.GL_FLOAT False 0))) (uvBuffer, uvBbs)
            geo3 = Geometry.addAttribBindings geo2 2 (Map.singleton "position" (AttribBinding (GLW.BindingIndex 2) (AttribFormat 3 GL.GL_FLOAT False 0))) (positionBuffer, positionBbs)
        texture <- mkTexture scene
        let material = Material.spriteMaterial texture
            mesh = Mesh geo3 material (Vector.length offsets) (Just . Vector.length $ positions)
        addMesh scene mesh
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindow' camera))
        return (scene, camera)

    onDisplay (s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (s, c) w

        where
        render = do
            renderScene s c
            GLFW.swapBuffers w

    resizeWindow' camera win w h = do
        GLW.glViewport 0 0 (fromIntegral w) (fromIntegral h)
        projection <- getCameraProjection camera
        let aspect = fromIntegral w / fromIntegral h
            projection' = updateProjectionAspectRatio projection aspect
        updateProjection camera projection'

    updateProjectionAspectRatio p @ Perspective {} aspect =
        p { perspectiveAspect = aspect }
    updateProjectionAspectRatio p _ = p

    mkTexture :: Scene -> IO Texture
    mkTexture scene = do
        let size = 1024
            settings = Texture.TextureSettings 1 GL.GL_RGBA8 (fromIntegral size) (fromIntegral size) False
            image = mkCircleImage size (V4 188 0 0 255)
        Vector.unsafeWith image $ \p -> do
            let source = Texture.TextureSourceData (fromIntegral size) (fromIntegral size) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr (V4 Word8)))
            (_, texture) <- addTexture scene "color" settings source
            (_, sampler) <- addSampler scene "sampler"
            return $ Texture (texture, sampler)
