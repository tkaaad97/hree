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

    spriteVertices = Vector.fromList
        $  map (\x -> SpriteVertex (V3 (fromIntegral x * delta) 0 0) (V3 delta delta 0) 0 (V2 0 0) (V2 1 1)) [0..9]
        ++ map (\x -> SpriteVertex (V3 (fromIntegral x * delta * 2) delta 0) (V3 (delta * 2) delta 0) 0 (V2 0 0) (V2 1 1)) [0..4]
        ++ map (\x -> SpriteVertex (V3 (fromIntegral x * delta * 2) (delta * 2) 0) (V3 (delta * 2) delta 0) (pi * 0.2) (V2 0 0) (V2 1 1)) [0..4]

    proj = perspective 90 aspect 0.1 10.0

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        GL.glEnable GL.GL_BLEND
        GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA
        scene <- newScene

        (geo, _) <- Geometry.newSpriteGeometry scene
        geo' <- Geometry.addVerticesToGeometry geo spriteVertices GL.GL_STATIC_READ scene
        texture <- mkTexture scene
        let material = Material.spriteMaterial texture
            mesh = Mesh geo' material (Just . Vector.length $ spriteVertices)
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
