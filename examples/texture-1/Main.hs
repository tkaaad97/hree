{-# LANGUAGE OverloadedStrings #-}
module Main where

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
    withWindow width height "texture-1" init onDisplay

    where
    width  = 640
    height = 480

    x = 1
    vs = Vector.fromList
        [ BasicVertex (V3 x x 0) (V3 0 0 0) (V2 1 1) (V4 0 0 0 255)
        , BasicVertex (V3 0 x 0) (V3 0 0 0) (V2 0.0 1) (V4 0 0 0 255)
        , BasicVertex (V3 x 0 0) (V3 0 0 0) (V2 1 0.0) (V4 0 0 0 255)
        , BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0.0 0.0) (V4 0 0 0 255)
        , BasicVertex (V3 x 0 0) (V3 0 0 0) (V2 1 0.0) (V4 0 0 0 255)
        , BasicVertex (V3 0 x 0) (V3 0 0 0) (V2 0.0 1) (V4 0 0 0 255)
        ]

    proj = perspective 90 1.0 0.1 10.0

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        scene <- newScene
        geometry <- Geometry.addVerticesToGeometry Geometry.newGeometry vs GL.GL_STREAM_DRAW scene
        texture <- mkTexture scene
        let material = Material.basicMaterial (Just texture)
            mesh = Mesh geometry material 6
        addMesh scene mesh
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera
        return (scene, camera)

    onDisplay (s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (s, c) w

        where
        render = do
            renderScene s c
            GLFW.swapBuffers w

    mkTexture :: Scene -> IO Texture
    mkTexture scene = do
        let size = 256
            settings = Texture.TextureSettings 1 GL.GL_RGBA8 (fromIntegral size) (fromIntegral size) False
            image = mkColoredImage size
        Vector.unsafeWith image $ \p -> do
            let source = Texture.TextureSourceData (fromIntegral size) (fromIntegral size) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr (V4 Word8)))
            (_, texture) <- addTexture scene "color" settings source
            (_, sampler) <- addSampler scene "sampler"
            return $ Texture (texture, sampler)
