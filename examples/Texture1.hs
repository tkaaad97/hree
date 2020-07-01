{-# LANGUAGE OverloadedStrings #-}
module Texture1 where

import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import Example
import Foreign (Ptr)
import qualified Foreign (castPtr)
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Graphics.Hree as Hree
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..), V4(..))
import Prelude hiding (init)

main :: IO ()
main =
    withWindow width height "texture-1" init onDisplay

    where
    width  = 640
    height = 480

    x = 1
    vs = Vector.fromList
        [ Hree.BasicVertex (V3 x x 0) (V3 0 0 1) (V2 1 1) (V4 255 255 255 255)
        , Hree.BasicVertex (V3 0 x 0) (V3 0 0 1) (V2 0.0 1) (V4 255 255 255 255)
        , Hree.BasicVertex (V3 x 0 0) (V3 0 0 1) (V2 1 0.0) (V4 255 255 255 255)
        , Hree.BasicVertex (V3 0 0 0) (V3 0 0 1) (V2 0.0 0.0) (V4 255 255 255 255)
        , Hree.BasicVertex (V3 x 0 0) (V3 0 0 1) (V2 1 0.0) (V4 255 255 255 255)
        , Hree.BasicVertex (V3 0 x 0) (V3 0 0 1) (V2 0.0 1) (V4 255 255 255 255)
        ]

    proj = Hree.perspective 90 1.0 0.1 10.0

    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        geometry <- Hree.addVerticesToGeometry Hree.newGeometry vs GL.GL_STREAM_DRAW scene
        texture <- mkTexture scene
        let material = (Hree.basicMaterial (V3 0 0 (-1)))
                { Hree.materialTextures = pure (Hree.BaseColorMapping, texture)
                }
            mesh = Hree.Mesh geometry material Nothing
        meshId <- Hree.addedMeshId <$> Hree.addMesh scene mesh
        _ <- Hree.addNode scene Hree.newNode{ Hree.nodeMesh = Just meshId } True
        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera
        return (renderer, scene, camera)

    onDisplay (r, s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (r, s, c) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w

    mkTexture :: Hree.Scene -> IO Hree.Texture
    mkTexture scene = do
        let size = 256
            settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral size) (fromIntegral size) False
            image = mkColoredImage size
        Vector.unsafeWith image $ \p -> do
            let source = Hree.TextureSourceData (fromIntegral size) (fromIntegral size) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr (V4 Word8)))
            (_, texture) <- Hree.addTexture scene "color" settings source
            (_, sampler) <- Hree.addSampler scene "sampler"
            return $ Hree.Texture (texture, sampler)
