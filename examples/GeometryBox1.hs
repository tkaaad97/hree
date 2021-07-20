{-# LANGUAGE OverloadedStrings #-}
module GeometryBox1 where

import qualified Data.Vector.Storable as Vector
import Example
import qualified Foreign
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Graphics.Hree as Hree
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..))
import Prelude hiding (init)

main :: IO ()
main =
    withWindow width height "geometry-box-1" init onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    uvs =
        [ Hree.Uv $ V2 0 0
        , Hree.Uv $ V2 1 0
        , Hree.Uv $ V2 0 1
        , Hree.Uv $ V2 1 1
        , Hree.Uv $ V2 0 1
        , Hree.Uv $ V2 1 0
        ]
    vs = Vector.fromList . concat . replicate 6 $ uvs

    proj = Hree.perspective 90 defaultAspect 0.1 10.0

    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        let geometry = Hree.addVerticesToGeometry (Hree.boxGeometry 0.5 0.5 0.5) vs GL.GL_STATIC_READ
        mapping <- mkMappingSource
        let material = (Hree.basicMaterial (V3 0.5 (-1) (-1)))
                { Hree.materialMappings = pure (Hree.BaseColorMapping, mapping)
                }
        materialId <- Hree.addMaterial scene material
        let mesh = Hree.Mesh geometry materialId Nothing
        meshId <- Hree.addMesh scene mesh
        _ <- Hree.addNode scene Hree.newNode (Just meshId) True
        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera)

    onDisplay (r, s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (r, s, c) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w

    mkMappingSource :: IO Hree.MappingSource
    mkMappingSource = do
        let size = 256
            settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral size) (fromIntegral size) False
            image = mkColoredImage size
            byteSize = size * size * 4
        pixels <- Foreign.mallocForeignPtrBytes byteSize
        let source = Hree.TextureSourceData (fromIntegral size) (fromIntegral size) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE pixels
        Foreign.withForeignPtr pixels $ \dest ->
            Vector.unsafeWith image $ \p -> do
                Foreign.copyBytes (Foreign.castPtr dest) p byteSize
        return (Hree.MappingSource settings source [])
