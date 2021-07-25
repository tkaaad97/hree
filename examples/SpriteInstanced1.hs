{-# LANGUAGE OverloadedStrings #-}
module SpriteInstanced1 where

import qualified Data.Vector.Storable as Vector
import Example
import qualified Foreign
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Hree
import Linear (V2(..), V3(..), V4(..))
import Prelude hiding (init)

main :: IO ()
main =
    withWindow width height "sprite-instanced-1" init onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    delta :: Float
    delta = 0.2

    spriteVertices = Vector.fromList
        $  map (\x -> Hree.SpriteVertex (V3 (fromIntegral x * delta) 0 0) (V3 delta delta 0) (V3 0 0 0) 0 (V2 0 0) (V2 1 1) GL.GL_FALSE 0) ([0..9] :: [Int])
        ++ map (\x -> Hree.SpriteVertex (V3 (fromIntegral x * delta * 2) delta 0) (V3 (delta * 2) delta 0) (V3 0 0 0) 0 (V2 0 0) (V2 1 1) GL.GL_FALSE 0) ([0..4] :: [Int])
        ++ map (\x -> Hree.SpriteVertex (V3 (fromIntegral x * delta * 2) (delta * 2) 0) (V3 (delta * 2) delta 0) (V3 0 0 0) (pi * 0.2) (V2 0 0) (V2 1 1) GL.GL_FALSE 0) ([0..4] :: [Int])

    proj = Hree.perspective 90 defaultAspect 0.1 10.0

    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene

        let geo' = Hree.addVerticesToGeometry Hree.spriteGeometry spriteVertices GL.GL_STATIC_READ
        mapping <- mkMappingSource
        let material = Hree.spriteMaterial { Hree.materialMappings = pure (Hree.BaseColorMapping, mapping) }
        materialId <- Hree.addMaterial scene material
        let mesh = (Hree.mesh geo' materialId) { Hree.meshInstanceCount = Just . Vector.length $ spriteVertices }
        meshId <- Hree.addMesh scene mesh
        _ <- Hree.addNode scene Hree.node (Just meshId) True
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
        let size = 1024
            settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral size) (fromIntegral size) False
            image = mkCircleImage size (V4 188 0 0 255)
            byteSize = size * size * 4
        pixels <- Foreign.mallocForeignPtrBytes byteSize
        let source = Hree.TextureSourceData (fromIntegral size) (fromIntegral size) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE pixels
        Foreign.withForeignPtr pixels $ \dest ->
            Vector.unsafeWith image $ \p -> do
                Foreign.copyBytes (Foreign.castPtr dest) p byteSize
        return (Hree.MappingSource settings source [])
