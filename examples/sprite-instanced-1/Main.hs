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
import Graphics.Hree.Scene
import qualified Graphics.Hree.Texture as Texture
import Graphics.Hree.Types (Mesh(..), Node(..))
import qualified Graphics.UI.GLFW as GLFW
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
        $  map (\x -> SpriteVertex (V3 (fromIntegral x * delta) 0 0) (V3 delta delta 0) 0 (V2 0 0) (V2 1 1)) ([0..9] :: [Int])
        ++ map (\x -> SpriteVertex (V3 (fromIntegral x * delta * 2) delta 0) (V3 (delta * 2) delta 0) 0 (V2 0 0) (V2 1 1)) ([0..4] :: [Int])
        ++ map (\x -> SpriteVertex (V3 (fromIntegral x * delta * 2) (delta * 2) 0) (V3 (delta * 2) delta 0) (pi * 0.2) (V2 0 0) (V2 1 1)) ([0..4] :: [Int])

    proj = perspective 90 defaultAspect 0.1 10.0

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        GL.glEnable GL.GL_BLEND
        GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA
        renderer <- newRenderer
        scene <- newScene

        (geo, _) <- Geometry.newSpriteGeometry scene
        geo' <- Geometry.addVerticesToGeometry geo spriteVertices GL.GL_STATIC_READ scene
        texture <- mkTexture scene
        let material = Material.spriteMaterial texture
            mesh = Mesh geo' material (Just . Vector.length $ spriteVertices)
        meshId <- addMesh scene mesh
        _ <- addNode scene newNode{ nodeMesh = Just meshId } True
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindow' camera))
        return (renderer, scene, camera)

    onDisplay (r, s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (r, s, c) w

        where
        render = do
            renderScene r s c
            GLFW.swapBuffers w

    resizeWindow' camera _ w h = do
        GLW.glViewport 0 0 (fromIntegral w) (fromIntegral h)
        projection <- getCameraProjection camera
        let aspect = fromIntegral w / fromIntegral h
            projection' = updateProjectionAspectRatio projection aspect
        updateProjection camera projection'

    updateProjectionAspectRatio (PerspectiveProjection (Perspective fov _ near far)) aspect =
        PerspectiveProjection $ Perspective fov aspect near far
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
