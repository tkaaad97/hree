{-# LANGUAGE OverloadedStrings #-}
module SpriteInstanced1 where

import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import Example
import Foreign (Ptr)
import qualified Foreign (castPtr)
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import Graphics.Hree as Hree
import qualified Graphics.Hree.Material.SpriteMaterial as Material
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
        $  map (\x -> SpriteVertex (V3 (fromIntegral x * delta) 0 0) (V3 delta delta 0) (V3 0 0 0) 0 (V2 0 0) (V2 1 1)) ([0..9] :: [Int])
        ++ map (\x -> SpriteVertex (V3 (fromIntegral x * delta * 2) delta 0) (V3 (delta * 2) delta 0) (V3 0 0 0) 0 (V2 0 0) (V2 1 1)) ([0..4] :: [Int])
        ++ map (\x -> SpriteVertex (V3 (fromIntegral x * delta * 2) (delta * 2) 0) (V3 (delta * 2) delta 0) (V3 0 0 0) (pi * 0.2) (V2 0 0) (V2 1 1)) ([0..4] :: [Int])

    proj = perspective 90 defaultAspect 0.1 10.0

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        renderer <- newRenderer
        scene <- newScene

        (geo, _) <- Hree.newSpriteGeometry scene
        geo' <- Hree.addVerticesToGeometry geo spriteVertices GL.GL_STATIC_READ scene
        texture <- mkTexture scene
        let material = Material.spriteMaterial { Material.baseColorTexture = Just texture }
            mesh = Mesh geo' material (Just . Vector.length $ spriteVertices)
        meshId <- addedMeshId <$> addMesh scene mesh
        _ <- addNode scene newNode{ nodeMesh = Just meshId } True
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera)

    onDisplay (r, s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (r, s, c) w

        where
        render = do
            renderScene r s c
            GLFW.swapBuffers w

    mkTexture :: Hree.Scene -> IO Hree.Texture
    mkTexture scene = do
        let size = 1024
            settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral size) (fromIntegral size) False
            image = mkCircleImage size (V4 188 0 0 255)
        Vector.unsafeWith image $ \p -> do
            let source = Hree.TextureSourceData (fromIntegral size) (fromIntegral size) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr (V4 Word8)))
            (_, texture) <- Hree.addTexture scene "color" settings source
            (_, sampler) <- Hree.addSampler scene "sampler"
            return $ Hree.Texture (texture, sampler)
