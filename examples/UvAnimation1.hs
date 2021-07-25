{-# LANGUAGE OverloadedStrings #-}
module UvAnimation1 where

import qualified Chronos as Time (now)
import qualified Codec.Picture as Picture (Image(..), PixelRGBA8, convertRGBA8,
                                           readImage)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.Int (Int64)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Example
import qualified Foreign
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Hree
import qualified Hree.Material.SpriteMaterial as Material (SpriteMaterialBlock(..))
import Linear (V2(..), V3(..), (!*))
import Prelude hiding (init)

data CharacterInfo = CharacterInfo
    { animationWalkFront :: !Hree.AnimationClip
    , animationWalkBack  :: !Hree.AnimationClip
    , animationWalkRight :: !Hree.AnimationClip
    } deriving (Show)

main :: IO ()
main =
    withWindow width height "uv-animation-1" init onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    proj = Hree.orthographic (-defaultAspect) defaultAspect (-1.0) 1.0 0.01 10.0

    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    tsize = V2 128 128
    V2 twidth theight = tsize
    V2 twidth' theight' = fmap fromIntegral tsize
    uvCoordMat = V2 (V2 (1 / twidth') 0) (V2 0 (1 / theight'))

    uvOffset p = uvCoordMat !* p

    timepoints :: UV.Vector Int64
    timepoints = UV.map (* 1000000) $ UV.fromList
        [ 0
        , 250
        , 500
        , 750
        , 1000
        ]

    walkFrontUVs, walkBackUVs, walkRightUVs :: UV.Vector (V2 Float)
    walkFrontUVs = UV.fromList
        [ V2 16 31
        , V2 16 48
        , V2 16 31
        , V2 16 64
        , V2 16 31
        ]
    walkBackUVs = UV.fromList
        [ V2 48 31
        , V2 48 48
        , V2 48 31
        , V2 48 64
        , V2 48 31
        ]
    walkRightUVs = UV.fromList
        [ V2 32 31
        , V2 32 48
        , V2 32 31
        , V2 32 64
        , V2 32 31
        ]

    init w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        (material, materialId) <- createMaterial scene (uvOffset $ UV.head walkFrontUVs)
        meshId <- createMesh scene materialId (V2 16 (-16))

        nodeId <- Hree.addNode scene Hree.node (Just meshId) True
        uniformBlockBinder <- Hree.addNodeUniformBlock scene nodeId Hree.materialBlockBindingIndex (Hree.materialUniformBlock material)

        let walkFront = createUvAnimation uniformBlockBinder walkFrontUVs
            walkBack = createUvAnimation uniformBlockBinder walkBackUVs
            walkRight = createUvAnimation uniformBlockBinder walkRightUVs
            characterInfo = CharacterInfo
                walkFront
                walkBack
                walkRight

        taskBoard <- Hree.newSceneTaskBoard scene
        taskId <- Hree.addSceneTask taskBoard Hree.Nop

        GLFW.setKeyCallback w (Just $ keyCallback uniformBlockBinder characterInfo taskBoard taskId)

        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera, taskBoard)

    onDisplay (r, s, c, taskBoard) w = do
        render
        threadDelay 100000
        GLFW.pollEvents
        t <- Time.now
        Hree.runSceneTasksOnBoard taskBoard t
        onDisplay (r, s, c, taskBoard) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w

    -- image source https://opengameart.org/sites/default/files/RPG_assets.png
    imagePath = "examples/images/RPG_assets.png"

    createMaterial scene off = do
        image <- either (throwIO . userError) (return . Picture.convertRGBA8) =<< Picture.readImage imagePath
        mapping <- mkMappingSource image
        let material = Hree.spriteMaterial
                { Hree.materialUniformBlock = (Hree.materialUniformBlock Hree.spriteMaterial)
                    { Material.uvOffset = off
                    }
                , Hree.materialMappings = pure (Hree.BaseColorMapping, mapping)
                }
        (,) material <$> Hree.addMaterial scene material

    mkMappingSource :: Picture.Image Picture.PixelRGBA8 -> IO Hree.MappingSource
    mkMappingSource image = do
        let settings = Hree.TextureSettings 1 GL.GL_RGBA8 twidth theight False
            byteSize = twidth * theight * 4
            samplerParamValues =
                [ Hree.SamplerParamValue Hree.glTextureMinFilter GL.GL_NEAREST
                , Hree.SamplerParamValue Hree.glTextureMagFilter GL.GL_NEAREST
                ]
        pixels <- Foreign.mallocForeignPtrBytes (fromIntegral byteSize)
        let textureSource = Hree.TextureSourceData twidth theight PixelFormat.glRgba GL.GL_UNSIGNED_BYTE pixels
        Foreign.withForeignPtr pixels $ \dest ->
            SV.unsafeWith (Picture.imageData image) $ \ptr -> do
                Foreign.copyBytes (Foreign.castPtr dest) ptr (fromIntegral byteSize)
        return (Hree.MappingSource settings textureSource samplerParamValues)

    createMesh scene materialId (V2 w h) = do
        let vs = SV.singleton $ Hree.SpriteVertex (V3 0 0 0) (V3 0.5 0.5 0) (V3 0 0 0) 0 (V2 0 0) (V2 (w / twidth') (h / theight')) GL.GL_FALSE 0
        let geo' = Hree.addVerticesToGeometry Hree.spriteGeometry vs GL.GL_STATIC_READ
        Hree.addMesh scene (Hree.mesh geo' materialId) { Hree.meshInstanceCount = Just 1 }

    createUvAnimation ubb uvs =
        let uvs' = UV.map uvOffset uvs
            setter off = Hree.modifyUniformBlock (\a -> a { Material.uvOffset = off }) ubb
            track = Hree.VariationTrackDiscrete uvs'
            keyFrames = Hree.KeyFrames Hree.InterpolationStep timepoints track
            animation = Hree.singleVariationClip setter keyFrames
        in animation

    keyCallback ubb characterInfo taskBoard taskId _ key _ GLFW.KeyState'Pressed _ =
        case resolveWalkAnimation characterInfo key of
            Just (animation, flipH) -> do
                st <- Time.now
                Hree.modifyUniformBlock (\a -> a { Material.uvFlippedHorizontally = flipH }) ubb
                Hree.modifySceneTask taskBoard (const $ Hree.AnimationTask st animation (Hree.AnimationTaskOption True False Nothing)) taskId
            Nothing -> return ()

    keyCallback _ _ taskBoard taskId _ _ _ GLFW.KeyState'Released _ = do
        Hree.terminateSceneTask taskBoard taskId
        Hree.modifySceneTask taskBoard (const Hree.Nop) taskId

    keyCallback _ _ _ _ _ _ _ _ _ = return ()

    resolveWalkAnimation characterInfo GLFW.Key'Up = Just (animationWalkBack characterInfo, GL.GL_FALSE)
    resolveWalkAnimation characterInfo GLFW.Key'Down = Just (animationWalkFront characterInfo, GL.GL_FALSE)
    resolveWalkAnimation characterInfo GLFW.Key'Left = Just (animationWalkRight characterInfo, GL.GL_TRUE)
    resolveWalkAnimation characterInfo GLFW.Key'Right = Just (animationWalkRight characterInfo, GL.GL_FALSE)
    resolveWalkAnimation _ _ = Nothing
