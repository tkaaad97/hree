{-# LANGUAGE OverloadedStrings #-}
module UvAnimation1 where

import qualified Chronos as Time (now)
import qualified Codec.Picture as Picture (Image(..), convertRGBA8, readImage)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.Int (Int64)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Example
import qualified Foreign (castPtr)
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import Graphics.Hree as Hree
import qualified Graphics.Hree.Animation as Animation
import qualified Graphics.Hree.GL.UniformBlock as Hree (modifyUniformBlock)
import qualified Graphics.Hree.Material.SpriteMaterial as Material (SpriteMaterial(..),
                                                                    SpriteMaterialBlock(..),
                                                                    spriteMaterial)
import qualified Graphics.Hree.SceneTask as SceneTask
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..), (!*))
import Prelude hiding (init)

data CharacterInfo = CharacterInfo
    { stillFront         :: !(V2 (V2 Float))
    , stillBack          :: !(V2 (V2 Float))
    , stillLeft          :: !(V2 (V2 Float))
    , stillRight         :: !(V2 (V2 Float))
    , animationWalkFront :: !Animation.AnimationClip
    , animationWalkBack  :: !Animation.AnimationClip
    , animationWalkLeft  :: !Animation.AnimationClip
    , animationWalkRight :: !Animation.AnimationClip
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

    uvOffsetScale (V2 p s) = V2 (uvCoordMat !* p) s

    timepoints :: UV.Vector Int64
    timepoints = UV.map (* 1000000) $ UV.fromList
        [ 0
        , 250
        , 500
        , 750
        , 1000
        ]

    walkFrontUVs, walkBackUVs, walkLeftUVs, walkRightUVs :: UV.Vector (V2 (V2 Float))
    walkFrontUVs = UV.fromList
        [ V2 (V2 16 31) (V2 1 (-1))
        , V2 (V2 16 48) (V2 1 (-1))
        , V2 (V2 16 31) (V2 1 (-1))
        , V2 (V2 16 64) (V2 1 (-1))
        , V2 (V2 16 31) (V2 1 (-1))
        ]
    walkBackUVs = UV.fromList
        [ V2 (V2 48 31) (V2 1 (-1))
        , V2 (V2 48 48) (V2 1 (-1))
        , V2 (V2 48 31) (V2 1 (-1))
        , V2 (V2 48 64) (V2 1 (-1))
        , V2 (V2 48 31) (V2 1 (-1))
        ]
    walkLeftUVs = UV.fromList
        [ V2 (V2 48 31) (V2 (-1) (-1))
        , V2 (V2 48 48) (V2 (-1) (-1))
        , V2 (V2 48 31) (V2 (-1) (-1))
        , V2 (V2 48 64) (V2 (-1) (-1))
        , V2 (V2 48 31) (V2 (-1) (-1))
        ]
    walkRightUVs = UV.fromList
        [ V2 (V2 32 31) (V2 1 (-1))
        , V2 (V2 32 48) (V2 1 (-1))
        , V2 (V2 32 31) (V2 1 (-1))
        , V2 (V2 32 64) (V2 1 (-1))
        , V2 (V2 32 31) (V2 1 (-1))
        ]

    init w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        material <- createMaterial scene (uvOffsetScale $ UV.head walkFrontUVs)
        Hree.AddedMesh meshId uniformBlockBinder <- createMesh scene material (V2 16 16)
        let walkFront = createUvAnimation uniformBlockBinder walkFrontUVs
            walkBack = createUvAnimation uniformBlockBinder walkBackUVs
            walkLeft = createUvAnimation uniformBlockBinder walkLeftUVs
            walkRight = createUvAnimation uniformBlockBinder walkRightUVs
            characterInfo = CharacterInfo
                (uvOffsetScale $ UV.head walkFrontUVs)
                (uvOffsetScale $ UV.head walkBackUVs)
                (uvOffsetScale $ UV.head walkLeftUVs)
                (uvOffsetScale $ UV.head walkRightUVs)
                walkFront
                walkBack
                walkLeft
                walkRight

        let node = Hree.newNode { Hree.nodeMesh = Just meshId }
        _ <- Hree.addNode scene node True
        taskBoard <- SceneTask.newSceneTaskBoard scene
        taskId <- SceneTask.addSceneTask taskBoard SceneTask.Nop

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
        SceneTask.runSceneTasksOnBoard taskBoard t
        onDisplay (r, s, c, taskBoard) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w

    -- image source https://opengameart.org/sites/default/files/RPG_assets.png
    imagePath = "examples/images/RPG_assets.png"

    createMaterial scene (V2 off scale) = do
        image <- either (throwIO . userError) (return . Picture.convertRGBA8) =<< Picture.readImage imagePath
        let settings = Hree.TextureSettings 1 GL.GL_RGBA8 twidth theight False
        (_, texture) <- SV.unsafeWith (Picture.imageData image) $ \ptr -> do
            let sourceData = Hree.TextureSourceData twidth theight PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr ptr)
            Hree.addTexture scene "material1" settings sourceData
        (_, sampler) <- Hree.addSampler scene "material1"
        Hree.setSamplerParameter sampler Hree.glTextureMinFilter GL.GL_NEAREST
        Hree.setSamplerParameter sampler Hree.glTextureMagFilter GL.GL_NEAREST
        let material = Material.spriteMaterial
                { Material.uniformBlock = (Material.uniformBlock Material.spriteMaterial)
                    { Material.uvOffset = off
                    , Material.uvScale = scale
                    }
                , Material.baseColorTexture = Just $ Hree.Texture (texture, sampler)
                }
        return material

    createMesh scene material (V2 w h) = do
        let vs = SV.singleton $ Hree.SpriteVertex (V3 0 0 0) (V3 0.5 0.5 0) (V3 0 0 0) 0 (V2 0 0) (V2 (w / twidth') (h / theight'))
        (geo, _) <- Hree.newSpriteGeometry scene
        geo' <- Hree.addVerticesToGeometry geo vs GL.GL_STATIC_READ scene
        Hree.addMesh scene $ Mesh geo' material (Just 1)

    createUvAnimation ubb uvs =
        let uvs' = UV.map uvOffsetScale uvs
            setter (V2 off scale) = Hree.modifyUniformBlock (\a -> a { Material.uvOffset = off, Material.uvScale = scale }) ubb
            track = Animation.VariationTrackDiscrete uvs'
            keyFrames = Animation.KeyFrames Animation.InterpolationStep timepoints track
            animation = Animation.singleVariationClip setter keyFrames
        in animation

    keyCallback _ characterInfo taskBoard taskId _ key _ GLFW.KeyState'Pressed _ =
        case resolveWalkAnimation characterInfo key of
            Just animation -> do
                st <- Time.now
                SceneTask.modifySceneTask taskBoard (const $ SceneTask.AnimationTask st animation (SceneTask.AnimationTaskOption True False)) taskId
            Nothing -> return ()

    keyCallback ubb characterInfo taskBoard taskId _ key _ GLFW.KeyState'Released _ =
        case resolveStillUV characterInfo key of
            Just (V2 off scale) -> do
                SceneTask.modifySceneTask taskBoard (const SceneTask.Nop) taskId
                Hree.modifyUniformBlock (\a -> a { Material.uvOffset = off, Material.uvScale = scale }) ubb
                return ()
            Nothing -> return ()

    keyCallback _ _ _ _ _ _ _ _ _ = return ()

    resolveWalkAnimation characterInfo GLFW.Key'Up = Just $ animationWalkBack characterInfo
    resolveWalkAnimation characterInfo GLFW.Key'Down = Just $ animationWalkFront characterInfo
    resolveWalkAnimation characterInfo GLFW.Key'Left = Just $ animationWalkLeft characterInfo
    resolveWalkAnimation characterInfo GLFW.Key'Right = Just $ animationWalkRight characterInfo
    resolveWalkAnimation _ _ = Nothing

    resolveStillUV characterInfo GLFW.Key'Up = Just $ stillBack characterInfo
    resolveStillUV characterInfo GLFW.Key'Down = Just $ stillFront characterInfo
    resolveStillUV characterInfo GLFW.Key'Left = Just $ stillLeft characterInfo
    resolveStillUV characterInfo GLFW.Key'Right = Just $ stillRight characterInfo
    resolveStillUV _ _ = Nothing
