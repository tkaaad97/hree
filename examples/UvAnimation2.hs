{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module UvAnimation2 where

import qualified Chronos as Time (now)
import qualified Codec.Picture as Picture (Image(..), PixelRGBA8, convertRGBA8,
                                           readImage)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import qualified Data.Aeson as DA (eitherDecodeStrict')
import qualified Data.Aeson.TH as DA (Options(..), defaultOptions, deriveJSON)
import qualified Data.ByteString as ByteString (readFile)
import Data.Char (toLower)
import qualified Data.Vector as BV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Example
import qualified Foreign
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Graphics.Hree as Hree
import qualified Graphics.Hree.Material.SpriteMaterial as Material (SpriteMaterialBlock(..))
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..))
import Prelude hiding (init)

data Sprite = Sprite
    { spriteIndex :: !Int
    , spriteX     :: !Int
    , spriteY     :: !Int
    , spriteW     :: !Int
    , spriteH     :: !Int
    , spriteOX    :: !Int
    , spriteOY    :: !Int
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = map toLower . drop 6 }) ''Sprite)

data FrameData = FrameData
    { frameDataPositionOffset :: !(V3 Float)
    , frameDataSizeFactor     :: !(V3 Float)
    , frameDataUvOffset       :: !(V2 Float)
    , frameDataUvSize         :: !(V2 Float)
    } deriving (Show, Eq)

main :: IO ()
main =
    withWindow width height "uv-animation-2" init onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    proj = Hree.orthographic (-defaultAspect) defaultAspect (-1.0) 1.0 0.01 10.0

    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    tsize = V2 512 512
    V2 twidth theight = tsize
    V2 twidth' theight' = fmap fromIntegral tsize

    init w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        materialId <- createMaterial scene
        Hree.AddedMesh meshId uniformBlockBinder <- createMesh scene materialId
        sprites <- either (throwIO . userError) return . DA.eitherDecodeStrict' =<< ByteString.readFile "examples/walksprite.json"
        let animation = createUvAnimation uniformBlockBinder sprites
            node = Hree.newNode { Hree.nodeMesh = Just meshId }
        _ <- Hree.addNode scene node True
        taskBoard <- Hree.newSceneTaskBoard scene
        st <- Time.now
        _ <- Hree.addSceneTask taskBoard (Hree.AnimationTask st animation (Hree.AnimationTaskOption True False Nothing))

        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera, taskBoard)

    onDisplay (r, s, c, taskBoard) w = do
        t <- Time.now
        Hree.runSceneTasksOnBoard taskBoard t
        render
        threadDelay 100000
        GLFW.pollEvents
        onDisplay (r, s, c, taskBoard) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w

    imagePath = "examples/images/walkpacked.png"

    createMaterial scene = do
        image <- either (throwIO . userError) (return . Picture.convertRGBA8) =<< Picture.readImage imagePath
        mapping <- mkMappingSource image
        let material = Hree.spriteMaterial
                { Hree.materialMappings = pure (Hree.BaseColorMapping, mapping)
                }
        Hree.addMaterial scene material

    mkMappingSource :: Picture.Image Picture.PixelRGBA8 -> IO Hree.MappingSource
    mkMappingSource image = do
        let settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral twidth) (fromIntegral theight) False
            byteSize = twidth * theight * 4
            samplerParamValues =
                [ Hree.SamplerParamValue Hree.glTextureMinFilter GL.GL_NEAREST
                , Hree.SamplerParamValue Hree.glTextureMagFilter GL.GL_NEAREST
                ]
        pixels <- Foreign.mallocForeignPtrBytes byteSize
        let textureSource = Hree.TextureSourceData (fromIntegral twidth) (fromIntegral theight) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE pixels
        Foreign.withForeignPtr pixels $ \dest ->
            SV.unsafeWith (Picture.imageData image) $ \ptr -> do
                Foreign.copyBytes (Foreign.castPtr dest) ptr byteSize
        return (Hree.MappingSource settings textureSource samplerParamValues)

    createMesh scene material = do
        let vs = SV.singleton $ Hree.SpriteVertex (V3 0 0 0) (V3 1 1 0) (V3 0 0 0) 0 (V2 0 0) (V2 1 1) GL.GL_FALSE 0
            geo' = Hree.addVerticesToGeometry Hree.spriteGeometry vs GL.GL_STATIC_READ
        Hree.addMesh scene $ Hree.Mesh geo' material (Just 1)

    createUvAnimation ubb sprites =
        let updateMaterialBlock x a = a
                { Material.positionOffset = frameDataPositionOffset x
                , Material.sizeFactor = frameDataSizeFactor x
                , Material.uvOffset = frameDataUvOffset x
                , Material.uvSizeFactor = frameDataUvSize x
                }
            frames = BV.map toFrameData sprites
            timepoints = UV.generate (BV.length sprites) (fromIntegral . (* 50000000))
            keyFrames = Hree.KeyFrames Hree.InterpolationStep timepoints (Hree.VariationTrackDiscrete frames)
            animation = Hree.singleVariationClip (\sprite -> Hree.modifyUniformBlock (updateMaterialBlock sprite) ubb) keyFrames
        in animation

    toFrameData sp =
        let uvx = fromIntegral (spriteX sp) / twidth'
            uvy = fromIntegral (spriteY sp + spriteH sp) / theight'
            uvw = fromIntegral (spriteW sp) / twidth'
            uvh = - fromIntegral (spriteH sp) / theight'
            px = 0.02 * fromIntegral (spriteOX sp - 64)
            py = 0.02 * fromIntegral (spriteOY sp - 64)
            sx = 0.02 * fromIntegral (spriteW sp)
            sy = 0.02 * fromIntegral (spriteH sp)
        in FrameData
            { frameDataPositionOffset = V3 px py 0
            , frameDataSizeFactor = V3 sx sy 1
            , frameDataUvOffset = V2 uvx uvy
            , frameDataUvSize = V2 uvw uvh
            }
