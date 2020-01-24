module Graphics.Format.Tiled
    ( Rect(..)
    , tileBoundingUpLeft
    , tileBoundingRect
    , tileBoundingSpriteVertex
    ) where

import qualified Codec.Picture as Picture (Image(..), PixelRGBA8, convertRGBA8,
                                           readImage)
import qualified Codec.Picture.Types as Picture (MutableImage(..),
                                                 PixelBaseComponent,
                                                 freezeImage, newMutableImage)
import Control.Exception (throwIO)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Vector as BV (Vector, mapM)
import qualified Data.Vector.Storable as SV (unsafeWith)
import qualified Data.Vector.Storable.Mutable as MSV (unsafeWith)
import Data.Word (Word8)
import qualified Foreign (Ptr, castPtr, copyArray, plusPtr, sizeOf)
import qualified GLW.Groups.PixelFormat as PixelFormat
import Graphics.Format.Tiled.Types
import qualified Graphics.GL as GL
import qualified Graphics.Hree.GL.Types as Hree (Texture(..))
import qualified Graphics.Hree.GL.Vertex as Hree (SpriteVertex(..))
import qualified Graphics.Hree.Material as Hree (spriteMaterial)
import qualified Graphics.Hree.Scene as Hree (addSampler, addTexture)
import qualified Graphics.Hree.Texture as Hree (TextureSettings(..),
                                                TextureSourceData(..))
import qualified Graphics.Hree.Types as Hree (Material, Scene)
import Linear (V2(..), V3(..), (^-^))
import System.Directory (canonicalizePath)
import System.FilePath ((</>))

data Rect = Rect
    { rectBottomLeft :: !(V2 Float)
    , rectSize       :: !(V2 Float)
    } deriving (Show, Eq)

tileBoundingRect :: Orientation -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> Int -> StaggerAxis -> StaggerIndex -> Int -> Rect
tileBoundingRect orientation origin index size offset unit staggerAxis staggerIndex hexSide =
    let upLeft = tileBoundingUpLeft orientation origin index size offset unit staggerAxis staggerIndex hexSide
        V2 width height = size
        w = fromIntegral width / fromIntegral unit
        h = fromIntegral height / fromIntegral unit
        bottomLeft = upLeft ^-^ V2 0 h
    in Rect bottomLeft (V2 w h)

tileBoundingSpriteVertex :: Rect -> Float -> Rect -> Hree.SpriteVertex
tileBoundingSpriteVertex (Rect (V2 x y) (V2 width height)) z (Rect uv uvSize) =
    let pos = V3 x y z
        size = V3 width height 0
        angle = 0
        vertex = Hree.SpriteVertex pos size angle uv uvSize
    in vertex

tileBoundingUpLeft :: Orientation -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> Int -> StaggerAxis -> StaggerIndex -> Int -> V2 Float
tileBoundingUpLeft OrientationOrthogonal (V2 ox oy) (V2 ix iy) (V2 width height) (V2 offsetX offsetY) unit _ _ _ =
    let x = fromIntegral (ox + ix * width + offsetX) / fromIntegral unit
        y = fromIntegral (oy - iy * height - offsetY) / fromIntegral unit
    in V2 x y
tileBoundingUpLeft OrientationIsometric (V2 ox oy) (V2 ix iy) (V2 width height) (V2 offsetX offsetY) unit _ _ _ =
    let x = fromIntegral (ox * 2 + (ix - iy) * width + offsetX * 2) / fromIntegral (unit * 2)
        y = fromIntegral (oy * 2 - (ix + iy) * height - offsetY * 2) / fromIntegral (unit * 2)
    in V2 x y
tileBoundingUpLeft OrientationStaggered (V2 ox oy) (V2 ix iy) (V2 width height) (V2 offsetX offsetY) unit staggerAxis staggerIndex _ =
    let V2 sx sy = stagger2 staggerAxis staggerIndex 0 (V2 ix iy) (V2 width height)
        x = fromIntegral (ox * 2 + sx + offsetX * 2) / fromIntegral (unit * 2)
        y = fromIntegral (oy * 2 + sy - offsetY * 2) / fromIntegral (unit * 2)
    in V2 x y
tileBoundingUpLeft OrientationHexagonal (V2 ox oy) (V2 ix iy) (V2 width height) (V2 offsetX offsetY) unit staggerAxis staggerIndex hexSide =
    let V2 sx sy = stagger2 staggerAxis staggerIndex hexSide (V2 ix iy) (V2 width height)
        x = fromIntegral (ox * 2 + sx + offsetX * 2) / fromIntegral (unit * 2)
        y = fromIntegral (oy * 2 + sy - offsetY * 2) / fromIntegral (unit * 2)
    in V2 x y

stagger2 :: StaggerAxis -> StaggerIndex -> Int -> V2 Int -> V2 Int -> V2 Int
stagger2 StaggerAxisX staggerIndex hexSide (V2 ix iy) (V2 width height) =
    let x = ix * (width + hexSide)
        y = case (staggerIndex, ix `mod` 2 == 0) of
                (StaggerIndexEven, True)  -> - iy * height * 2
                (StaggerIndexEven, False) -> - (iy + 1) * height * 2
                (StaggerIndexOdd, False)  -> - iy * height * 2
                (StaggerIndexOdd, True)   -> - (iy + 1) * height * 2
    in V2 x y
stagger2 StaggerAxisY staggerIndex hexSide (V2 ix iy) (V2 width height) =
    let x = case (staggerIndex, iy `mod` 2 == 0) of
                (StaggerIndexEven, True)  -> - ix * width * 2
                (StaggerIndexEven, False) -> - (ix + 1) * width * 2
                (StaggerIndexOdd, False)  -> - ix * width * 2
                (StaggerIndexOdd, True)   -> - (ix + 1) * width * 2
        y = - iy * (height + hexSide)
    in V2 x y

createMaterials :: Hree.Scene -> FilePath -> BV.Vector Tileset -> IO (BV.Vector (Maybe Hree.Material))
createMaterials scene cd = BV.mapM (createMaterial scene cd)

createMaterial :: Hree.Scene -> FilePath -> Tileset -> IO (Maybe Hree.Material)
createMaterial scene cd tileset =
    maybe (return Nothing) (fmap Just . go) (tilesetImage tileset)
    where
    go (Image sourcePath width height) = do
        path <- canonicalizePath $ cd </> Text.unpack sourcePath
        image <- either (throwIO . userError) (resizeImage width height . Picture.convertRGBA8) =<< Picture.readImage path
        let name = Text.encodeUtf8 sourcePath
            settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral width) (fromIntegral height) False
        (_, texture) <- SV.unsafeWith (Picture.imageData image) $ \ptr -> do
            let sourceData = Hree.TextureSourceData (fromIntegral width) (fromIntegral height) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr ptr)
            Hree.addTexture scene name settings sourceData
        let sname = Text.encodeUtf8 sourcePath
        (_, sampler) <- Hree.addSampler scene sname
        let material = Hree.spriteMaterial $ Hree.Texture (texture, sampler)
        return material

resizeImage :: Int -> Int -> Picture.Image Picture.PixelRGBA8 -> IO (Picture.Image Picture.PixelRGBA8)
resizeImage width height source =
    if width == sourceWidth && height == sourceHeight
        then return source
        else do
            dest <- Picture.newMutableImage width height
            SV.unsafeWith (Picture.imageData source) $ \sp ->
                MSV.unsafeWith (Picture.mutableImageData dest) $ \dp -> mapM_ (writeRow sp dp) [0..((min height sourceHeight) - 1)]
            Picture.freezeImage dest
    where
    sourceWidth = Picture.imageWidth source
    sourceHeight = Picture.imageHeight source
    minWidth = min width sourceWidth
    byteSize = Foreign.sizeOf (undefined :: Picture.PixelBaseComponent Picture.PixelRGBA8)
    writeRow sp dp i = do
        let sp' = Foreign.plusPtr sp (i * sourceWidth * byteSize) :: Foreign.Ptr (Picture.PixelBaseComponent Picture.PixelRGBA8)
            dp' = Foreign.plusPtr dp (i * width * byteSize) :: Foreign.Ptr (Picture.PixelBaseComponent Picture.PixelRGBA8)
        Foreign.copyArray sp' dp' minWidth
