{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hree.Text
    ( FontFace
    , createTextMesh
    , deleteFontFace
    , newFontFace
    ) where

import Control.Monad (foldM, forM_, when)
import Data.Bits (shift, (.|.))
import Data.Char (ord)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map.Strict as Map (fromList, (!))
import Data.Text (Text)
import qualified Data.Text as Text (length, unpack)
import qualified Data.Vector.Storable as SV (fromList)
import qualified Foreign (allocaArray, castPtr, nullPtr, peek, peekElemOff,
                          pokeElemOff, with)
import qualified FreeType
import qualified GLW
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Graphics.Hree as Hree
import Linear (V2(..), V3(..), V4(..))
import System.Directory (canonicalizePath)

data FontFace = FontFace !FreeType.FT_Library !FreeType.FT_Face

data Character = Character
    { characterCharcode :: !Char
    , characterSize     :: !(V2 Int)
    , characterBearing  :: !(V2 Int)
    , characterAdvance  :: !Int
    } deriving (Show, Eq)

newFontFace :: FilePath -> IO FontFace
newFontFace fontPath = do
    path <- canonicalizePath fontPath
    freeType <- FreeType.ft_Init_FreeType
    face <- FreeType.ft_New_Face freeType path 0
    return (FontFace freeType face)

deleteFontFace :: FontFace -> IO ()
deleteFontFace (FontFace freeType face) = do
    FreeType.ft_Done_Face face
    FreeType.ft_Done_FreeType freeType

createTextMesh :: Hree.Scene -> FontFace -> Text -> Int -> IO (Hree.AddedMesh Hree.SpriteMaterialBlock)
createTextMesh scene (FontFace freeType face) text pixelSize = do
    let str = Text.unpack text
        charcodes = nubOrd str

    isScalable <- FreeType.FT_IS_SCALABLE face
    when isScalable
      $ FreeType.ft_Set_Pixel_Sizes face 0 (fromIntegral pixelSize)

    characterMap <- Map.fromList <$> mapM loadMetrics charcodes
    let characters = map (characterMap Map.!) str
        V2 twidth theight = calcTextureSize
        settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral twidth) (fromIntegral theight) False
        sourceData = Hree.TextureSourceData (fromIntegral twidth) (fromIntegral theight) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE Foreign.nullPtr

    (tname, texture) <- Hree.addTexture scene "textmesh" settings sourceData
    (_, sampler) <- Hree.addSampler scene tname
    let material = Hree.spriteMaterial { Hree.materialTextures = pure (Hree.BaseColorMapping, Hree.Texture (texture, sampler)) }

    upMap <- Foreign.allocaArray (pixelSize * pixelSize * 4) $ \p ->
        Map.fromList . snd <$> foldM (renderBitmap (V2 twidth theight) p texture) (V2 0 0, []) charcodes
    let advanceSums = scanl (+) 0 . map characterAdvance $ characters
        vertices = SV.fromList . map (toSpriteVertex (V2 twidth theight) upMap) $ (characters `zip` advanceSums)
    (geo, _) <- Hree.newSpriteGeometry scene
    geo' <- Hree.addVerticesToGeometry geo vertices GL.GL_STATIC_READ scene

    let mesh = Hree.Mesh geo' material . Just . Text.length $ text
    Hree.addMesh scene mesh

    where
    calcTextureSize = V2 1024 1024 :: V2 Int
    padding = 2
    unitLength = 1024 :: Int

    toSpriteVertex (V2 twidth theight) uvMap (Character charcode (V2 w h) (V2 bx by) _, advanceSum) =
        let V2 uvx uvy = uvMap Map.! charcode
            x = fromIntegral (advanceSum + bx) / fromIntegral unitLength
            y = fromIntegral (by - h) / fromIntegral unitLength
            position = V3 x y 0
            size = V3 (fromIntegral w / fromIntegral unitLength) (fromIntegral h / fromIntegral unitLength) 0
            uv = V2 (fromIntegral uvx / fromIntegral twidth) (fromIntegral (h - fromIntegral uvy) / fromIntegral theight)
            uvSize = V2 (fromIntegral w / fromIntegral twidth) (- fromIntegral h / fromIntegral theight)
        in Hree.SpriteVertex position size (V3 0 0 0) 0 uv uvSize 0 0

    loadMetrics charcode = do
        FreeType.ft_Load_Char face (fromIntegral (ord charcode :: Int)) (FreeType.FT_LOAD_DEFAULT .|. FreeType.FT_LOAD_NO_BITMAP)
        metrics <- FreeType.gsrMetrics <$> (Foreign.peek . FreeType.frGlyph =<< Foreign.peek face)
        let w = fromIntegral (FreeType.gmWidth metrics) `shift` (-6)
            h = fromIntegral (FreeType.gmHeight metrics) `shift` (-6)
            bx = fromIntegral (FreeType.gmHoriBearingX metrics) `shift` (-6)
            by = fromIntegral (FreeType.gmHoriBearingY metrics) `shift` (-6)
            advance = fromIntegral (FreeType.gmHoriAdvance metrics) `shift` (-6)
        return (charcode, Character charcode (V2 w h) (V2 bx by) advance)

    renderBitmap (V2 twidth theight) p texture (V2 x y, results) charcode =
        FreeType.ft_Bitmap_With freeType $ \convertBitmap -> do
            FreeType.ft_Load_Char face (fromIntegral $ ord charcode) FreeType.FT_LOAD_RENDER
            slot <- Foreign.peek . FreeType.frGlyph =<< Foreign.peek face
            let source = FreeType.gsrBitmap slot

            bitmap <- if FreeType.bPixel_mode source /= FreeType.FT_PIXEL_MODE_GRAY
                then Foreign.with source $ \sourceBitmap -> do
                    FreeType.ft_Bitmap_Convert freeType sourceBitmap convertBitmap 1
                    Foreign.peek convertBitmap
                else return source

            let width = FreeType.bWidth bitmap
                height = FreeType.bRows bitmap
                buffer = FreeType.bBuffer bitmap
                pitch = FreeType.bPitch bitmap
                nextPos
                    | x + fromIntegral width + padding + fromIntegral pixelSize <= fromIntegral twidth = V2 (x + fromIntegral width + padding) y
                    | otherwise = V2 0 (y + fromIntegral pixelSize)

            when (width > 0 && height > 0) $ forM_ [0..(fromIntegral $ width * height - 1)] $ \i -> do
                let (py, px) = divMod i (fromIntegral width)
                color <- Foreign.peekElemOff buffer (py * fromIntegral pitch + px)
                Foreign.pokeElemOff p i (V4 (255 - color) (255 - color) (255 - color) color)

            GLW.glTextureSubImage2D texture 0 x y (fromIntegral width) (fromIntegral height) GL.GL_RGBA GL.GL_UNSIGNED_BYTE (Foreign.castPtr p)
            return (nextPos, results ++ [(charcode, V2 x y)])
