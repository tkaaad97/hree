{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Graphics.Hree.Text
    ( FontFace
    , OriginLocation(..)
    , TextOption
    , PartialTextOption
    , GlyphInfo
    , createText
    , deleteFontFace
    , newFontFace
    ) where

import Control.Monad (forM_, when)
import Data.Bits (shift, (.|.))
import Data.Char (ord)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List (find, partition, sort)
import qualified Data.Map.Strict as Map (fromList, lookup, (!))
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..))
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as BV (fromList, (!))
import qualified Data.Vector.Storable as SV (generate, length)
import qualified Data.Vector.Unboxed as UV (Vector, foldl', fromList, imap,
                                            length, mapMaybe, scanl', zip, (!))
import Data.Word (Word8)
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

data GlyphInfo = GlyphInfo
    { glyphInfoCharcode :: !Char
    , glyphInfoSize     :: !(V2 Int)
    , glyphInfoBearing  :: !(V2 Int)
    , glyphInfoAdvance  :: !Int
    } deriving (Show, Eq)

data Packed = Packed
    { packedLayouts :: ![Layout]
    , packedSpaces  :: ![Rect]
    } deriving (Show, Eq)

data Layout = Layout
    { layoutIndex    :: !Int
    , layoutPosition :: !(V2 Int)
    } deriving (Show, Eq)

data Rect = Rect
    { rectArea     :: !Int
    , rectSize     :: !(V2 Int)
    , rectPosition :: !(V2 Int)
    } deriving (Show, Eq, Ord)

data TextOption_ f = TextOption
    { characterHeight  :: !(f Float)
    , characterSpacing :: !(f Float)
    , lineSpacing      :: !(f Float)
    , pixelWidth       :: !(f Int)
    , pixelHeight      :: !(f Int)
    , originPosition   :: !(f (V2 Float))
    , originLocation   :: !(f OriginLocation)
    , faceColor        :: !(f (V4 Word8))
    }

data OriginLocation =
    OriginLocationBottom |
    OriginLocationTop |
    OriginLocationFirstBaseLine |
    OriginLocationLastBaseLine
    deriving (Show, Eq, Enum)

type TextOption = TextOption_ Identity
type PartialTextOption = TextOption_ Last

deriving instance Eq TextOption
deriving instance Eq PartialTextOption
deriving instance Show TextOption
deriving instance Show PartialTextOption

instance Semigroup PartialTextOption where
    a <> b = TextOption
        { characterHeight = characterHeight a <> characterHeight b
        , characterSpacing = characterSpacing a <> characterSpacing b
        , lineSpacing = lineSpacing a <> lineSpacing b
        , pixelWidth = pixelWidth a <> pixelWidth b
        , pixelHeight = pixelHeight a <> pixelHeight b
        , originPosition = originPosition a <> originPosition b
        , originLocation = originLocation a <> originLocation b
        , faceColor = faceColor a <> faceColor b
        }

instance Monoid PartialTextOption where
    mempty = TextOption
        { characterHeight = mempty
        , characterSpacing = mempty
        , lineSpacing = mempty
        , pixelWidth = mempty
        , pixelHeight = mempty
        , originPosition = mempty
        , originLocation = mempty
        , faceColor = mempty
        }

defaultTextOption :: TextOption
defaultTextOption = TextOption
    { characterHeight = pure (1 / 16)
    , characterSpacing = pure 0
    , lineSpacing = pure 0
    , pixelWidth = pure 0
    , pixelHeight = pure 128
    , originPosition = pure (V2 0 0)
    , originLocation = pure OriginLocationBottom
    , faceColor = pure (V4 0 0 0 255)
    }

overrideTextOption :: TextOption -> PartialTextOption -> TextOption
overrideTextOption option override = TextOption
    { characterHeight = fromMaybe (characterHeight option) (toIdentity . characterHeight $ override)
    , characterSpacing = fromMaybe (characterSpacing option) (toIdentity . characterSpacing $ override)
    , lineSpacing = fromMaybe (lineSpacing option) (toIdentity . lineSpacing $ override)
    , pixelWidth = fromMaybe (pixelWidth option) (toIdentity . pixelWidth $ override)
    , pixelHeight = fromMaybe (pixelHeight option) (toIdentity . pixelHeight $ override)
    , originPosition = fromMaybe (originPosition option) (toIdentity . originPosition $ override)
    , originLocation = fromMaybe (originLocation option) (toIdentity . originLocation $ override)
    , faceColor = fromMaybe (faceColor option) (toIdentity . faceColor $ override)
    }
    where
    toIdentity = fmap Identity . getLast

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

createText :: Hree.Scene -> FontFace -> Text -> Int -> IO Hree.NodeId
createText scene (FontFace freeType face) text pixelSize = do
    let str = Text.unpack text
        charcodes = nubOrd str

    isScalable <- FreeType.FT_IS_SCALABLE face
    when isScalable
      $ FreeType.ft_Set_Pixel_Sizes face 0 (fromIntegral pixelSize)

    glyphs <- mapM loadMetrics charcodes
    let glyphMap = Map.fromList . map (\a -> (glyphInfoCharcode a, a)) $ glyphs
        glyphVec = BV.fromList glyphs
        glyphSizeVec = UV.fromList $ map glyphInfoSize glyphs
        packResults = packGlyphs textureSize 1 glyphSizeVec

    materials <- mapM (createMaterial glyphVec) packResults
    let uvMap = Map.fromList .
            map (\(materialIndex, Layout index pos) ->
                    let glyph = glyphVec BV.! index
                    in (glyphInfoCharcode glyph, (index, materialIndex, toUv glyph pos))) .
            concatMap (\(i, xs) -> zip (repeat i) xs) .
            zip ([0..] :: [Int]) . map packedLayouts $ packResults

    let charVec = UV.fromList str
        charPosVec = UV.zip charVec . UV.scanl' (calcCharPos glyphMap 0) (V2 0 0) $ charVec

    meshIds <- map Hree.addedMeshId <$> mapM (\(materialIndex, material) -> createMesh materialIndex material uvMap glyphVec charPosVec)
                (zip [0..] materials)
    childNodeIds <- mapM (\meshId -> Hree.addNode scene Hree.newNode { Hree.nodeMesh = Just meshId } False) meshIds
    Hree.addNode scene Hree.newNode { Hree.nodeChildren = BV.fromList childNodeIds } False

    where
    textureSize = V2 1024 1024 :: V2 Int
    V2 twidth theight = textureSize
    unitLength = 1024 :: Int

    calcCharPos glyphMap sx (V2 x y) c
        | c == '\n' = V2 sx (y - fromIntegral pixelSize / fromIntegral unitLength)
        | otherwise =
            let advance = glyphInfoAdvance $ glyphMap Map.! c
            in V2 (x + fromIntegral advance / fromIntegral unitLength) y

    toUv glyph (V2 x y) =
        let V2 _ gh = glyphInfoSize glyph
        in V2 (fromIntegral x / fromIntegral twidth) (fromIntegral (y + gh) / fromIntegral theight)

    toSpriteVertex glyphVec (index, V2 sx sy, uv) =
        let GlyphInfo _ (V2 w h) (V2 bx by) _ = glyphVec BV.! index
            size = V3 (fromIntegral w / fromIntegral unitLength) (fromIntegral h / fromIntegral unitLength) 0
            uvSize = V2 (fromIntegral w / fromIntegral twidth) (- fromIntegral h / fromIntegral theight)
            x = sx + fromIntegral bx / fromIntegral unitLength
            y = sy + fromIntegral (by - h) / fromIntegral unitLength
            position = V3 x y 0
        in Hree.SpriteVertex position size (V3 0 0 0) 0 uv uvSize 0 0

    loadMetrics charcode = do
        FreeType.ft_Load_Char face (fromIntegral . ord $ charcode) (FreeType.FT_LOAD_DEFAULT .|. FreeType.FT_LOAD_NO_BITMAP)
        metrics <- FreeType.gsrMetrics <$> (Foreign.peek . FreeType.frGlyph =<< Foreign.peek face)
        let w = fromIntegral (FreeType.gmWidth metrics) `shift` (-6)
            h = fromIntegral (FreeType.gmHeight metrics) `shift` (-6)
            bx = fromIntegral (FreeType.gmHoriBearingX metrics) `shift` (-6)
            by = fromIntegral (FreeType.gmHoriBearingY metrics) `shift` (-6)
            advance = fromIntegral (FreeType.gmHoriAdvance metrics) `shift` (-6)
        return (GlyphInfo charcode (V2 w h) (V2 bx by) advance)

    createMaterial glyphs (Packed layouts _) = do
        let settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral twidth) (fromIntegral theight) False
            sourceData = Hree.TextureSourceData (fromIntegral twidth) (fromIntegral theight) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE Foreign.nullPtr

        (tname, texture) <- Hree.addTexture scene "textmesh" settings sourceData
        (_, sampler) <- Hree.addSampler scene tname
        let material = Hree.spriteMaterial { Hree.materialTextures = pure (Hree.BaseColorMapping, Hree.Texture (texture, sampler)) }
        Foreign.allocaArray (pixelSize * pixelSize * 4) $ \p ->
            mapM_ (renderBitmap p texture glyphs) layouts

        return material

    renderBitmap p texture glyphs (Layout index (V2 x y)) =
        FreeType.ft_Bitmap_With freeType $ \convertBitmap -> do
            let charcode = glyphInfoCharcode $ glyphs BV.! index
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

            when (width > 0 && height > 0) $ do
                forM_ [0..(fromIntegral $ width * height - 1)] $ \i -> do
                    let (py, px) = divMod i (fromIntegral width)
                    color <- Foreign.peekElemOff buffer (py * fromIntegral pitch + px)
                    Foreign.pokeElemOff p i (V4 (255 - color) (255 - color) (255 - color) color)
                GLW.glTextureSubImage2D texture 0 (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height) GL.GL_RGBA GL.GL_UNSIGNED_BYTE (Foreign.castPtr p)

    createMesh materialIndex material uvMap glyphVec charPosVec = do
        let xs = flip UV.mapMaybe charPosVec $ \(char, pos) -> do
                (i, m, uv) <- Map.lookup char uvMap
                let glyph = glyphVec BV.! i
                    V2 gw gh = glyphInfoSize glyph
                if m == materialIndex && gw > 0 && gh > 0 && char /= '\n'
                    then return (i, pos, uv)
                    else Nothing
            vs = SV.generate (UV.length xs) (toSpriteVertex glyphVec . (xs UV.!))
        (geo, _) <- Hree.newSpriteGeometry scene
        geo' <- Hree.addVerticesToGeometry geo vs GL.GL_STATIC_READ scene
        let mesh = Hree.Mesh geo' material . Just . SV.length $ vs
        Hree.addMesh scene mesh

packGlyphs :: V2 Int -> Int -> UV.Vector (V2 Int) -> [Packed]
packGlyphs (V2 textureWidth textureHeight) spacing =
    reverse . UV.foldl' pack [] . UV.imap (,)

    where
    pack :: [Packed] -> (Int, V2 Int) -> [Packed]
    pack ps a =
        fromMaybe (newRegion a : ps) (tryPack a ps)

    newRegion (index, V2 w h) =
        let layouts = [Layout index (V2 spacing spacing)]
            spaces = relocateSpaces (V2 spacing spacing) (V2 (w + spacing) (h + spacing)) (newRect (V2 spacing spacing) (V2 (textureWidth - spacing) (textureHeight - spacing)))
        in Packed layouts spaces

    tryPack _ [] = Nothing

    tryPack (index, V2 w h) (r : rs) =
        case tryPackOne (index, V2 (w + spacing) (h + spacing)) r of
            Just r' -> Just (r' : rs)
            Nothing -> fmap (r :) (tryPack (index, V2 w h) rs)

tryPackOne :: (Int, V2 Int) -> Packed -> Maybe Packed
tryPackOne (index, V2 w h) (Packed layouts spaces) = do
    Rect _ _ rp <- List.find locatable spaces
    let layout = Layout index rp
        size = V2 w h
        (intersectSpaces, restSpaces) = List.partition (hasIntersection rp size) spaces
        relocated = removeInclusion . concatMap (relocateSpaces rp size) $ intersectSpaces
    return $ Packed (layout : layouts) (restSpaces ++ relocated)
    where
    locatable (Rect _ (V2 rw rh) _) = rw >= w && rh >= h

hasIntersection :: V2 Int -> V2 Int -> Rect -> Bool
hasIntersection (V2 x y) (V2 w h) (Rect _ (V2 rw rh) (V2 rx ry)) = dx < w + rw && dy < h + rh
    where
    (cx, cy) = (x * 2 + w, y * 2 + h)
    (rcx, rcy) = (rx * 2 + rw, ry * 2 + rh)
    (dx, dy) = (abs (cx - rcx), abs (cy - rcy))

relocateSpaces :: V2 Int -> V2 Int -> Rect -> [Rect]
relocateSpaces p s r =
    horizontalSpaces p s r ++ verticalSpaces p s r

horizontalSpaces :: V2 Int -> V2 Int -> Rect -> [Rect]
horizontalSpaces (V2 _ y) (V2 _ h) (Rect _ (V2 rw rh) (V2 rx ry))
    | ry < y && (y + h) < (ry + rh) = [s1, s2]
    | ry < y = [s1]
    | (y + h) < (ry + rh) = [s2]
    | otherwise = []
    where
    s1 = newRect (V2 rx ry) (V2 rw (y - ry))
    s2 = newRect (V2 rx (y + h)) (V2 rw (ry + rh - y - h))

verticalSpaces :: V2 Int -> V2 Int -> Rect -> [Rect]
verticalSpaces (V2 x _) (V2 w _) (Rect _ (V2 rw rh) (V2 rx ry))
    | rx < x && (x + w) < (rx + rw) = [s1, s2]
    | rx < x = [s1]
    | (x + w) < (rx + rw) = [s2]
    | otherwise = []
    where
    s1 = newRect (V2 rx ry) (V2 (x - rx) rh)
    s2 = newRect (V2 (x + w) ry) (V2 (rx + rw - x - w) rh)

removeInclusion :: [Rect] -> [Rect]
removeInclusion = removeInclusion' . List.sort
    where
    removeInclusion' [] = []
    removeInclusion' (x : xs)
        | any (inclusion x) xs = removeInclusion' xs
        | otherwise = x : removeInclusion' xs
    inclusion (Rect _ (V2 w h) (V2 x y)) (Rect _ (V2 rw rh) (V2 rx ry)) =
        rx <= x && x + w <= rx + rw && ry <= y && y + h <= ry + rh

newRect :: V2 Int -> V2 Int -> Rect
newRect p (V2 w h) = Rect (w * h) (V2 w h) p
