{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Graphics.Hree.Text
    ( FontFace
    , FontOption
    , FontOption_(..)
    , PartialFontOption
    , OriginLocation(..)
    , TextOption
    , TextOption_(..)
    , PartialTextOption
    , GlyphInfo
    , createText
    , createTextWithOption
    , deleteFontFace
    , newFontFace
    ) where

import Control.Monad (forM_, when)
import Data.Bits (shift, (.|.))
import Data.Char (ord)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity(..))
import qualified Data.HashTable.IO as HT
import Data.IORef (IORef)
import qualified Data.List as List (delete, find, partition, sort)
import qualified Data.Map.Strict as Map (fromList, lookup, (!))
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..))
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as BV (Vector, fromList, (!))
import qualified Data.Vector.Storable as SV (generate, length)
import qualified Data.Vector.Unboxed as UV (Vector, foldl', fromList, imap,
                                            last, length, mapMaybe, null,
                                            scanl', zip, (!))
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

data Font = Font !FontFace !FontOption !SizeInfo !(IORef FontState)

data SizeInfo = SizeInfo
    { sizeInfoPixelSize      :: !(V2 Int)
    , sizeInfoUnitsPerEM     :: !Int
    , sizeInfoAscenderUnits  :: !Int
    , sizeInfoDescenderUnits :: !Int
    } deriving (Show, Eq)

data FontState = FontState
    { fontStateCharVec     :: !(UV.Vector Char)
    , fontStateGlyphVec    :: !(BV.Vector GlyphInfo)
    , fontStateCharUvMap   :: !(HT.BasicHashTable Char UvInfo)
    , fontStatePackedVec   :: !(BV.Vector Packed)
    , fontStateMaterialVec :: !(BV.Vector Hree.SpriteMaterial)
    }

data UvInfo = UvInfo
    { uvInfoMaterialIndex :: !Int
    , uvInfoUvPos         :: !(V2 Float)
    , uvInfoUvSize        :: !(V2 Float)
    } deriving (Show ,Eq)

data GlyphInfo = GlyphInfo
    { glyphInfoCharcode :: !Char
    , glyphInfoSize     :: !(V2 Int)
    , glyphInfoBearing  :: !(V2 Double)
    , glyphInfoAdvance  :: !Double
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

data FontOption_ f = FontOption
    { pixelWidth  :: !(f Int)
    , pixelHeight :: !(f Int)
    , faceColor   :: !(f (V4 Word8))
    , textureSize :: !(f (V2 Int))
    }

type FontOption = FontOption_ Identity
type PartialFontOption = FontOption_ Last

deriving instance Eq FontOption
deriving instance Eq PartialFontOption
deriving instance Show FontOption
deriving instance Show PartialFontOption

instance Semigroup PartialFontOption where
    a <> b = FontOption
        { pixelWidth = pixelWidth a <> pixelWidth b
        , pixelHeight = pixelHeight a <> pixelHeight b
        , faceColor = faceColor a <> faceColor b
        , textureSize = textureSize a <> textureSize b
        }

instance Monoid PartialFontOption where
    mempty = FontOption
        { pixelWidth = mempty
        , pixelHeight = mempty
        , faceColor = mempty
        , textureSize = mempty
        }

data TextOption_ f = TextOption
    { characterHeight  :: !(f Float)
    , characterSpacing :: !(f Float)
    , lineSpacing      :: !(f Float)
    , originPosition   :: !(f (V2 Float))
    , originLocation   :: !(f OriginLocation)
    }

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
        , originPosition = originPosition a <> originPosition b
        , originLocation = originLocation a <> originLocation b
        }

instance Monoid PartialTextOption where
    mempty = TextOption
        { characterHeight = mempty
        , characterSpacing = mempty
        , lineSpacing = mempty
        , originPosition = mempty
        , originLocation = mempty
        }

data OriginLocation =
    OriginLocationBottom |
    OriginLocationTop |
    OriginLocationFirstBaseLine |
    OriginLocationLastBaseLine
    deriving (Show, Read, Eq, Enum)

defaultFontOption :: FontOption
defaultFontOption = FontOption
    { pixelWidth = pure 0
    , pixelHeight = pure 64
    , faceColor = pure (V4 0 0 0 255)
    , textureSize = pure (V2 2048 2048)
    }

overrideFontOption :: FontOption -> PartialFontOption -> FontOption
overrideFontOption option override = FontOption
    { pixelWidth = fromMaybe (pixelWidth option) (toIdentity . pixelWidth $ override)
    , pixelHeight = fromMaybe (pixelHeight option) (toIdentity . pixelHeight $ override)
    , faceColor = fromMaybe (faceColor option) (toIdentity . faceColor $ override)
    , textureSize = fromMaybe (textureSize option) (toIdentity . textureSize $ override)
    }
    where
    toIdentity = fmap Identity . getLast

defaultTextOption :: TextOption
defaultTextOption = TextOption
    { characterHeight = pure (1 / 16)
    , characterSpacing = pure 0
    , lineSpacing = pure 0
    , originPosition = pure (V2 0 0)
    , originLocation = pure OriginLocationBottom
    }

overrideTextOption :: TextOption -> PartialTextOption -> TextOption
overrideTextOption option override = TextOption
    { characterHeight = fromMaybe (characterHeight option) (toIdentity . characterHeight $ override)
    , characterSpacing = fromMaybe (characterSpacing option) (toIdentity . characterSpacing $ override)
    , lineSpacing = fromMaybe (lineSpacing option) (toIdentity . lineSpacing $ override)
    , originPosition = fromMaybe (originPosition option) (toIdentity . originPosition $ override)
    , originLocation = fromMaybe (originLocation option) (toIdentity . originLocation $ override)
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

createText :: Hree.Scene -> FontFace -> Text -> Float -> IO Hree.NodeId
createText scene fontFace text charHeight =
    createTextWithOption scene fontFace text mempty partialOption
    where
    partialOption = mempty { characterHeight = pure charHeight }

createTextWithOption :: Hree.Scene -> FontFace -> Text -> PartialFontOption -> PartialTextOption -> IO Hree.NodeId
createTextWithOption scene (FontFace freeType face) text partialFontOption partialTextOption = do
    let str = Text.unpack text
        charcodes = List.delete '\n' . nubOrd $ str

    isScalable <- FreeType.FT_IS_SCALABLE face
    when isScalable
      $ FreeType.ft_Set_Pixel_Sizes face (fromIntegral . pixelWidth $ fontOption) (fromIntegral . pixelHeight $ fontOption)

    faceRec <- Foreign.peek face

    fontSizeMetrics <- fmap FreeType.srMetrics . Foreign.peek . FreeType.frSize $ faceRec
    let unitsPerEM = fromIntegral (FreeType.frUnits_per_EM faceRec) :: Int
        pixelSizeX = fromIntegral (FreeType.smX_ppem fontSizeMetrics) :: Int
        pixelSizeY = fromIntegral (FreeType.smY_ppem fontSizeMetrics) :: Int
        pixelSize = V2 pixelSizeX pixelSizeY
        unitLen = realToFrac charH / fromIntegral unitsPerEM :: Double
        pixelLenY = realToFrac charH / fromIntegral pixelSizeY :: Double
        pixelLenX = pixelLenY * fromIntegral pixelSizeY / fromIntegral pixelSizeX
        pixelLen = V2 pixelLenX pixelLenY
        ascender = unitLen * fromIntegral (FreeType.frAscender faceRec) :: Double
        descender = - unitLen * fromIntegral (FreeType.frDescender faceRec) :: Double

    glyphs <- mapM loadMetrics charcodes
    let glyphMap = Map.fromList . map (\a -> (glyphInfoCharcode a, a)) $ glyphs
        glyphVec = BV.fromList glyphs
        glyphSizeVec = UV.fromList $ map glyphInfoSize glyphs
        packResults = packGlyphs (runIdentity . textureSize $ fontOption) 1 glyphSizeVec

    materials <- mapM (createMaterial pixelSize glyphVec) packResults
    let uvMap = Map.fromList .
            map (\(materialIndex, Layout index pos) ->
                    let glyph = glyphVec BV.! index
                        (_, textureSize') = materials !! materialIndex
                    in (glyphInfoCharcode glyph, (index, materialIndex, toUv textureSize' glyph pos))) .
            concatMap (\(i, xs) -> zip (repeat i) xs) .
            zip ([0..] :: [Int]) . map packedLayouts $ packResults

    let charVec = UV.fromList str
        charPosVec = UV.zip charVec . UV.scanl' (calcCharPos pixelLenX glyphMap 0) (V2 0 0) $ charVec

    meshIds <- map Hree.addedMeshId <$> mapM (\(materialIndex, material) -> createMesh  ascender descender pixelLen materialIndex material uvMap glyphVec charPosVec)
                (zip [0..] materials)
    childNodeIds <- mapM (\meshId -> Hree.addNode scene Hree.newNode { Hree.nodeMesh = Just meshId } False) meshIds
    Hree.addNode scene Hree.newNode { Hree.nodeChildren = BV.fromList childNodeIds } False

    where
    fontOption = overrideFontOption defaultFontOption partialFontOption
    textOption = overrideTextOption defaultTextOption partialTextOption
    V2 twidth theight = runIdentity . textureSize $ fontOption
    lineS = runIdentity . lineSpacing $ textOption
    charH = runIdentity . characterHeight $ textOption
    charS = runIdentity . characterSpacing $ textOption

    calcCharPos pixelLenX glyphMap sx (V2 x y) c
        | c == '\n' = V2 sx (y - realToFrac (charH + lineS))
        | otherwise =
            let advance = glyphInfoAdvance $ glyphMap Map.! c
            in V2 (x + advance * pixelLenX + realToFrac charS) y

    toUv (V2 tw th) glyph (V2 x y) =
        let V2 _ gh = glyphInfoSize glyph
        in V2 (fromIntegral x / tw) (fromIntegral (y + gh) / th)

    toSpriteVertex (V2 tw th) (V2 ox oy) (V2 pixelLenX pixelLenY) glyphVec (index, V2 sx sy, uv) =
        let GlyphInfo _ (V2 w h) (V2 bx by) _ = glyphVec BV.! index
            size = V3 (realToFrac (fromIntegral w * pixelLenX)) (realToFrac (fromIntegral h * pixelLenY)) 0
            uvSize = V2 (fromIntegral w / tw) (- fromIntegral h / th)
            x = realToFrac (sx + bx * pixelLenX - ox)
            y = realToFrac (sy + (by - fromIntegral h) * pixelLenY - oy)
            position = V3 x y 0
        in Hree.SpriteVertex position size (V3 0 0 0) 0 uv uvSize 0 0

    loadMetrics charcode = do
        FreeType.ft_Load_Char face (fromIntegral . ord $ charcode) (FreeType.FT_LOAD_DEFAULT .|. FreeType.FT_LOAD_NO_BITMAP)
        metrics <- FreeType.gsrMetrics <$> (Foreign.peek . FreeType.frGlyph =<< Foreign.peek face)
        let w = fromIntegral (FreeType.gmWidth metrics) `shift` (-6)
            h = fromIntegral (FreeType.gmHeight metrics) `shift` (-6)
            bx = fromIntegral (FreeType.gmHoriBearingX metrics) / 64
            by = fromIntegral (FreeType.gmHoriBearingY metrics) / 64
            advance = fromIntegral (FreeType.gmHoriAdvance metrics) / 64
        return (GlyphInfo charcode (V2 w h) (V2 bx by) advance)


    createMaterial (V2 pixelSizeX pixelSizeY) glyphs (Packed layouts _) = do
        let V2 twidth' theight' = minimizeTextureSize glyphs layouts
            settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral twidth') (fromIntegral theight') False
            sourceData = Hree.TextureSourceData (fromIntegral twidth) (fromIntegral theight) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE Foreign.nullPtr

        (tname, texture) <- Hree.addTexture scene "textmesh" settings sourceData
        (_, sampler) <- Hree.addSampler scene tname
        let material = Hree.spriteMaterial { Hree.materialTextures = pure (Hree.BaseColorMapping, Hree.Texture (texture, sampler)) }
        Foreign.allocaArray (pixelSizeX * pixelSizeY * 4) $ \p ->
            mapM_ (renderBitmap p texture glyphs) layouts

        return (material, fromIntegral <$> V2 twidth' theight')

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

            when (width > 0 && height > 0 && charcode /= '\n') $ do
                forM_ [0..(fromIntegral $ width * height - 1)] $ \i -> do
                    let (py, px) = divMod i (fromIntegral width)
                    color <- Foreign.peekElemOff buffer (py * fromIntegral pitch + px)
                    Foreign.pokeElemOff p i (V4 (255 - color) (255 - color) (255 - color) color)
                GLW.glTextureSubImage2D texture 0 (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height) GL.GL_RGBA GL.GL_UNSIGNED_BYTE (Foreign.castPtr p)

    relocateOrigin _ descender bottom OriginLocationBottom (V2 ox oy) = V2 ox (oy + bottom - descender)
    relocateOrigin ascender _ _ OriginLocationTop (V2 ox oy) = V2 ox (oy + ascender)
    relocateOrigin _ _ _ OriginLocationFirstBaseLine (V2 ox oy) = V2 ox oy
    relocateOrigin _ _ bottom OriginLocationLastBaseLine (V2 ox oy) = V2 ox (oy + bottom)

    createMesh ascender descender pixelLen materialIndex (material, textureSize') uvMap glyphVec charPosVec = do
        let xs = flip UV.mapMaybe charPosVec $ \(char, pos) -> do
                (i, m, uv) <- Map.lookup char uvMap
                let glyph = glyphVec BV.! i
                    V2 gw gh = glyphInfoSize glyph
                if m == materialIndex && gw > 0 && gh > 0 && char /= '\n'
                    then return (i, pos, uv)
                    else Nothing
            V2 _ bottomLine = if UV.null xs then V2 0 0 else (\(_, a, _) -> a) $ UV.last xs
            origin = relocateOrigin ascender descender bottomLine (runIdentity $ originLocation textOption) (fmap realToFrac . runIdentity . originPosition $ textOption)
            vs = SV.generate (UV.length xs) (toSpriteVertex textureSize' origin pixelLen glyphVec . (xs UV.!))
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

minimizeTextureSize :: BV.Vector GlyphInfo -> [Layout] -> V2 Int
minimizeTextureSize glyphVec layouts = fmap minPow2 maxPos
    where
    bottomLeftPos (Layout i (V2 px py)) =
        let glyph = glyphVec BV.! i
            V2 sx sy = glyphInfoSize glyph
        in V2 (px + sx) (py + sy)
    maxPos = foldl' (\(V2 ax ay) (V2 bx by) -> V2 (max ax bx) (max ay by)) (V2 1 1) (map bottomLeftPos layouts)
    minPow2 = minPow2_ 1
    minPow2_ a x
        | a >= x = a
        | otherwise = minPow2_ (a * 2) x

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
