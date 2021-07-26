{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Hree.Text
    ( Font
    , FontFace
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
    , loadCharactersIntoFont
    , deleteFont
    , newFont
    , newFontWithOption
    ) where

import Control.Exception (throwIO)
import Control.Monad (forM_, unless, void, when)
import Data.Bits (shift, (.|.))
import Data.Char (ord)
import Data.Functor.Identity (Identity(..))
import qualified Data.HashTable.IO as HT
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.List as List (find, partition, sort)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as BV (Vector, generateM, imapM, last, length,
                                    null, snoc, take, (!))
import qualified Data.Vector.Algorithms.Intro as V (sort)
import qualified Data.Vector.Storable as SV (generate, length)
import qualified Data.Vector.Unboxed as UV (Vector, filter, filterM, foldl',
                                            freeze, fromList, generate, imap,
                                            last, length, mapM, null, scanl',
                                            thaw, uniq, zip, (!))
import Data.Word (Word8)
import qualified Foreign (allocaArray, castPtr, newForeignPtr_, nullPtr, peek,
                          peekElemOff, pokeElemOff, with)
import qualified FreeType
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Hree.Geometry as Hree (addVerticesToGeometry, spriteGeometry)
import qualified Hree.GL.Vertex as Hree (SpriteVertex(..))
import qualified Hree.Material.SpriteMaterial as Hree (SpriteMaterialBlock,
                                                       spriteMaterial)
import qualified Hree.Scene as Hree (addMaterial, addMesh, addNode,
                                     updateMaterialMappingSubImage)
import qualified Hree.Types as Hree (MappingSource(..), Material(..),
                                     MaterialId, Mesh(..), Node(..), NodeId,
                                     Scene, TextureMappingType(..),
                                     TextureSettings(..), TextureSourceData(..))
import qualified Hree.Utility as Hree (mesh, node)
import Linear (V2(..), V3(..), V4(..))
import System.Directory (canonicalizePath)

data FontFace = FontFace !FreeType.FT_Library !FreeType.FT_Face

data Font = Font !Hree.Scene !FontFace !FontOption !SizeInfo !(IORef FontState)

data SizeInfo = SizeInfo
    { sizeInfoPixelSize      :: !(V2 Int)
    , sizeInfoUnitsPerEM     :: !Int
    , sizeInfoAscenderUnits  :: !Int
    , sizeInfoDescenderUnits :: !Int
    } deriving (Show, Eq)

data FontState = FontState
    { fontStateDeleted     :: !Bool
    , fontStateCharVec     :: !(UV.Vector Char)
    , fontStateGlyphVec    :: !(BV.Vector GlyphInfo)
    , fontStateCharUvMap   :: !(HT.BasicHashTable Char UvInfo)
    , fontStatePackedVec   :: !(BV.Vector Packed)
    , fontStateMaterialVec :: !(BV.Vector MaterialInfo)
    }

data UvInfo = UvInfo
    { uvInfoGlyphIndex    :: !Int
    , uvInfoMaterialIndex :: !Int
    , uvInfoUvPos         :: !(V2 Float)
    , uvInfoUvSize        :: !(V2 Float)
    } deriving (Show ,Eq)

newtype MaterialInfo = MaterialInfo (Hree.MaterialId Hree.SpriteMaterialBlock)

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

newFont :: Hree.Scene -> FilePath -> IO Font
newFont scene fontPath = newFontWithOption scene fontPath mempty

newFontWithOption :: Hree.Scene -> FilePath -> PartialFontOption -> IO Font
newFontWithOption scene fontPath partialOption = do
    face <- newFontFace fontPath
    setFontPixelSize face pixelW pixelH
    sizeInfo <- loadFontSizeInfo face
    charUvMap <- HT.newSized reserveSize
    let state = FontState False mempty mempty charUvMap mempty mempty
    stateRef <- newIORef state
    return (Font scene face option sizeInfo stateRef)
    where
    option = overrideFontOption defaultFontOption partialOption
    pixelW = runIdentity . pixelWidth $ option
    pixelH = runIdentity . pixelHeight $ option
    reserveSize = 256

deleteFont :: Font -> IO ()
deleteFont (Font _ face _ _ stateRef) = do
    state <- readIORef stateRef
    unless (fontStateDeleted state) $ do
        deleteFontFace face
        charUvMap <- HT.new
        let deletedState = FontState True mempty mempty charUvMap mempty mempty
        writeIORef stateRef deletedState
        -- TODO delete textures

createText :: Font -> Text -> Float -> IO Hree.NodeId
createText fontFace text charHeight =
    createTextWithOption fontFace text partialOption
    where
    partialOption = mempty { characterHeight = pure charHeight }

createTextWithOption :: Font -> Text -> PartialTextOption -> IO Hree.NodeId
createTextWithOption font text partialTextOption = do
    fontState <- loadCharactersIntoFont_ font str

    let charVec = UV.fromList str
        glyphVec = fontStateGlyphVec fontState
        charUvMap = fontStateCharUvMap fontState
        materials = fontStateMaterialVec fontState

    charPosVec <- UV.zip charVec . UV.scanl' (calcCharPos 0) (V2 0 0) <$> UV.mapM (getCharAdvance glyphVec charUvMap) charVec

    meshIds <- BV.imapM (createMesh fontState charPosVec) materials
    childNodeIds <- mapM (\meshId -> Hree.addNode scene Hree.node (Just meshId) False) meshIds
    Hree.addNode scene Hree.node { Hree.nodeChildren = childNodeIds } Nothing False

    where
    Font scene _ _ sizeInfo _ = font
    str = Text.unpack text
    textOption = overrideTextOption defaultTextOption partialTextOption
    lineS = runIdentity . lineSpacing $ textOption
    charH = runIdentity . characterHeight $ textOption
    charS = runIdentity . characterSpacing $ textOption
    V2 pixelSizeX pixelSizeY = sizeInfoPixelSize sizeInfo
    unitsPerEM = sizeInfoUnitsPerEM sizeInfo
    unitLen = realToFrac charH / fromIntegral unitsPerEM :: Double
    pixelLenY = realToFrac charH / fromIntegral pixelSizeY :: Double
    pixelLenX = pixelLenY * fromIntegral pixelSizeY / fromIntegral pixelSizeX
    ascender = unitLen * fromIntegral (sizeInfoAscenderUnits sizeInfo) :: Double
    descender = - unitLen * fromIntegral (sizeInfoDescenderUnits sizeInfo) :: Double

    getCharAdvance glyphVec charUvMap c
        | c == '\n' = return (c, 0)
        | otherwise = do
            maybeUvInfo <- HT.lookup charUvMap c
            case maybeUvInfo of
                Just uvInfo -> do
                    let glyphIndex = uvInfoGlyphIndex uvInfo
                    return (c, glyphInfoAdvance (glyphVec BV.! glyphIndex))
                Nothing -> return (c, 0)


    calcCharPos sx (V2 x y) (c, advance)
        | c == '\n' = V2 sx (y - realToFrac (charH + lineS))
        | otherwise = V2 (x + advance * pixelLenX + realToFrac charS) y

    toSpriteVertex (V2 ox oy) glyphVec (index, (V2 sx sy, (uv, uvSize))) =
        let GlyphInfo _ (V2 w h) (V2 bx by) _ = glyphVec BV.! index
            size = V3 (realToFrac (fromIntegral w * pixelLenX)) (realToFrac (fromIntegral h * pixelLenY)) 0
            x = realToFrac (sx + bx * pixelLenX - ox)
            y = realToFrac (sy + (by - fromIntegral h) * pixelLenY - oy)
            position = V3 x y 0
        in Hree.SpriteVertex position size (V3 0 0 0) 0 uv uvSize 0 0

    relocateOrigin bottom OriginLocationBottom (V2 ox oy) = V2 ox (oy + bottom - descender)
    relocateOrigin _ OriginLocationTop (V2 ox oy) = V2 ox (oy + ascender)
    relocateOrigin _ OriginLocationFirstBaseLine (V2 ox oy) = V2 ox oy
    relocateOrigin bottom OriginLocationLastBaseLine (V2 ox oy) = V2 ox (oy + bottom)

    createMesh fontState charPosVec materialIndex (MaterialInfo materialId) = do
        let FontState _ _ glyphVec charUvMap _ _ = fontState
        xs <- fmap (UV.filter ((>= 0) . fst)) . flip UV.mapM charPosVec $ \(char, pos) -> do
            maybeUvInfo <- HT.lookup charUvMap char
            case maybeUvInfo of
                Just uvInfo -> do
                    let UvInfo i m uv uvSize = uvInfo
                    if m == materialIndex && uvSize /= V2 0 0
                        then return (i, (pos, (uv, uvSize)))
                        else return (-1, (V2 0 0, (V2 0 0, V2 0 0)))
                Nothing -> return (-1, (V2 0 0, (V2 0 0, V2 0 0)))

        let V2 _ bottomLine = if UV.null xs then V2 0 0 else (\(_, (a, _)) -> a) $ UV.last xs
            origin = relocateOrigin bottomLine (runIdentity $ originLocation textOption) (fmap realToFrac . runIdentity . originPosition $ textOption)
            vs = SV.generate (UV.length xs) (toSpriteVertex origin glyphVec . (xs UV.!))

        let geo = Hree.addVerticesToGeometry Hree.spriteGeometry vs GL.GL_STATIC_READ
        let mesh = (Hree.mesh geo materialId) { Hree.meshInstanceCount = Just . SV.length $ vs }
        Hree.addMesh scene mesh

setFontPixelSize :: FontFace -> Int -> Int -> IO ()
setFontPixelSize (FontFace _ face) pixelW pixelH = do
    isScalable <- FreeType.FT_IS_SCALABLE face
    when isScalable
      $ FreeType.ft_Set_Pixel_Sizes face (fromIntegral pixelW) (fromIntegral pixelH)

loadFontSizeInfo :: FontFace -> IO SizeInfo
loadFontSizeInfo (FontFace _ face) = do
    faceRec <- Foreign.peek face
    fontSizeMetrics <- fmap FreeType.srMetrics . Foreign.peek . FreeType.frSize $ faceRec
    let unitsPerEM = fromIntegral (FreeType.frUnits_per_EM faceRec) :: Int
        pixelSizeX = fromIntegral (FreeType.smX_ppem fontSizeMetrics) :: Int
        pixelSizeY = fromIntegral (FreeType.smY_ppem fontSizeMetrics) :: Int
        pixelSize = V2 pixelSizeX pixelSizeY
        ascender = fromIntegral (FreeType.frAscender faceRec)
        descender = - fromIntegral (FreeType.frDescender faceRec)
    return (SizeInfo pixelSize unitsPerEM ascender descender)

loadCharactersIntoFont :: Font -> String -> IO ()
loadCharactersIntoFont font str = void (loadCharactersIntoFont_ font str)

loadCharactersIntoFont_ :: Font -> String -> IO FontState
loadCharactersIntoFont_ (Font scene (FontFace freeType face) fontOption sizeInfo stateRef) str = do
    state <- readIORef stateRef
    let FontState deleted charcodes glyphVec charUvMap packResults materials = state
        startGlyphIndex = UV.length charcodes
    when deleted . throwIO . userError $ "font deleted"

    addedCharcodes <- findNewCharacters charUvMap str
    let addedCharCount = UV.length addedCharcodes
    addedGlyphVec <- BV.generateM addedCharCount (loadMetrics . (addedCharcodes UV.!))
    let glyphSizeVec = UV.generate addedCharCount (glyphInfoSize . (addedGlyphVec BV.!))
        newPackResults = packGlyphs startGlyphIndex textureSize' 1 packResults glyphSizeVec
        newGlyphVec = glyphVec <> addedGlyphVec

    newMaterials <- BV.imapM (createOrUpdateMaterial charUvMap materials newGlyphVec startGlyphIndex) newPackResults

    let newCharcodes = charcodes `mappend` addedCharcodes
        newState = FontState deleted newCharcodes newGlyphVec charUvMap newPackResults newMaterials
    writeIORef stateRef newState
    return newState

    where
    pixelSize = sizeInfoPixelSize sizeInfo
    V2 pixelSizeX pixelSizeY = pixelSize
    textureSize' = runIdentity . textureSize $ fontOption
    V2 twidth theight = textureSize'
    V2 tw th = fmap fromIntegral textureSize'

    toUvInfo materialIndex (V2 gw gh) (Layout index (V2 x y)) =
        let uvPos = V2 (fromIntegral x / tw) (fromIntegral (y + gh) / th)
            uvSize = V2 (fromIntegral gw / tw) (- fromIntegral gh / th)
        in UvInfo index materialIndex uvPos uvSize

    loadMetrics charcode = do
        FreeType.ft_Load_Char face (fromIntegral . ord $ charcode) (FreeType.FT_LOAD_DEFAULT .|. FreeType.FT_LOAD_NO_BITMAP)
        metrics <- FreeType.gsrMetrics <$> (Foreign.peek . FreeType.frGlyph =<< Foreign.peek face)
        let w = fromIntegral (FreeType.gmWidth metrics) `shift` (-6)
            h = fromIntegral (FreeType.gmHeight metrics) `shift` (-6)
            bx = fromIntegral (FreeType.gmHoriBearingX metrics) / 64
            by = fromIntegral (FreeType.gmHoriBearingY metrics) / 64
            advance = fromIntegral (FreeType.gmHoriAdvance metrics) / 64
        return (GlyphInfo charcode (V2 w h) (V2 bx by) advance)

    createOrUpdateMaterial charUvMap materials glyphVec startGlyphIndex materialIndex (Packed layouts _) = do
        pixels <- Foreign.newForeignPtr_ Foreign.nullPtr
        materialInfo <- if materialIndex >= BV.length materials
            then do
                let settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral twidth) (fromIntegral theight) False
                    sourceData = Hree.TextureSourceData 0 0 PixelFormat.glRgba GL.GL_UNSIGNED_BYTE pixels
                    mapping = Hree.MappingSource settings sourceData []
                let material = Hree.spriteMaterial { Hree.materialMappings = pure (Hree.BaseColorMapping, mapping) }
                materialId <- Hree.addMaterial scene material
                return (MaterialInfo materialId)
            else return (materials BV.! materialIndex)

        when (materialIndex >= BV.length materials - 1) $ do
            let MaterialInfo materialId = materialInfo
                addedLayouts = filter ((>= startGlyphIndex) . layoutIndex) layouts

            Foreign.allocaArray (pixelSizeX * pixelSizeY * 4) $ \p ->
                mapM_ (renderBitmap p materialId glyphVec) addedLayouts

            mapM_ (insertCharUvMap charUvMap glyphVec materialIndex) addedLayouts

        return materialInfo

    insertCharUvMap charUvMap glyphVec materialIndex (Layout index p) = do
        let glyph = glyphVec BV.! index
            char = glyphInfoCharcode glyph
            uvInfo = toUvInfo materialIndex (glyphInfoSize glyph) (Layout index p)
        HT.insert charUvMap char uvInfo

    renderBitmap p materialId glyphs (Layout index (V2 x y)) =
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
                Hree.updateMaterialMappingSubImage scene materialId Hree.BaseColorMapping (V2 x y) (V2 (fromIntegral width) (fromIntegral height)) (Foreign.castPtr p)

findNewCharacters :: HT.BasicHashTable Char UvInfo -> String -> IO (UV.Vector Char)
findNewCharacters charMap str = do
    charcodes <- fmap (UV.filter (/= '\n')) . nubOrd . UV.fromList $ str
    UV.filterM (fmap (== Nothing) . HT.lookup charMap) charcodes
    where
    nubOrd :: UV.Vector Char -> IO (UV.Vector Char)
    nubOrd v = do
        mv <- UV.thaw v
        V.sort mv
        UV.uniq <$> UV.freeze mv

packGlyphs :: Int -> V2 Int -> Int -> BV.Vector Packed -> UV.Vector (V2 Int) -> BV.Vector Packed
packGlyphs start (V2 textureWidth textureHeight) spacing xs =
    UV.foldl' pack xs . UV.imap (,)

    where
    unsnoc v
        | BV.null v = Nothing
        | otherwise = Just (BV.take (BV.length v - 1) v, BV.last v)

    pack ps a = fromMaybe (ps `BV.snoc` newRegion a) (tryPack a (unsnoc ps))

    newRegion (index, V2 w h) =
        let layouts = pure (Layout (start + index) (V2 spacing spacing))
            spaces = relocateSpaces (V2 spacing spacing) (V2 (w + spacing) (h + spacing)) (newRect (V2 spacing spacing) (V2 (textureWidth - spacing) (textureHeight - spacing)))
        in Packed layouts spaces

    tryPack _ Nothing = Nothing
    tryPack (index, V2 w h) (Just (rs, r)) =
        case tryPackOne start (index, V2 (w + spacing) (h + spacing)) r of
            Just r' -> Just (rs `BV.snoc` r')
            Nothing -> Nothing

tryPackOne :: Int -> (Int, V2 Int) -> Packed -> Maybe Packed
tryPackOne start (index, V2 w h) (Packed layouts spaces) = do
    Rect _ _ rp <- List.find locatable spaces
    let layout = Layout (index + start) rp
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
