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
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Vector as BV (Vector, findIndex, fromList, imapM, mapM,
                                    mapMaybe, postscanl', reverse, zip, (!?))
import qualified Data.Vector.Storable as SV (generate, mapMaybe, unsafeWith)
import qualified Data.Vector.Storable.Mutable as MSV (unsafeWith)
import qualified Data.Vector.Unboxed as UV (Vector, generate, ifoldl', (!))
import qualified Foreign (Ptr, castPtr, copyArray, plusPtr, sizeOf)
import qualified GLW.Groups.PixelFormat as PixelFormat
import Graphics.Format.Tiled.Types
import qualified Graphics.GL as GL
import qualified Graphics.Hree.Geometry as Hree (addVerticesToGeometry,
                                                 newSpriteGeometry)
import qualified Graphics.Hree.GL.Types as Hree (Texture(..))
import qualified Graphics.Hree.GL.Vertex as Hree (SpriteVertex(..))
import qualified Graphics.Hree.Material as Hree (spriteMaterial)
import qualified Graphics.Hree.Scene as Hree (addMesh, addSampler, addTexture)
import qualified Graphics.Hree.Texture as Hree (TextureSettings(..),
                                                TextureSourceData(..))
import qualified Graphics.Hree.Types as Hree (Geometry, Material, Mesh(..),
                                              MeshId, Scene)
import Linear (V2(..), V3(..), (^-^))
import System.Directory (canonicalizePath)
import System.FilePath ((</>))

data Rect = Rect
    { rectBottomLeft :: !(V2 Float)
    , rectSize       :: !(V2 Float)
    } deriving (Show, Eq)

data TilesetInfo = TilesetInfo
    { tilesetInfoIndex       :: !Int
    , tilesetInfoTileset     :: !Tileset
    , tilesetInfoMaterial    :: !Hree.Material
    , tilesetInfoTextureSize :: !(V2 Int)
    , tilesetInfoGidRange    :: !(V2 Gid)
    } deriving (Show)

data TiledConfig = TiledConfig
    { tiledConfigUnitLength  :: !Int
    , tiledConfigStartZ      :: !Float
    , tiledConfigLayerDeltaZ :: !Float
    } deriving (Show, Eq)

defaultTiledConfig :: TiledConfig
defaultTiledConfig = TiledConfig 1024 0 0

{-
createNodesFromTiled :: Tiled -> Scene -> IO ()
createNodesFromTiled = undefined
-}

tilesetGidRanges :: BV.Vector Tileset -> BV.Vector (Gid, Gid)
tilesetGidRanges = BV.postscanl' go (0, 1)
    where
    go (_, a) tileset =
        let fgid = maybe a (max a) (tilesetFirstGid tileset)
            next = fgid + fromIntegral (tilesetTileCount tileset)
        in (fgid, next)

resolveTileset :: BV.Vector (Maybe TilesetInfo) -> BV.Vector Gid -> Gid -> Maybe TilesetInfo
resolveTileset tilesets firstGids gid = do
    index <- fmap (+ (-1)) . BV.findIndex (gid <) $ firstGids
    id =<< tilesets BV.!? index

renderOrderIndex :: RenderOrder -> Int -> Int -> Int -> Int
renderOrderIndex RenderOrderRightDown _ _ i = i
renderOrderIndex RenderOrderRightUp w h i =
    let (d, m) = divMod i w
    in w * (h - 1 - d) + m
renderOrderIndex RenderOrderLeftDown w h i =
    let (d, m) = divMod i w
    in w * d + w - 1 - m
renderOrderIndex RenderOrderLeftUp w h i =
    let (d, m) = divMod i w
    in w * (h - 1 - d) + w - 1 - m

tileOriginPixelCoord :: Orientation -> V2 Int -> V2 Int -> V2 Int
tileOriginPixelCoord = undefined

createMeshesFromTileLayer :: Hree.Scene -> TiledConfig -> Map -> BV.Vector (Maybe TilesetInfo) -> BV.Vector Gid -> TileLayer -> IO (BV.Vector Hree.MeshId)
createMeshesFromTileLayer scene config map tilesetInfos firstGids tileLayer = BV.mapM (uncurry $ createMeshFromTiles scene config map layerData origin) groups
    where
    go [] i gid = [(resolveTileset tilesetInfos firstGids gid, V2 i 1)]
    go ((Nothing, V2 i0 n) : xs) _ gid = (resolveTileset tilesetInfos firstGids gid, V2 i0 (n + 1)) : xs
    go a @ ((Just tileset, V2 i0 n) : xs) i gid =
        case resolveTileset tilesetInfos firstGids gid of
            Nothing -> (Just tileset, V2 i0 (n + 1)) : xs
            Just x | tilesetInfoIndex x == tilesetInfoIndex tileset -> (Just tileset, V2 i0 (n + 1)) : xs
                   | otherwise -> (Just x, V2 i 1) : a
    layerData = tileLayerData tileLayer
    width = mapWidth map
    height = mapHeight map
    orientation = mapOrientation map
    renderOrder = mapRenderOrder map
    ordered = UV.generate (width * height) ((layerData UV.!) . renderOrderIndex renderOrder width height)
    groups = BV.reverse . BV.mapMaybe tilesetExists . BV.fromList . UV.ifoldl' go [] $ ordered
    tilesetExists (Just material, a) = Just (material, a)
    tilesetExists (Nothing, _)       = Nothing
    origin = tileOriginPixelCoord orientation (V2 width height) (V2 (mapTileWidth map) (mapTileHeight map))

createMeshFromTiles :: Hree.Scene -> TiledConfig -> Map -> UV.Vector Gid -> V2 Int -> TilesetInfo -> V2 Int -> IO Hree.MeshId
createMeshFromTiles scene config map layerData origin tilesetInfo tiles = do
    geometry <- createGeometryFromTiles scene config map layerData origin tilesetInfo tiles
    let material = tilesetInfoMaterial tilesetInfo
        mesh = Hree.Mesh geometry material Nothing
    Hree.addMesh scene mesh

createGeometryFromTiles :: Hree.Scene -> TiledConfig -> Map -> UV.Vector Gid -> V2 Int -> TilesetInfo -> V2 Int -> IO Hree.Geometry
createGeometryFromTiles scene config map layerData origin tilesetInfo (V2 i0 n) = do
    let vertices = SV.mapMaybe mkVertex $ SV.generate n (+ i0)
    (geo, _) <- Hree.newSpriteGeometry scene
    Hree.addVerticesToGeometry geo vertices GL.GL_STATIC_READ scene
    where
    width = mapWidth map
    height = mapHeight map
    orientation = mapOrientation map
    renderOrder = mapRenderOrder map
    mapTileSize = V2 (mapTileWidth map) (mapTileHeight map)
    tileset = tilesetInfoTileset tilesetInfo
    tilesetTileSize = V2 (tilesetTileWidth tileset) (tilesetTileHeight tileset)
    staggerAxis = fromMaybe StaggerAxisX $ mapStaggerAxis map
    staggerIndex = fromMaybe StaggerIndexEven $ mapStaggerIndex map
    hexSide = mapHexSideLength map
    offset = V2 (coordX . tilesetTileOffset $ tileset) (coordY . tilesetTileOffset $ tileset)
    unit = tiledConfigUnitLength config
    mkVertex i = do
        let gid = layerData UV.! i
            (iy, ix) = divMod i width
        uvRect <- uvBoundingRect tilesetInfo gid
        let rect = tileBoundingRect orientation origin (V2 ix iy) mapTileSize tilesetTileSize offset unit staggerAxis staggerIndex hexSide
            vertex = tileBoundingSpriteVertex rect 0 uvRect
        return vertex

uvBoundingRect :: TilesetInfo -> Gid -> Maybe Rect
uvBoundingRect tilesetInfo gid =
    if gid < firstGid || nextGid <= gid
        then Nothing
        else Just $ go (fromIntegral $ gid - firstGid)
    where
    tileset = tilesetInfoTileset tilesetInfo
    V2 firstGid nextGid = tilesetInfoGidRange tilesetInfo
    V2 uvWidth uvHeight = tilesetInfoTextureSize tilesetInfo
    count = tilesetTileCount tileset
    columns = tilesetColumns tileset
    rows = count `div` columns
    margin = tilesetMargin tileset
    spacing = tilesetSpacing tileset
    tileWidth = tilesetTileWidth tileset
    tileHeight = tilesetTileHeight tileset
    tsw = columns * tileWidth + (columns - 1) * spacing + margin * 2
    tsh = rows * tileHeight + (rows - 1) * spacing + margin * 2
    go lid =
        let (iy, ix) = divMod lid columns
            px = margin + (max 0 (ix - 1)) * (tileWidth + spacing)
            py = tsh - (margin + iy * (tileHeight + spacing))
            x = fromIntegral px / fromIntegral uvWidth
            y = fromIntegral py / fromIntegral uvHeight
            w = fromIntegral tileWidth / fromIntegral uvWidth
            h = fromIntegral tileHeight / fromIntegral uvHeight
        in Rect (V2 x y) (V2 w h)

tileBoundingRect :: Orientation -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> Int -> StaggerAxis -> StaggerIndex -> Int -> Rect
tileBoundingRect orientation origin index mapTileSize tilesetTileSize offset unit staggerAxis staggerIndex hexSide =
    let upLeft = tileBoundingUpLeft orientation origin index mapTileSize offset unit staggerAxis staggerIndex hexSide
        V2 tileWidth tileHeight = tilesetTileSize
        w = fromIntegral tileWidth / fromIntegral unit
        h = fromIntegral tileHeight / fromIntegral unit
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

createTilesetInfos :: Hree.Scene -> FilePath -> BV.Vector Tileset ->  IO (BV.Vector (Maybe TilesetInfo))
createTilesetInfos scene cd tilesets =
    let gidRanges = tilesetGidRanges tilesets
    in BV.imapM (createTilesetInfo scene cd) (BV.zip tilesets gidRanges)

createTilesetInfo :: Hree.Scene -> FilePath -> Int -> (Tileset, (Gid, Gid)) -> IO (Maybe TilesetInfo)
createTilesetInfo scene cd index (tileset, (firstGid, nextGid)) =
    maybe (return Nothing) (fmap Just . go) (tilesetImage tileset)
    where
    go (Image sourcePath width height) = do
        path <- canonicalizePath $ cd </> Text.unpack sourcePath
        image <- either (throwIO . userError) (resizeImage width height . Picture.convertRGBA8) =<< Picture.readImage path
        let name = Text.encodeUtf8 sourcePath
            twidth = nextPow2 width
            theight = nextPow2 height
            settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral twidth) (fromIntegral theight) False
        (_, texture) <- SV.unsafeWith (Picture.imageData image) $ \ptr -> do
            let sourceData = Hree.TextureSourceData (fromIntegral width) (fromIntegral height) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr ptr)
            Hree.addTexture scene name settings sourceData
        let sname = Text.encodeUtf8 sourcePath
        (_, sampler) <- Hree.addSampler scene sname
        let material = Hree.spriteMaterial $ Hree.Texture (texture, sampler)
        return $ TilesetInfo index tileset material (V2 twidth theight) (V2 firstGid nextGid)
    nextPow2 = nextPow2_ 1
    nextPow2_ a x | a >= x = a
                  | otherwise = nextPow2_ (a * 2) x

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