module Graphics.Format.Tiled
    ( module Graphics.Format.Tiled.Types
    , LoadInfo(..)
    , Rect(..)
    , TiledConfig(..)
    , TilesetInfo(..)
    , loadTiledMap
    , loadTiledMapWithConfig
    , createTilesetInfo
    , createTilesetInfos
    , defaultTiledConfig
    , tileBoundingUpLeft
    , tileBoundingRect
    , tileBoundingSpriteVertex
    , uvBoundingRect
    ) where

import qualified Codec.Picture as Picture (Image(..), PixelRGBA8, convertRGBA8,
                                           readImage)
import qualified Codec.Picture.Types as Picture (MutableImage(..),
                                                 PixelBaseComponent,
                                                 freezeImage, newMutableImage)
import Control.Exception (throwIO)
import Data.Bits (complement, (.&.))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Vector as BV (Vector, findIndex, fromList, imapM, mapM,
                                    mapMaybe, postscanl', reverse, zip, (!?))
import qualified Data.Vector.Storable as SV (generate, mapMaybe, unsafeWith)
import qualified Data.Vector.Storable.Mutable as MSV (unsafeWith)
import qualified Data.Vector.Unboxed as UV (Vector, generate, ifoldl', (!))
import Data.Word (Word32)
import qualified Foreign (Ptr, castPtr, copyArray, plusPtr, sizeOf)
import qualified GLW.Groups.PixelFormat as PixelFormat
import Graphics.Format.Tiled.Types
import qualified Graphics.GL as GL
import qualified Graphics.Hree.Geometry as Hree (addVerticesToGeometry,
                                                 newSpriteGeometry)
import qualified Graphics.Hree.GL.Types as Hree (Texture(..))
import qualified Graphics.Hree.GL.Vertex as Hree (SpriteVertex(..))
import qualified Graphics.Hree.Material.SpriteMaterial as Hree (SpriteMaterial,
                                                                baseColorTexture,
                                                                spriteMaterial)
import qualified Graphics.Hree.Sampler as Hree (glTextureMagFilter,
                                                glTextureMinFilter,
                                                setSamplerParameter)
import qualified Graphics.Hree.Scene as Hree (addMesh, addNode, addSampler,
                                              addTexture, addedMeshId, newNode)
import qualified Graphics.Hree.Texture as Hree (TextureSettings(..),
                                                TextureSourceData(..))
import qualified Graphics.Hree.Types as Hree (Geometry, Mesh(..), MeshId,
                                              Node(..), NodeId, Scene)
import Linear (V2(..), V3(..), (^-^))
import Prelude hiding (map)
import System.Directory (canonicalizePath)
import System.FilePath ((</>))

data Rect = Rect
    { rectBottomLeft :: !(V2 Float)
    , rectSize       :: !(V2 Float)
    } deriving (Show, Eq)

data TilesetInfo = TilesetInfo
    { tilesetInfoIndex       :: !Int
    , tilesetInfoTileset     :: !Tileset
    , tilesetInfoMaterial    :: !Hree.SpriteMaterial
    , tilesetInfoTextureSize :: !(V2 Int)
    , tilesetInfoGidRange    :: !(V2 Gid)
    } deriving (Show)

data TiledConfig = TiledConfig
    { tiledConfigUnitLength  :: !Int
    , tiledConfigStartZ      :: !Float
    , tiledConfigLayerDeltaZ :: !Float
    , tiledConfigOriginPixel :: !(V2 Int)
    } deriving (Show, Eq)

data LoadInfo = LoadInfo
    { loadInfoMap          :: !Map
    , loadInfoLayerNodeIds :: !(BV.Vector Hree.NodeId)
    } deriving (Show)

defaultTiledConfig :: TiledConfig
defaultTiledConfig = TiledConfig 1024 0 0 (V2 0 0)

flippedHorizontallyFlag :: Word32
flippedHorizontallyFlag = 0x80000000

flippedVerticallyFlag :: Word32
flippedVerticallyFlag = 0x40000000

flippedAntiDiagonallyFlag :: Word32
flippedAntiDiagonallyFlag = 0x20000000

rotatedHexagonal120Flag :: Word32
rotatedHexagonal120Flag = 0x10000000

flippedHorizontally :: Word32 -> Bool
flippedHorizontally gid = flippedHorizontallyFlag .&. gid /= 0

flippedVertically :: Word32 -> Bool
flippedVertically gid = flippedVerticallyFlag .&. gid /= 0

flippedAntiDiagonally :: Word32 -> Bool
flippedAntiDiagonally gid = flippedAntiDiagonallyFlag .&. gid /= 0

rotatedHexagonal120 :: Word32 -> Bool
rotatedHexagonal120 gid = rotatedHexagonal120Flag .&. gid /= 0

unsetGidFlags :: Word32 -> Word32
unsetGidFlags gid = gid .&. complement flippedHorizontallyFlag .&. complement flippedVerticallyFlag .&. complement flippedAntiDiagonallyFlag .&. complement rotatedHexagonal120Flag

tilesetGidRanges :: BV.Vector Tileset -> BV.Vector (V2 Gid)
tilesetGidRanges = BV.postscanl' go (V2 0 1)
    where
    go (V2 _ a) tileset =
        let fgid = maybe a (max a) (tilesetFirstGid tileset)
            next = fgid + fromIntegral (tilesetTileCount tileset)
        in V2 fgid next

resolveTileset :: BV.Vector (Maybe TilesetInfo) -> BV.Vector (V2 Gid) -> Gid -> Maybe TilesetInfo
resolveTileset tilesets gidRanges gid = do
    index <- BV.findIndex (\(V2 firstGid nextGid) -> firstGid <= gid && gid < nextGid) $ gidRanges
    id =<< tilesets BV.!? index

renderOrderIndex :: Orientation -> RenderOrder -> StaggerAxis -> StaggerIndex -> V2 Int -> Int -> Int
renderOrderIndex OrientationOrthogonal renderOrder _ _ size index = nostaggeredRenderOrderIndex renderOrder size index
renderOrderIndex OrientationIsometric renderOrder _ _ size index = nostaggeredRenderOrderIndex renderOrder size index
renderOrderIndex _ renderOrder staggerAxis staggerIndex size index = staggeredRenderOrderIndex staggerAxis staggerIndex renderOrder size index

staggeredRenderOrderIndex :: StaggerAxis -> StaggerIndex -> RenderOrder -> V2 Int -> Int -> Int
staggeredRenderOrderIndex StaggerAxisY _ renderOrder size index = nostaggeredRenderOrderIndex renderOrder size index
staggeredRenderOrderIndex StaggerAxisX staggerIndex RenderOrderRightDown (V2 w _) i =
    let (d, m) = divMod i w
        half = case staggerIndex of
                StaggerIndexEven -> w `div` 2
                StaggerIndexOdd  -> (w + 1) `div` 2
        m' = case staggerIndex of
                StaggerIndexEven -> if m < half
                    then 2 * m + 1
                    else 2 * (m - half)
                StaggerIndexOdd -> if m < half
                    then 2 * m
                    else 2 * (m - half) + 1
    in w * d + m'
staggeredRenderOrderIndex StaggerAxisX staggerIndex RenderOrderRightUp (V2 w h) i =
    let (d, m) = divMod i w
        half = case staggerIndex of
                StaggerIndexEven -> (w + 1) `div` 2
                StaggerIndexOdd  -> w `div` 2
        m' = case staggerIndex of
                StaggerIndexEven -> if m < half
                    then 2 * m
                    else 2 * (m - half) + 1
                StaggerIndexOdd -> if m < half
                    then 2 * m + 1
                    else 2 * (m - half)
    in w * (h - 1 - d) + m'
staggeredRenderOrderIndex StaggerAxisX staggerIndex RenderOrderLeftDown (V2 w _) i =
    let (d, m) = divMod i w
        half = case staggerIndex of
                StaggerIndexEven -> w `div` 2
                StaggerIndexOdd  -> (w + 1) `div` 2
        m' = case staggerIndex of
                StaggerIndexEven -> if m < half
                    then w - 1 - 2 * m - (w `mod` 2)
                    else w - 1 - 2 * (m - half) - (w `mod` 2) - 1
                StaggerIndexOdd -> if m < half
                    then w - 1 - 2 * m - (w `mod` 2) - 1
                    else w - 1 - 2 * (m - half) - (w `mod` 2)
    in w * d + m'
staggeredRenderOrderIndex StaggerAxisX staggerIndex RenderOrderLeftUp (V2 w h) i =
    let (d, m) = divMod i w
        half = case staggerIndex of
                StaggerIndexEven -> (w + 1) `div` 2
                StaggerIndexOdd  -> w `div` 2
        m' = case staggerIndex of
                StaggerIndexEven -> if m < half
                    then w - 1 - 2 * m - (w `mod` 2) - 1
                    else w - 1 - 2 * (m - half) - (w `mod` 2)
                StaggerIndexOdd -> if m < half
                    then w - 1 - 2 * m - (w `mod` 2)
                    else w - 1 - 2 * (m - half) - (w `mod` 2) - 1
    in w * (h - 1 - d) + m'

nostaggeredRenderOrderIndex :: RenderOrder -> V2 Int -> Int -> Int
nostaggeredRenderOrderIndex RenderOrderRightDown (V2 _ _) i = i
nostaggeredRenderOrderIndex RenderOrderRightUp (V2 w h) i =
    let (d, m) = divMod i w
    in w * (h - 1 - d) + m
nostaggeredRenderOrderIndex RenderOrderLeftDown (V2 w _) i =
    let (d, m) = divMod i w
    in w * d + w - 1 - m
nostaggeredRenderOrderIndex RenderOrderLeftUp (V2 w h) i =
    let (d, m) = divMod i w
    in w * (h - 1 - d) + w - 1 - m

loadTiledMap :: Hree.Scene -> FilePath -> Map -> IO LoadInfo
loadTiledMap scene cd = loadTiledMapWithConfig scene cd defaultTiledConfig

loadTiledMapWithConfig :: Hree.Scene -> FilePath -> TiledConfig -> Map -> IO LoadInfo
loadTiledMapWithConfig scene cd config map = do
    tilesetInfos <- createTilesetInfos scene cd tilesets
    nodeIds <- BV.mapMaybe id <$> BV.imapM (createNodeFromLayer scene config map tilesetInfos gidRanges) layers
    return (LoadInfo map nodeIds)
    where
    tilesets = mapTilesets map
    gidRanges = tilesetGidRanges tilesets
    layers = mapLayers map

createNodeFromLayer :: Hree.Scene -> TiledConfig -> Map -> BV.Vector (Maybe TilesetInfo) -> BV.Vector (V2 Gid) -> Int -> Layer -> IO (Maybe Hree.NodeId)
createNodeFromLayer scene config map tilesetInfos gidRanges layerIndex (LayerTileLayer layer) = do
    meshIds <- createMeshesFromTileLayer scene config map tilesetInfos gidRanges layerIndex layer
    nodeIds <- BV.mapM mkNode meshIds
    Just <$> Hree.addNode scene Hree.newNode { Hree.nodeChildren = nodeIds } False
    where
    mkNode meshId = Hree.addNode scene Hree.newNode { Hree.nodeMesh = Just meshId } False
createNodeFromLayer _ _ _ _ _ _ _ = return Nothing

createMeshesFromTileLayer :: Hree.Scene -> TiledConfig -> Map -> BV.Vector (Maybe TilesetInfo) -> BV.Vector (V2 Gid) -> Int -> TileLayer -> IO (BV.Vector Hree.MeshId)
createMeshesFromTileLayer scene config map tilesetInfos gidRanges layerIndex tileLayer = BV.mapM (uncurry $ createMeshFromTiles scene config map layerData origin z) groups
    where
    go [] i gid = [(resolveTileset tilesetInfos gidRanges gid, V2 i 1)]
    go ((Nothing, V2 i0 n) : xs) _ gid = (resolveTileset tilesetInfos gidRanges gid, V2 i0 (n + 1)) : xs
    go a @ ((Just tileset, V2 i0 n) : xs) i gid =
        case resolveTileset tilesetInfos gidRanges gid of
            Nothing -> (Just tileset, V2 i0 (n + 1)) : xs
            Just x | tilesetInfoIndex x == tilesetInfoIndex tileset -> (Just tileset, V2 i0 (n + 1)) : xs
                   | otherwise -> (Just x, V2 i 1) : a
    layerData = tileLayerData tileLayer
    orientation = mapOrientation map
    columns = mapWidth map
    rows = mapHeight map
    renderOrder = mapRenderOrder map
    staggerAxis = fromMaybe StaggerAxisX $ mapStaggerAxis map
    staggerIndex = fromMaybe StaggerIndexEven $ mapStaggerIndex map
    ordered = UV.generate (columns * rows) ((layerData UV.!) . renderOrderIndex orientation renderOrder staggerAxis staggerIndex (V2 columns rows))
    groups = BV.reverse . BV.mapMaybe tilesetExists . BV.fromList . UV.ifoldl' go [] $ ordered
    tilesetExists (Just material, a) = Just (material, a)
    tilesetExists (Nothing, _)       = Nothing
    origin = tiledConfigOriginPixel config
    z = tiledConfigStartZ config + tiledConfigLayerDeltaZ config * fromIntegral layerIndex

createMeshFromTiles :: Hree.Scene -> TiledConfig -> Map -> UV.Vector Gid -> V2 Int -> Float -> TilesetInfo -> V2 Int -> IO Hree.MeshId
createMeshFromTiles scene config map layerData origin z tilesetInfo tiles @ (V2 _ n) = do
    geometry <- createGeometryFromTiles scene config map layerData origin z tilesetInfo tiles
    let material = tilesetInfoMaterial tilesetInfo
        mesh = Hree.Mesh geometry material (Just n)
    Hree.addedMeshId <$> Hree.addMesh scene mesh

createGeometryFromTiles :: Hree.Scene -> TiledConfig -> Map -> UV.Vector Gid -> V2 Int -> Float -> TilesetInfo -> V2 Int -> IO Hree.Geometry
createGeometryFromTiles scene config map layerData origin z tilesetInfo (V2 i0 n) = do
    let vertices = SV.mapMaybe mkVertex $ SV.generate n ((renderOrderIndex orientation renderOrder staggerAxis staggerIndex (V2 columns rows)) . (+ i0))
    (geo, _) <- Hree.newSpriteGeometry scene
    Hree.addVerticesToGeometry geo vertices GL.GL_STATIC_READ scene
    where
    columns = mapWidth map
    rows = mapHeight map
    mapSize = V2 columns rows
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
        let gidWithFlags = layerData UV.! i
            gid = unsetGidFlags gidWithFlags
            hflip = flippedHorizontally gidWithFlags
            vflip = flippedVertically gidWithFlags
            dflip = flippedAntiDiagonally gidWithFlags
            rotated = rotatedHexagonal120 gidWithFlags
            (iy, ix) = divMod i columns
        uvRect <- uvBoundingRect tilesetInfo gid
        let rect = tileBoundingRect orientation origin mapSize mapTileSize tilesetTileSize offset (V2 ix iy) unit staggerAxis staggerIndex hexSide
            vertex = tileBoundingSpriteVertex rect z uvRect orientation hflip vflip dflip rotated
        return vertex

flipRectHorizontally :: Rect -> Rect
flipRectHorizontally (Rect (V2 x y) (V2 w h)) = Rect (V2 (x + w) y) (V2 (-w) h)

flipRectVertically :: Rect -> Rect
flipRectVertically (Rect (V2 x y) (V2 w h)) = Rect (V2 x (y + h)) (V2 w (-h))

uvBoundingRect :: TilesetInfo -> Gid -> Maybe Rect
uvBoundingRect tilesetInfo gid =
    if gid < firstGid || nextGid <= gid
        then Nothing
        else Just $ go (fromIntegral $ gid - firstGid)
    where
    tileset = tilesetInfoTileset tilesetInfo
    V2 firstGid nextGid = tilesetInfoGidRange tilesetInfo
    V2 uvWidth uvHeight = tilesetInfoTextureSize tilesetInfo
    columns = tilesetColumns tileset
    margin = tilesetMargin tileset
    spacing = tilesetSpacing tileset
    tileWidth = tilesetTileWidth tileset
    tileHeight = tilesetTileHeight tileset
    go lid =
        let (iy, ix) = divMod lid columns
            px = margin + ix * (tileWidth + spacing)
            py = margin + iy * (tileHeight + spacing) + tileHeight
            x = (0.5 + fromIntegral px) / fromIntegral uvWidth
            y = (- 0.5 + fromIntegral py) / fromIntegral uvHeight
            w = (fromIntegral tileWidth - 1.0) / fromIntegral uvWidth
            h = - (fromIntegral tileHeight - 1.0) / fromIntegral uvHeight
        in Rect (V2 x y) (V2 w h)

tileBoundingRect :: Orientation -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> Int -> StaggerAxis -> StaggerIndex -> Int -> Rect
tileBoundingRect orientation origin mapSize mapTileSize tilesetTileSize offset index unit staggerAxis staggerIndex hexSide =
    let upLeft = tileBoundingUpLeft orientation origin mapSize mapTileSize offset index unit staggerAxis staggerIndex hexSide
        V2 tileWidth tileHeight = tilesetTileSize
        w = fromIntegral tileWidth / fromIntegral unit
        h = fromIntegral tileHeight / fromIntegral unit
        bottomLeft = upLeft ^-^ V2 0 h
    in Rect bottomLeft (V2 w h)

tileBoundingSpriteVertex :: Rect -> Float -> Rect -> Orientation -> Bool -> Bool -> Bool -> Bool -> Hree.SpriteVertex
tileBoundingSpriteVertex rect z (Rect uv uvSize) orientation hflip vflip dflip rotated =
    let isHexagonal = orientation == OrientationHexagonal || orientation == OrientationStaggered
        applyWhen True f = f
        applyWhen _ _    = id
        Rect (V2 x y) (V2 width height) =
            if isHexagonal
                then rect
                    & applyWhen hflip flipRectHorizontally
                    & applyWhen vflip flipRectVertically
                else rect
                    & applyWhen ((not dflip && hflip) || (dflip && not vflip)) flipRectHorizontally
                    & applyWhen ((not dflip && vflip) || (dflip && hflip)) flipRectVertically
        angle
            | isHexagonal = (if rotated then 1 else 0) * (- pi * 2 / 3) + (if dflip then 1 else 0) * (- pi / 3)
            | otherwise = if dflip then pi / 2 else 0
        center = V3 (0.5 * width) (0.5 * height) 0
        vertex = Hree.SpriteVertex (V3 x y z) (V3 width height 0) center angle uv uvSize
    in vertex

tileBoundingUpLeft :: Orientation -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> Int -> StaggerAxis -> StaggerIndex -> Int -> V2 Float
tileBoundingUpLeft OrientationOrthogonal (V2 ox oy) (V2 _ rows) (V2 tileWidth tileHeight) (V2 offsetX offsetY) (V2 ix iy) unit _ _ _ =
    let ox' = ox + 0
        oy' = oy + rows * tileHeight
        x = fromIntegral (ox' + ix * tileWidth + offsetX) / fromIntegral unit
        y = fromIntegral (oy' - iy * tileHeight - offsetY) / fromIntegral unit
    in V2 x y
tileBoundingUpLeft OrientationIsometric (V2 ox oy) (V2 columns rows) (V2 tileWidth tileHeight) (V2 offsetX offsetY) (V2 ix iy) unit _ _ _ =
    let halfw = (tileWidth + 1) `div` 2
        halfh = (tileHeight + 1) `div` 2
        ox' = ox + (rows - 1) * halfw
        oy' = oy + (columns + rows) * halfh
        x = fromIntegral (ox' + (ix - iy) * halfw + offsetX) / fromIntegral unit
        y = fromIntegral (oy' - (ix + iy) * halfh - offsetY) / fromIntegral unit
    in V2 x y
tileBoundingUpLeft OrientationStaggered (V2 ox oy) mapSize mapTileSize (V2 offsetX offsetY) ixy unit staggerAxis staggerIndex _ =
    let V2 sx sy = stagger staggerAxis staggerIndex 0 mapSize mapTileSize ixy
        x = fromIntegral (ox + sx + offsetX) / fromIntegral unit
        y = fromIntegral (oy + sy - offsetY) / fromIntegral unit
    in V2 x y
tileBoundingUpLeft OrientationHexagonal (V2 ox oy) mapSize mapTileSize (V2 offsetX offsetY) ixy unit staggerAxis staggerIndex hexSide =
    let V2 sx sy = stagger staggerAxis staggerIndex hexSide mapSize mapTileSize ixy
        x = fromIntegral (ox + sx + offsetX) / fromIntegral unit
        y = fromIntegral (oy + sy - offsetY) / fromIntegral unit
    in V2 x y

stagger :: StaggerAxis -> StaggerIndex -> Int -> V2 Int -> V2 Int -> V2 Int -> V2 Int
stagger StaggerAxisX staggerIndex hexSide (V2 _ rows) (V2 tileWidth tileHeight) (V2 ix iy)  =
    let halfw = tileWidth - (tileWidth - hexSide) `div` 2
        halfh = (tileHeight + 1) `div` 2
        ox = 0
        oy = rows * tileHeight + halfh
        x = ox + ix * halfw
        y = case (staggerIndex, ix `mod` 2 == 0) of
                (StaggerIndexEven, True)  -> oy - (iy * tileHeight + halfh)
                (StaggerIndexEven, False) -> oy - iy * tileHeight
                (StaggerIndexOdd, False)  -> oy - (iy * tileHeight + halfh)
                (StaggerIndexOdd, True)   -> oy - iy * tileHeight
    in V2 x y
stagger StaggerAxisY staggerIndex hexSide (V2 _ rows) (V2 tileWidth tileHeight) (V2 ix iy) =
    let halfw = (tileWidth + 1) `div` 2
        halfh = (tileHeight - (tileHeight - hexSide) `div` 2)
        ox = 0
        oy = (rows + 1) * halfh
        x = case (staggerIndex, iy `mod` 2 == 0) of
                (StaggerIndexEven, True)  -> ox + ix * tileWidth + halfw
                (StaggerIndexEven, False) -> ox + ix * tileWidth
                (StaggerIndexOdd, False)  -> ox + ix * tileWidth + halfw
                (StaggerIndexOdd, True)   -> ox + ix * tileWidth
        y = oy - iy * halfh
    in V2 x y

createTilesetInfos :: Hree.Scene -> FilePath -> BV.Vector Tileset ->  IO (BV.Vector (Maybe TilesetInfo))
createTilesetInfos scene cd tilesets =
    let gidRanges = tilesetGidRanges tilesets
    in BV.imapM (createTilesetInfo scene cd) (BV.zip tilesets gidRanges)

createTilesetInfo :: Hree.Scene -> FilePath -> Int -> (Tileset, V2 Gid) -> IO (Maybe TilesetInfo)
createTilesetInfo scene cd index (tileset, gidRange) =
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
        Hree.setSamplerParameter sampler Hree.glTextureMinFilter GL.GL_NEAREST
        Hree.setSamplerParameter sampler Hree.glTextureMagFilter GL.GL_NEAREST
        let material = Hree.spriteMaterial { Hree.baseColorTexture = Just $ Hree.Texture (texture, sampler) }
        return $ TilesetInfo index tileset material (V2 twidth theight) gidRange
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
