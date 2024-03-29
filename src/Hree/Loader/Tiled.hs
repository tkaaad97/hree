module Hree.Loader.Tiled
    ( module Hree.Loader.Tiled.Types
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
import Control.Monad (when)
import Data.Bits (complement, (.&.))
import Data.Function ((&))
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as BV (Vector, concatMap, findIndex, freeze,
                                    fromList, generate, ifoldM', imapM,
                                    imapMaybe, length, map, mapM, mapMaybe,
                                    postscanl', reverse, zip, (!), (!?))
import Data.Vector.Algorithms.Merge (sortBy)
import qualified Data.Vector.Mutable as MBV (new, slice, write)
import qualified Data.Vector.Storable as SV (generate, replicate, singleton,
                                             unsafeWith, (//))
import qualified Data.Vector.Storable.Mutable as MSV (unsafeWith)
import qualified Data.Vector.Unboxed as UV (Vector, findIndex, findIndices,
                                            freeze, generate, ifoldl', length,
                                            map, mapM_, scanl, (!))
import qualified Data.Vector.Unboxed.Mutable as MUV (new, slice, write)
import Data.Word (Word32)
import qualified Foreign (Ptr, castPtr, copyArray, copyBytes,
                          mallocForeignPtrBytes, peekElemOff, plusPtr,
                          pokeElemOff, sizeOf, withForeignPtr)
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Hree (AnimationClip, Elem(..), Geometry, Interpolation(..),
                       KeyFrames(..), LimitedVector(..), MappingSource(..),
                       Material(..), MaterialId, Mesh(..), Node(..), NodeId,
                       SamplerParamValue(..), Scene, SpriteVertex(..),
                       TextureMappingType(..), TextureSettings(..),
                       TextureSourceData(..), VariationTrack(..), addMaterial,
                       addMesh, addNode, addVerticesToGeometry, mesh, node,
                       singleVariationClip, spriteGeometry,
                       updateMeshMaterialUniformBlock, updateMeshVertexBuffer)
import qualified Hree.GL.Sampler as Hree (glTextureMagFilter,
                                          glTextureMinFilter)
import Hree.Loader.Tiled.Types
import qualified Hree.Material.SpriteMaterial as Hree (SpriteMaterialBlock(..),
                                                       SpriteTile(..),
                                                       maxSpriteTileCount,
                                                       spriteMaterial)
import Linear (V2(..), V3(..), (^-^))
import Prelude hiding (map)
import System.Directory (canonicalizePath)
import System.FilePath ((</>))

data Rect = Rect
    { rectBottomLeft :: !(V2 Float)
    , rectSize       :: !(V2 Float)
    } deriving (Show, Eq)

data TilesetInfo = TilesetInfo
    { tilesetInfoIndex           :: !Int
    , tilesetInfoTileset         :: !Tileset
    , tilesetInfoMaterial        :: !(Maybe (Hree.MaterialId Hree.SpriteMaterialBlock))
    , tilesetInfoTextureSize     :: !(V2 Int)
    , tilesetInfoGidRange        :: !(V2 Gid)
    , tilesetInfoAnimationGids   :: !(UV.Vector Gid)
    , tilesetInfoAnimationFrames :: !(BV.Vector (BV.Vector Frame))
    , tilesetInfoImageGids       :: !(UV.Vector Gid)
    , tilesetInfoImageSources    :: !(BV.Vector Image)
    , tilesetInfoImageMaterials  :: !(BV.Vector (Hree.MaterialId Hree.SpriteMaterialBlock, V2 Int, V2 Int))
    }

data TiledConfig = TiledConfig
    { tiledConfigUnitLength  :: !Int
    , tiledConfigStartZ      :: !Float
    , tiledConfigLayerDeltaZ :: !Float
    , tiledConfigOriginPixel :: !(V2 Int)
    } deriving (Show, Eq)

data LoadInfo = LoadInfo
    { loadInfoMap        :: !Map
    , loadInfoNodeIds    :: !(BV.Vector Hree.NodeId)
    , loadInfoAnimations :: !(BV.Vector Hree.AnimationClip)
    , loadInfoLayers     :: !(BV.Vector LayerLoadInfo)
    } deriving (Show)

data LayerLoadInfo = LayerLoadInfo
    { layerLoadInfoNodeId  :: !Hree.NodeId
    , layerLoadInfoRegions :: !(BV.Vector RegionLoadInfo)
    } deriving (Show)

data RegionLoadInfo = RegionLoadInfo
    { regionLoadInfoNodeId     :: !Hree.NodeId
    , regionLoadInfoAnimations :: !(BV.Vector Hree.AnimationClip)
    } deriving (Show)

data TileMaterial = TileMaterial !TilesetInfo !(Hree.MaterialId Hree.SpriteMaterialBlock)

data ObjectMaterial =
    ObjectMaterialTileset !TilesetInfo !(Hree.MaterialId Hree.SpriteMaterialBlock, Hree.SpriteMaterialBlock) |
    ObjectMaterialTileImage !(Hree.MaterialId Hree.SpriteMaterialBlock, Hree.SpriteMaterialBlock) !(V2 Int) !(V2 Int)

instance Eq TileMaterial where
    (==) (TileMaterial tilesetInfo0 _)
         (TileMaterial tilesetInfo1 _) = tilesetInfoIndex tilesetInfo0 == tilesetInfoIndex tilesetInfo1

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
            next = fgid + fromIntegral (tilesetTileCount tileset) + 1
        in V2 fgid next

resolveTileset :: BV.Vector TilesetInfo -> BV.Vector (V2 Gid) -> Gid -> Maybe TileMaterial
resolveTileset tilesets gidRanges gid = do
    index <- BV.findIndex (\(V2 firstGid nextGid) -> firstGid <= gid && gid < nextGid) gidRanges
    tileset <- tilesets BV.!? index
    materialId <- tilesetInfoMaterial tileset
    return (TileMaterial tileset materialId)

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
    layers <- BV.mapMaybe id <$> BV.imapM (loadLayer scene cd config map tilesetInfos gidRanges) (mapLayers map)
    let nodeIds = BV.map layerLoadInfoNodeId layers
        animations = BV.concatMap (BV.concatMap regionLoadInfoAnimations . layerLoadInfoRegions) layers
    return (LoadInfo map nodeIds animations layers)
    where
    tilesets = mapTilesets map
    gidRanges = tilesetGidRanges tilesets

loadLayer :: Hree.Scene -> FilePath -> TiledConfig -> Map -> BV.Vector TilesetInfo -> BV.Vector (V2 Gid) -> Int -> Layer -> IO (Maybe LayerLoadInfo)
loadLayer scene _ config map tilesetInfos gidRanges layerIndex (LayerTileLayer layer) = do
    regions <- loadRegions scene config map tilesetInfos gidRanges layerIndex layer
    let regionNodeIds = BV.map regionLoadInfoNodeId regions
    nodeId <- Hree.addNode scene Hree.node { Hree.nodeChildren = regionNodeIds } Nothing False
    return . Just $ LayerLoadInfo nodeId regions
loadLayer scene _ config map tilesetInfos gidRanges layerIndex (LayerObjectGroup layer) = do
    regions <- loadObjectGroup scene config map tilesetInfos gidRanges layerIndex layer
    let objectNodeIds = BV.map regionLoadInfoNodeId regions
    nodeId <- Hree.addNode scene Hree.node { Hree.nodeChildren = objectNodeIds } Nothing False
    return . Just $ LayerLoadInfo nodeId regions
loadLayer scene cd config map _ _ layerIndex (LayerImageLayer (ImageLayer _ image offx offy)) = do
    (material, V2 iwidth iheight, V2 twidth theight) <- createMaterialFromImage scene cd image
    let mapH = fromIntegral $ mapHeight map * mapTileHeight map
        unit = tiledConfigUnitLength config
        x = realToFrac offx / fromIntegral unit
        y = realToFrac (mapH - fromIntegral iheight - offy) / fromIntegral unit
        z = tiledConfigStartZ config + tiledConfigLayerDeltaZ config * fromIntegral layerIndex
        width = fromIntegral iwidth / fromIntegral unit
        height = fromIntegral iheight / fromIntegral unit
        uv = V2 0 (fromIntegral iheight / fromIntegral theight)
        uvSize = V2 (fromIntegral iwidth / fromIntegral twidth) (- fromIntegral iheight / fromIntegral theight)
        vertex = Hree.SpriteVertex (V3 x y z) (V3 width height 0) (V3 0 0 0) 0 uv uvSize 0 0
    let geometry = Hree.addVerticesToGeometry Hree.spriteGeometry (SV.singleton vertex) GL.GL_STATIC_READ
        mesh = (Hree.mesh geometry material) { Hree.meshInstanceCount = Just 1 }
    meshId <- Hree.addMesh scene mesh
    nodeId <- Hree.addNode scene Hree.node (Just meshId) False
    return . Just $ LayerLoadInfo nodeId mempty

loadRegions :: Hree.Scene -> TiledConfig -> Map -> BV.Vector TilesetInfo -> BV.Vector (V2 Gid) -> Int -> TileLayer -> IO (BV.Vector RegionLoadInfo)
loadRegions scene config map tilesetInfos gidRanges layerIndex tileLayer = BV.mapM (uncurry $ loadRegionFromTiles scene config map layerData origin z) groups
    where
    go [] i gid = [(resolveTileset tilesetInfos gidRanges gid, V2 i 1)]
    go ((Nothing, V2 i0 n) : xs) _ gid = (resolveTileset tilesetInfos gidRanges gid, V2 i0 (n + 1)) : xs
    go a @ ((Just tileMaterial, V2 i0 n) : xs) i gid =
        case resolveTileset tilesetInfos gidRanges gid of
            Nothing -> (Just tileMaterial, V2 i0 (n + 1)) : xs
            Just x | x == tileMaterial -> (Just tileMaterial, V2 i0 (n + 1)) : xs
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

loadRegionFromTiles :: Hree.Scene -> TiledConfig -> Map -> UV.Vector Gid -> V2 Int -> Float -> TileMaterial -> V2 Int -> IO RegionLoadInfo
loadRegionFromTiles scene config map layerData origin z (TileMaterial tilesetInfo materialId) tiles @ (V2 _ n) = do
    (geometry, geometryGids) <- createGeometryFromTiles config map layerData origin z tilesetInfo useTileAnimation tiles
    let mesh = (Hree.mesh geometry materialId) { Hree.meshInstanceCount = Just n }
    meshId <- Hree.addMesh scene mesh
    nodeId <- Hree.addNode scene Hree.node (Just meshId) False
    let animations = BV.imapMaybe (createTileAnimation meshId geometryGids) (tilesetInfoAnimationFrames tilesetInfo)
    when useTileAnimation $ Hree.updateMeshMaterialUniformBlock scene meshId setSpriteTileVectorSize
    return (RegionLoadInfo nodeId animations)
    where
    animationGids = tilesetInfoAnimationGids tilesetInfo
    useTileAnimation = UV.length animationGids > 0 && UV.length animationGids <= Hree.maxSpriteTileCount
    V2 firstGid _ = tilesetInfoGidRange tilesetInfo

    setSpriteTileVectorSize block =
        let tile = Hree.SpriteTile GL.GL_FALSE GL.GL_FALSE (V2 0 0) (V2 0 0)
            v = Hree.LimitedVector (SV.replicate (UV.length animationGids) (Hree.Elem tile))
        in block { Hree.spriteTiles = v }

    updateBlockTile index (V2 uv usSize) block =
        let v = Hree.unLimitedVector . Hree.spriteTiles $ block
            spriteTile = Hree.SpriteTile GL.GL_FALSE GL.GL_FALSE uv usSize
        in block { Hree.spriteTiles = Hree.LimitedVector (v SV.// [(index, Hree.Elem spriteTile)]) }

    createTileAnimation meshId gids index frames
        | UV.length offsets > 0 && BV.length frames > 0 =
            let durations = UV.generate (BV.length frames) ((* 1000000) . fromIntegral . frameDuration . (frames BV.!))
                timepoints = UV.scanl (+) 0 durations
                tileGids = UV.generate (BV.length frames + 1) ((+ firstGid) . frameTileId . (frames BV.!) . max 0 . flip (-) 1)
                uvs = UV.map gidToUvRect tileGids
                track = Hree.VariationTrackDiscrete uvs
                keyFrames = Hree.KeyFrames Hree.InterpolationStep timepoints track
                setter = if useTileAnimation
                            then spriteTileSetter meshId index
                            else vboUvSetter meshId offsets
                animation = Hree.singleVariationClip setter keyFrames
            in Just animation
        | otherwise = Nothing
        where
        animationGid = animationGids UV.! index
        offsets = findOffsetsInRegion gids animationGid

    spriteTileSetter meshId index uvRect = Hree.updateMeshMaterialUniformBlock scene meshId (updateBlockTile index uvRect)

    writeVertexUv ptr (V2 uv uvSize) off = do
        vertex <- Foreign.peekElemOff ptr off
        Foreign.pokeElemOff ptr off vertex { Hree.svUv = uv, Hree.svUvSize = uvSize }

    vboUvSetter meshId offsets uvRect =
        Hree.updateMeshVertexBuffer scene meshId 2
            (\ptr -> UV.mapM_ (writeVertexUv (Foreign.castPtr ptr) uvRect) offsets)

    gidToUvRect gid =
        maybe (V2 (V2 0 0) (V2 0 0)) (\(Rect uv uvSize) -> V2 uv uvSize) $ uvBoundingRect tilesetInfo gid

findOffsetsInRegion :: UV.Vector Gid -> Gid -> UV.Vector Int
findOffsetsInRegion gids target = offsets
    where
    offsets = UV.findIndices f gids
    f gidWithFlags = unsetGidFlags gidWithFlags == target

createGeometryFromTiles :: TiledConfig -> Map -> UV.Vector Gid -> V2 Int -> Float -> TilesetInfo -> Bool -> V2 Int -> IO (Hree.Geometry, UV.Vector Gid)
createGeometryFromTiles config map layerData origin z tilesetInfo useTileAnimation (V2 i0 n) = do
    let orderedIndices = BV.generate n (renderOrderIndex orientation renderOrder staggerAxis staggerIndex (V2 columns rows) . (+ i0))
        vertexAndGids = BV.mapMaybe mkVertex orderedIndices
        vertices = SV.generate (BV.length vertexAndGids) (fst . (vertexAndGids BV.!))
        gids = UV.generate (BV.length vertexAndGids) (snd . (vertexAndGids BV.!))
        geometry = Hree.addVerticesToGeometry Hree.spriteGeometry vertices GL.GL_STATIC_READ
    return (geometry, gids)
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
    animationGids = tilesetInfoAnimationGids tilesetInfo
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
            maybeAnimationIndex = UV.findIndex (== gid) animationGids
            useSpriteTile = if useTileAnimation && isJust maybeAnimationIndex then 1 else 0
            spriteTileIndex = maybe 0 fromIntegral maybeAnimationIndex
            vertex = tileBoundingSpriteVertex rect z uvRect useSpriteTile spriteTileIndex orientation hflip vflip dflip rotated
        return (vertex, gid)

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
            x = fromIntegral px / fromIntegral uvWidth
            y = fromIntegral py / fromIntegral uvHeight
            w = fromIntegral tileWidth / fromIntegral uvWidth
            h = - fromIntegral tileHeight / fromIntegral uvHeight
        in Rect (V2 x y) (V2 w h)

tileBoundingRect :: Orientation -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> Int -> StaggerAxis -> StaggerIndex -> Int -> Rect
tileBoundingRect orientation origin mapSize mapTileSize tilesetTileSize offset index unit staggerAxis staggerIndex hexSide =
    let upLeft = tileBoundingUpLeft orientation origin mapSize mapTileSize offset index unit staggerAxis staggerIndex hexSide
        V2 tileWidth tileHeight = tilesetTileSize
        w = fromIntegral tileWidth / fromIntegral unit
        h = fromIntegral tileHeight / fromIntegral unit
        bottomLeft = upLeft ^-^ V2 0 h
    in Rect bottomLeft (V2 w h)

tileBoundingSpriteVertex :: Rect -> Float -> Rect -> GL.GLuint -> GL.GLuint -> Orientation -> Bool -> Bool -> Bool -> Bool -> Hree.SpriteVertex
tileBoundingSpriteVertex rect z (Rect uv uvSize) useSpriteTile spriteTileIndex orientation hflip vflip dflip rotated =
    let isHexagonal = orientation == OrientationHexagonal || orientation == OrientationStaggered
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
        vertex = Hree.SpriteVertex (V3 x y z) (V3 width height 0) center angle uv uvSize useSpriteTile spriteTileIndex
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

createTilesetInfos :: Hree.Scene -> FilePath -> BV.Vector Tileset ->  IO (BV.Vector TilesetInfo)
createTilesetInfos scene cd tilesets =
    let gidRanges = tilesetGidRanges tilesets
    in BV.imapM (createTilesetInfo scene cd) (BV.zip tilesets gidRanges)

loadObjectGroup :: Hree.Scene -> TiledConfig -> Map -> BV.Vector TilesetInfo -> BV.Vector (V2 Gid) -> Int -> ObjectGroup -> IO (BV.Vector RegionLoadInfo)
loadObjectGroup scene config map tilesetInfos gidRanges layerIndex (ObjectGroup layerCommon drawOrder objects) = do
    objectIndices <- MUV.new (BV.length objects)
    regionIndices <- MUV.new (BV.length objects)
    regions <- MBV.new (BV.length objects)
    len <- BV.ifoldM' (go objectIndices regionIndices regions) 0 objects
    let regionIndices' = MUV.slice 0 len regionIndices
    objectIndices' <- UV.freeze $ MUV.slice 0 len objectIndices
    regions' <- BV.freeze $ MBV.slice 0 len regions
    sortResult drawOrder len objectIndices' regionIndices' regions'
    where
    go objectIndices regionIndices regions i j object = do
        r <- loadObject scene config map tilesetInfos gidRanges layerIndex opacity object
        case r of
            Just v  -> do
                MUV.write objectIndices i j
                MUV.write regionIndices i i
                MBV.write regions i v
                return (i + 1)
            Nothing -> return i
    opacity = realToFrac $ layerCommonOpacity layerCommon

    sortResult DrawOrderTopDown len objectIndices regionIndices results = do
        sortBy (compareObjectPositionY objectIndices) regionIndices
        regionIndices' <- UV.freeze regionIndices
        return $ BV.generate len ((results BV.!) . (regionIndices' UV.!))

    sortResult DrawOrderIndex len objectIndices regionIndices results = do
        sortBy (compareObjectId objectIndices) regionIndices
        regionIndices' <- UV.freeze regionIndices
        return $ BV.generate len ((results BV.!) . (regionIndices' UV.!))

    compareObjectPositionY objectIndices i0 i1 =
        let y0 = objectCommonY . getObjectCommon $ objects BV.! (objectIndices UV.! i0)
            y1 = objectCommonY . getObjectCommon $ objects BV.! (objectIndices UV.! i1)
        in compare y0 y1

    compareObjectId objectIndices i0 i1 =
        let id0 = objectCommonId . getObjectCommon $ objects BV.! (objectIndices UV.! i0)
            id1 = objectCommonId . getObjectCommon $ objects BV.! (objectIndices UV.! i1)
        in compare id0 id1

loadObject :: Hree.Scene -> TiledConfig -> Map -> BV.Vector TilesetInfo -> BV.Vector (V2 Gid) -> Int -> Float -> Object -> IO (Maybe RegionLoadInfo)
loadObject scene config map tilesetInfos gidRanges layerIndex opacity (ObjectTile (TileObject object gidWithFlags))
    | objectCommonVisible object = go (resolveObjectMaterial opacity tilesetInfos gidRanges gid)
    | otherwise = return Nothing
    where
    gid = unsetGidFlags gidWithFlags
    hflip = flippedHorizontally gidWithFlags
    vflip = flippedVertically gidWithFlags
    mapH = fromIntegral $ mapHeight map * mapTileHeight map
    z = tiledConfigStartZ config + tiledConfigLayerDeltaZ config * fromIntegral layerIndex
    V2 x0 y0 = fromIntegral <$> tiledConfigOriginPixel config
    unit = tiledConfigUnitLength config
    ox = realToFrac $ (x0 + objectCommonX object) / fromIntegral unit
    oy = realToFrac $ (y0 + mapH - objectCommonY object) / fromIntegral unit
    ow = realToFrac $ objectCommonWidth object / fromIntegral unit
    oh = realToFrac $ objectCommonHeight object / fromIntegral unit
    rotation = realToFrac $ - objectCommonRotation object * pi / 180.0

    go (Just (ObjectMaterialTileset tilesetInfo material)) = do
        let Rect uv uvSize = fromMaybe (Rect (V2 0 0) (V2 0 0)) $ uvBoundingRect tilesetInfo gid
            Rect (V2 x y) (V2 w h) = Rect (V2 ox oy) (V2 ow oh)
                & applyWhen hflip flipRectHorizontally
                & applyWhen vflip flipRectVertically
            vertex = Hree.SpriteVertex (V3 x y z) (V3 w h 0) (V3 0 0 0) rotation uv uvSize 0 0
        region <- loadRegion vertex material
        return (Just region)

    go (Just (ObjectMaterialTileImage material (V2 iwidth iheight) (V2 twidth theight))) = do
        let uvw = fromIntegral iwidth / fromIntegral twidth
            uvh = fromIntegral iheight / fromIntegral theight
            uv = V2 0 uvh
            uvSize = V2 uvw (-uvh)
            Rect (V2 x y) (V2 w h) = Rect (V2 ox oy) (V2 ow oh)
                & applyWhen hflip flipRectHorizontally
                & applyWhen vflip flipRectVertically
            vertex = Hree.SpriteVertex (V3 x y z) (V3 w h 0) (V3 0 0 0) rotation uv uvSize 0 0
        region <- loadRegion vertex material
        return (Just region)

    go Nothing = return Nothing

    loadRegion vertex (materialId, block) = do
        let geometry = Hree.addVerticesToGeometry Hree.spriteGeometry (SV.singleton vertex) GL.GL_STATIC_READ
        meshId <- Hree.addMesh scene (Hree.mesh geometry materialId) { Hree.meshBlock = Just block, Hree.meshInstanceCount = Just 1 }
        nodeId <- Hree.addNode scene Hree.node (Just meshId) False
        return (RegionLoadInfo nodeId mempty)

loadObject _ _ _ _ _ _ _ _ = return Nothing

resolveObjectMaterial :: Float -> BV.Vector TilesetInfo -> BV.Vector (V2 Gid) -> Gid -> Maybe ObjectMaterial
resolveObjectMaterial opacityFactor tilesets gidRanges gid = do
    tilesetIndex <- BV.findIndex (\(V2 firstGid nextGid) -> firstGid <= gid && gid < nextGid) gidRanges
    tileset <- tilesets BV.!? tilesetIndex
    let imageGids = tilesetInfoImageGids tileset
        imageMaterials = tilesetInfoImageMaterials tileset
        block = (Hree.materialUniformBlock Hree.spriteMaterial) { Hree.opacityFactor =  opacityFactor }
    case (UV.findIndex (== gid) imageGids, tilesetInfoMaterial tileset) of
        (Just imageIndex, _) ->
            let (materialId, imageSize, textureSize) = imageMaterials BV.! imageIndex
            in Just $ ObjectMaterialTileImage (materialId, block) imageSize textureSize
        (Nothing, Just materialId) ->
            Just $ ObjectMaterialTileset tileset (materialId, block)
        _ -> Nothing

createTilesetInfo :: Hree.Scene -> FilePath -> Int -> (Tileset, V2 Gid) -> IO TilesetInfo
createTilesetInfo scene cd index (tileset, gidRange) = go (tilesetImage tileset)
    where
    go (Just image) = do
        (materialId, _, textureSize) <- createMaterialFromImage scene cd image
        imageMaterials <- BV.mapM (createMaterialFromImage scene cd) imageSources
        return $ TilesetInfo index tileset (Just materialId) textureSize gidRange animationGids animationFrames imageGids imageSources imageMaterials
    go Nothing =
        TilesetInfo index tileset Nothing (V2 0 0) gidRange animationGids animationFrames imageGids imageSources
            <$> BV.mapM (createMaterialFromImage scene cd) imageSources
    V2 firstGid _ = gidRange

    pickAnimationTile tile = (,) (tileId tile + firstGid) <$> tileAnimation tile
    animationTiles = BV.mapMaybe pickAnimationTile $ tilesetTiles tileset
    animationGids = UV.generate (BV.length animationTiles) (fst . (animationTiles BV.! ))
    animationFrames = BV.map snd animationTiles

    pickImageTile tile = (,) (tileId tile + firstGid) <$> tileImage tile
    imageTiles = BV.mapMaybe pickImageTile $ tilesetTiles tileset
    imageGids = UV.generate (BV.length imageTiles) (fst . (imageTiles BV.! ))
    imageSources = BV.map snd imageTiles

createMaterialFromImage :: Hree.Scene -> FilePath -> Image -> IO (Hree.MaterialId Hree.SpriteMaterialBlock, V2 Int, V2 Int)
createMaterialFromImage scene cd (Image sourcePath maybeWidth maybeHeight) = do
    path <- canonicalizePath $ cd </> Text.unpack sourcePath
    image <- either (throwIO . userError) (return . Picture.convertRGBA8) =<< Picture.readImage path
    let width = fromMaybe (Picture.imageWidth image) maybeWidth
        height = fromMaybe (Picture.imageHeight image) maybeHeight
        twidth = nextPow2 width
        theight = nextPow2 height
    resizedImage <- resizeImage width height image
    mapping <- mkMappingSource resizedImage (V2 width height) (V2 twidth theight)
    let material = Hree.spriteMaterial { Hree.materialMappings = pure (Hree.BaseColorMapping, mapping) }
    materialId <- Hree.addMaterial scene material
    return (materialId, V2 width height, V2 twidth theight)
    where
    nextPow2 = nextPow2_ 1
    nextPow2_ a x | a >= x = a
                  | otherwise = nextPow2_ (a * 2) x

    mkMappingSource :: Picture.Image Picture.PixelRGBA8 -> V2 Int -> V2 Int -> IO Hree.MappingSource
    mkMappingSource image (V2 width height) (V2 twidth theight) = do
        let byteSize = width * height * 4
            settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral twidth) (fromIntegral theight) False
        pixels <- Foreign.mallocForeignPtrBytes byteSize
        let sourceData = Hree.TextureSourceData (fromIntegral width) (fromIntegral height) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE pixels
            samplerParamValues =
                [ Hree.SamplerParamValue Hree.glTextureMinFilter GL.GL_NEAREST
                , Hree.SamplerParamValue Hree.glTextureMagFilter GL.GL_NEAREST
                ]
        Foreign.withForeignPtr pixels $ \dest ->
            SV.unsafeWith (Picture.imageData image) $ \p -> do
                Foreign.copyBytes (Foreign.castPtr dest) p byteSize
        return (Hree.MappingSource settings sourceData samplerParamValues)

resizeImage :: Int -> Int -> Picture.Image Picture.PixelRGBA8 -> IO (Picture.Image Picture.PixelRGBA8)
resizeImage width height source =
    if width == sourceWidth && height == sourceHeight
        then return source
        else do
            dest <- Picture.newMutableImage width height
            SV.unsafeWith (Picture.imageData source) $ \sp ->
                MSV.unsafeWith (Picture.mutableImageData dest) $ \dp -> mapM_ (writeRow sp dp) [0..(min height sourceHeight - 1)]
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

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f = f
applyWhen _ _    = id

getObjectCommon :: Object -> ObjectCommon
getObjectCommon (ObjectRectangle (Rectangle a)) = a
getObjectCommon (ObjectEllipse (Ellipse a))     = a
getObjectCommon (ObjectPoint (Point a))         = a
getObjectCommon (ObjectPolygon (Polygon a _))   = a
getObjectCommon (ObjectPolyline (Polyline a _)) = a
getObjectCommon (ObjectTile (TileObject a _))   = a
