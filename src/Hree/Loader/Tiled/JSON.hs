module Hree.Loader.Tiled.JSON
    ( module Hree.Loader.Tiled.Types
    , Tiled.LoadInfo(..)
    , Tiled.TiledConfig(..)
    , Tiled.defaultTiledConfig
    , readTiledMap
    , loadTiledMap
    , loadTiledMapWithConfig
    ) where

import qualified Codec.Compression.GZip as GZip (decompress)
import qualified Codec.Compression.Zlib as Zlib (decompress)
import Control.Exception (throwIO)
import qualified Data.Aeson as DA (eitherDecodeStrict')
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (length, readFile, useAsCString)
import qualified Data.ByteString.Base64 as Base64 (decode)
import qualified Data.ByteString.Lazy as ByteString (fromStrict, toStrict)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Vector as BV (mapM)
import qualified Data.Vector.Unboxed as UV (Vector, generateM)
import Data.Word (Word32)
import qualified Foreign (castPtr, peekByteOff)
import qualified Hree
import qualified Hree.Loader.Tiled as Tiled (LoadInfo(..), TiledConfig(..),
                                             defaultTiledConfig,
                                             loadTiledMapWithConfig)
import Hree.Loader.Tiled.Types
import System.Directory (canonicalizePath)
import System.FilePath (dropFileName, (</>))

readTiledMap :: FilePath -> IO Map
readTiledMap path = do
    mid <- either (throwIO . userError) return . DA.eitherDecodeStrict' =<< ByteString.readFile path
    let basepath = dropFileName path
    completeMap basepath mid

loadTiledMap :: Hree.Scene -> FilePath -> IO Tiled.LoadInfo
loadTiledMap scene path =
    loadTiledMapWithConfig scene path Tiled.defaultTiledConfig

loadTiledMapWithConfig :: Hree.Scene -> FilePath -> Tiled.TiledConfig -> IO Tiled.LoadInfo
loadTiledMapWithConfig scene path config = do
    m <- readTiledMap path
    Tiled.loadTiledMapWithConfig scene (dropFileName path) config m

completeMap :: FilePath -> MapMid -> IO Map
completeMap basepath m = do
    let layerMids = mapMidLayers m
        tilesetMids = mapMidTilesets m
    layers <- BV.mapM completeLayer layerMids
    tilesets <- BV.mapM (completeTileset basepath) tilesetMids
    return Map
        { mapVersion = mapMidVersion m
        , mapTiledVersion = mapMidTiledVersion m
        , mapOrientation = mapMidOrientation m
        , mapRenderOrder = mapMidRenderOrder m
        , mapWidth = mapMidWidth m
        , mapHeight = mapMidHeight m
        , mapTileWidth = mapMidTileWidth m
        , mapTileHeight = mapMidTileHeight m
        , mapHexSideLength = mapMidHexSideLength m
        , mapStaggerAxis = mapMidStaggerAxis m
        , mapStaggerIndex = mapMidStaggerIndex m
        , mapBackgroundColor = mapMidBackgroundColor m
        , mapNextLayerId = mapMidNextLayerId m
        , mapNextObjectId = mapMidNextObjectId m
        , mapLayers = layers
        , mapTilesets = tilesets
        , mapProperties = mapMidProperties m
        }

completeTileset :: FilePath -> TilesetSource -> IO Tileset
completeTileset basepath (TilesetSourceFile firstgid sourcePath) = do
    path <- canonicalizePath $ basepath </> Text.unpack sourcePath
    tileset <- either (throwIO . userError) return . DA.eitherDecodeStrict' =<< ByteString.readFile path
    return tileset { tilesetFirstGid = firstgid }
completeTileset _ (TilesetSourceInplace tileset) = return tileset

completeLayer :: LayerMid -> IO Layer
completeLayer (LayerMidTileLayer a)   = LayerTileLayer <$> completeTileLayer a
completeLayer (LayerMidObjectGroup a) = return (LayerObjectGroup a)
completeLayer (LayerMidImageLayer a)  = return (LayerImageLayer a)

completeTileLayer :: TileLayerMid -> IO TileLayer
completeTileLayer (TileLayerMid common w h (TileLayerDataCsv d)) = return (TileLayer common w h d CsvEncoding NoCompression)
completeTileLayer (TileLayerMid common w h (TileLayerDataBase64 c d)) = do
    data_ <- bytestringToVector =<< decompress c d
    return (TileLayer common w h data_ Base64Encoding c)

decompress :: CompressionType -> Text -> IO ByteString
decompress NoCompression =
    either (throwIO . userError) return . Base64.decode . Text.encodeUtf8
decompress GZipCompression =
    fmap (ByteString.toStrict . GZip.decompress . ByteString.fromStrict) . either (throwIO . userError) return . Base64.decode . Text.encodeUtf8
decompress ZlibCompression =
    fmap (ByteString.toStrict . Zlib.decompress . ByteString.fromStrict) . either (throwIO . userError) return . Base64.decode . Text.encodeUtf8

bytestringToVector :: ByteString -> IO (UV.Vector Word32)
bytestringToVector a =
    ByteString.useAsCString a $ \ptr ->
        UV.generateM count (\i -> Foreign.peekByteOff (Foreign.castPtr ptr) (stride * i))
    where
    stride = 4
    count = ByteString.length a `div` stride
