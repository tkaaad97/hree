{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Graphics.Format.Tiled
    ( Color
    , Coord(..)
    , Ellipse(..)
    , Gid
    , ImageLayer(..)
    , Layer(..)
    , LayerCommon(..)
    , Map(..)
    , Object(..)
    , ObjectCommon(..)
    , ObjectGroup(..)
    , Orientation(..)
    , Point(..)
    , Polygon(..)
    , Polyline(..)
    , Properties
    , Rectangle(..)
    , RenderOrder(..)
    , Terrain(..)
    , Tile(..)
    , TileLayer(..)
    , TileLayerData(..)
    , Tileset(..)
    ) where

import Control.Monad (mzero)
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as DA (FromJSON, ToJSON, Value(..), object,
                                   parseJSON, toJSON, withText)
import qualified Data.Aeson.TH as DA (Options(..), defaultOptions, deriveJSON)
import qualified Data.Aeson.Types as DA (Object, Parser, typeMismatch)
import qualified Data.HashMap.Lazy as HML (lookup, toList)
import qualified Data.Map as DM (Map, empty)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV (Vector)
import Data.Word (Word32)
import Graphics.Format.Tiled.Internal

type Gid = Int

type Properties = DM.Map Text Text

withDefault :: (DA.FromJSON b) => DA.Object -> Text -> b -> DA.Parser b
withDefault a label b =
    a .:? label >>= return . fromMaybe b

data Coord = Coord
    { coordX :: Int
    , coordY :: Int
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = constructorTagModifier 5 }) ''Coord)

type Color = Text

data Orientation
    = OrientationOrthogonal
    | OrientationIsometric
    | OrientationStaggered
    deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.constructorTagModifier = constructorTagModifier 11 }) ''Orientation)

data RenderOrder
    = RenderOrderRightDown
    | RenderOrderRightUp
    | RenderOrderLeftDown
    | RenderOrderLeftUp
    deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.constructorTagModifier = renderOrderTagModifier }) ''RenderOrder)

data ObjectCommon = ObjectCommon
    { objectCommonId         :: !Int
    , objectCommonType       :: !Text
    , objectCommonWidth      :: !Int
    , objectCommonHeight     :: !Int
    , objectCommonName       :: !Text
    , objectCommonProperties :: !Properties
    , objectCommonVisible    :: !Bool
    , objectCommonX          :: !Int
    , objectCommonY          :: !Int
    , objectCommonRotation   :: !Double
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = constructorTagModifier 12 }) ''ObjectCommon)

data Object
    = ObjectRectangle !Rectangle
    | ObjectEllipse !Ellipse
    | ObjectPoint !Point
    | ObjectPolygon !Polygon
    | ObjectPolyline !Polyline
    deriving (Show, Eq)

newtype Rectangle = Rectangle ObjectCommon
    deriving (Show, Eq)

newtype Ellipse = Ellipse ObjectCommon
    deriving (Show, Eq)

newtype Point = Point ObjectCommon
    deriving (Show, Eq)

data Polygon = Polygon
    { polygonCommon :: !ObjectCommon
    , polygonCoords :: !(Vector Coord)
    } deriving (Show, Eq)

data Polyline = Polyline
    { polylineCommon :: !ObjectCommon
    , polylineCoords :: !(Vector Coord)
    } deriving (Show, Eq)

instance DA.FromJSON Object where
    parseJSON (DA.Object v) = do
        common <- parseObjectCommon
        isEllipse <- withDefault v "ellipse" False
        isPoint <- withDefault v "point" False
        polygon <- v .:? "polygon"
        polyline <- v .:? "polyline"

        return $ parseObject common isEllipse isPoint polygon polyline

        where

        parseObjectCommon = ObjectCommon
            <$> v .: "id"
            <*> v .: "type"
            <*> v .: "width"
            <*> v .: "height"
            <*> v .: "name"
            <*> withDefault v "properties" DM.empty
            <*> v .: "visible"
            <*> v .: "x"
            <*> v .: "y"
            <*> v .: "rotation"

        parseObject c True _ _ _                      = ObjectEllipse $ Ellipse c

        parseObject c False True _ _                  = ObjectPoint $ Point c

        parseObject c False False (Just poly) _       = ObjectPolygon $ Polygon c poly

        parseObject c False False Nothing (Just poly) = ObjectPolyline $ Polyline c poly

        parseObject c False False Nothing Nothing     = ObjectRectangle $ Rectangle c

    parseJSON invalid = DA.typeMismatch "Object" invalid

instance DA.ToJSON Object where
    toJSON (ObjectRectangle (Rectangle common)) = DA.toJSON common

    toJSON (ObjectEllipse (Ellipse common)) =
        let fields = objectCommonFields common
        in DA.object $ fields ++ ["ellipse" .= True]

    toJSON (ObjectPoint (Point common)) =
        let fields = objectCommonFields common
        in DA.object $ fields ++ ["point" .= True]

    toJSON (ObjectPolygon (Polygon common coords)) =
        let fields = objectCommonFields common
        in DA.object $ fields ++ ["polygon" .= coords]

    toJSON (ObjectPolyline (Polyline common coords)) =
        let fields = objectCommonFields common
        in DA.object $ fields ++ ["polyline" .= coords]

objectCommonFields :: ObjectCommon -> [(Text, DA.Value)]
objectCommonFields = fields . DA.toJSON
    where
    fields (DA.Object v) = HML.toList v
    fields _             = []

data Tile = Tile
    { tileTerrain :: ![Int]
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = constructorTagModifier 4 }) ''Tile)

data Terrain = Terrain
    { terrainName :: !Text
    , terrainTile :: !Int
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = constructorTagModifier 7 }) ''Terrain)

data Tileset = Tileset
    { tilesetFirstGid          :: !(Maybe Gid)
    , tilesetImage             :: !Text
    , tilesetName              :: !Text
    , tilesetTileWidth         :: !Int
    , tilesetTileHeight        :: !Int
    , tilesetImageWidth        :: !Int
    , tilesetImageHeight       :: !Int
    , tilesetTileOffset        :: !Coord
    , tilesetProperties        :: !Properties
    , tilesetPropertyTypes     :: !Properties
    , tilesetMargin            :: !Int
    , tilesetSpacing           :: !Int
    , tilesetTileProperties    :: !(Maybe (DM.Map Gid Properties))
    , tilesetTilePropertyTypes :: !(Maybe (DM.Map Gid Properties))
    , tilesetTerrains          :: !(Maybe [Terrain])
    , tilesetColumns           :: !Int
    , tilesetTileCount         :: !Int
    , tilesetTiles             :: !(Maybe (DM.Map Gid Tile))
    } deriving (Show, Eq)

instance DA.FromJSON Tileset where
    parseJSON (DA.Object v) = Tileset
        <$> v .:? "firstgid"
        <*> v .: "image"
        <*> v .: "name"
        <*> v .: "tilewidth"
        <*> v .: "tileheight"
        <*> v .: "imagewidth"
        <*> v .: "imageheight"
        <*> withDefault v "tileoffset" (Coord 0 0)
        <*> withDefault v "properties" DM.empty
        <*> withDefault v "propertytypes" DM.empty
        <*> v .: "margin"
        <*> v .: "spacing"
        <*> v .:? "tileproperties"
        <*> v .:? "tilepropertytypes"
        <*> v .:? "terrains"
        <*> v .: "columns"
        <*> v .: "tilecount"
        <*> v .:? "tiles"

    parseJSON invalid = DA.typeMismatch "Tileset" invalid

instance DA.ToJSON Tileset where
    toJSON (Tileset firstgid image name tilewidth tileheight imagewidth imageheight tileoffset properties propertytypes margin spacing tileproperties tilepropertytypes terrains columns tilecount tiles) = DA.object
        [ "firstgid" .= firstgid
        , "image" .= image
        , "name" .= name
        , "tilewidth" .= tilewidth
        , "tileheight" .= tileheight
        , "imagewidth" .= imagewidth
        , "imageheight" .= imageheight
        , "tileoffset" .= tileoffset
        , "properties" .= properties
        , "propertytypes" .= propertytypes
        , "margin" .= margin
        , "spacing" .= spacing
        , "tileproperties" .= tileproperties
        , "tilepropertytypes" .= tilepropertytypes
        , "terrains" .= terrains
        , "columns" .= columns
        , "tilecount" .= tilecount
        , "tiles" .= tiles
        ]

data LayerCommon = LayerCommon
    { layerCommonWidth      :: !Int
    , layerCommonHeight     :: !Int
    , layerCommonName       :: !Text
    , layerCommonOpacity    :: !Double
    , layerCommonVisible    :: !Bool
    , layerCommonX          :: !Int
    , layerCommonY          :: !Int
    , layerCommonProperties :: !Properties
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = constructorTagModifier 11 }) ''LayerCommon)

data Layer
    = LayerTileLayer !TileLayer
    | LayerObjectGroup !ObjectGroup
    | LayerImageLayer !ImageLayer
    deriving (Show, Eq)

data TileLayer = TileLayer
    { tileLayerCommon :: !LayerCommon
    , tileLayerData   :: !TileLayerData
    } deriving (Show, Eq)

data TileLayerData =
    TileLayerDataCsv !(UV.Vector Word32) |
    TileLayerDataBase64 !CompressionType !Text
    deriving (Show, Eq)

data CompressionType =
    NoCompression |
    ZlibCompression |
    GZipCompression
    deriving (Show, Eq)

data EncodingType =
    CsvEncoding |
    Base64Encoding
    deriving (Show, Eq)

data ObjectGroup = ObjectGroup
    { objectGroupCommon  :: !LayerCommon
    , objectGroupObjects :: !(Vector Object)
    } deriving (Show, Eq)

data ImageLayer = ImageLayer
    { imageLayerCommon :: !LayerCommon
    , imageLayerImage  :: !Text
    } deriving (Show, Eq)

data Map = Map
    { mapVersion         :: !Double
    , mapWidth           :: !Int
    , mapHeight          :: !Int
    , mapTileWidth       :: !Int
    , mapTileHeight      :: !Int
    , mapOrientation     :: !Orientation
    , mapLayers          :: !(Vector Layer)
    , mapTilesets        :: !(Vector Tileset)
    , mapBackgroundColor :: !(Maybe Color)
    , mapRenderOrder     :: !RenderOrder
    , mapProperties      :: !Properties
    , mapNextObjectId    :: !Int
    } deriving (Show, Eq)

instance DA.FromJSON Layer where
    parseJSON (DA.Object v) = do
        layerType <- v .: "type"
        common <- parseLayerCommon
        parseLayer layerType common

        where
        parseLayerCommon = LayerCommon
            <$> v .: "width"
            <*> v .: "height"
            <*> v .: "name"
            <*> v .: "opacity"
            <*> v .: "visible"
            <*> v .: "x"
            <*> v .: "y"
            <*> withDefault v "properties" DM.empty

        parseLayer :: Text -> LayerCommon -> DA.Parser Layer
        parseLayer "tilelayer" common = do
            encoding <- v .:? "encoding" .!= CsvEncoding
            compression <- v .:? "compression" .!= NoCompression
            data_ <- maybe (fail "tilelayer needs data field.") (parseTileLayerData encoding compression) $ HML.lookup "data" v
            return (LayerTileLayer (TileLayer common data_))

        parseLayer "objectgroup" common = LayerObjectGroup . ObjectGroup common
            <$> v .: "objects"

        parseLayer "imagelayer" common = LayerImageLayer . ImageLayer common
            <$> v .: "image"

        parseLayer _ _ = mzero

    parseJSON invalid = DA.typeMismatch "Layer" invalid

parseTileLayerData :: EncodingType -> CompressionType -> DA.Value -> DA.Parser TileLayerData
parseTileLayerData CsvEncoding _ = fmap TileLayerDataCsv . DA.parseJSON
parseTileLayerData Base64Encoding compression = DA.withText "tilelayer.data" (return . TileLayerDataBase64 compression)

instance DA.ToJSON Layer where
    toJSON (LayerTileLayer (TileLayer common (TileLayerDataCsv d))) =
        let fields = layerCommonFields common
        in DA.object $ fields ++ ["data" .= d, "type" .= ("tilelayer" :: Text)]

    toJSON (LayerTileLayer (TileLayer common (TileLayerDataBase64 c d))) =
        let fields = layerCommonFields common
            otherFields =
                [ "encoding" .= Base64Encoding
                , "data" .= d
                , "type" .= ("tilelayer" :: Text)
                ]
            compressionFields = case c of
                NoCompression -> []
                _             -> [ "compression" .= c ]

        in DA.object $ fields ++ compressionFields ++ otherFields

    toJSON (LayerObjectGroup (ObjectGroup common objects)) =
        let fields = layerCommonFields common
        in DA.object $ fields ++ ["objects" .= objects, "type" .= ("objectgroup" :: Text)]

    toJSON (LayerImageLayer (ImageLayer common image)) =
        let fields = layerCommonFields common
        in DA.object $ fields ++ ["type" .= ("imagelayer" :: Text), "image" .= image]

layerCommonFields :: LayerCommon -> [(Text, DA.Value)]
layerCommonFields = fields . DA.toJSON
    where
    fields (DA.Object v) = HML.toList v
    fields _             = []

instance DA.FromJSON EncodingType where
    parseJSON = DA.withText "EncodingType" $ \t ->
        case t of
            "csv"    -> return CsvEncoding
            "base64" -> return Base64Encoding
            _        -> fail "unknown encoding"

instance DA.ToJSON EncodingType where
    toJSON CsvEncoding    = DA.String "csv"
    toJSON Base64Encoding = DA.String "base64"

instance DA.FromJSON CompressionType where
    parseJSON = DA.withText "CompressionType" $ \t ->
        case t of
            "gzip" -> return GZipCompression
            "zlib" -> return ZlibCompression
            _      -> fail "unknown compression"

instance DA.ToJSON CompressionType where
    toJSON NoCompression   = DA.String "nocompression"
    toJSON GZipCompression = DA.String "gzip"
    toJSON ZlibCompression = DA.String "zlib"

instance DA.FromJSON Map where
    parseJSON (DA.Object v) = Map
        <$> v .: "version"
        <*> v .: "width"
        <*> v .: "height"
        <*> v .: "tilewidth"
        <*> v .: "tileheight"
        <*> v .: "orientation"
        <*> v .: "layers"
        <*> v .: "tilesets"
        <*> v .:? "backgroundcolor"
        <*> v .: "renderorder"
        <*> withDefault v "properties" DM.empty
        <*> v .: "nextobjectid"

    parseJSON invalid = DA.typeMismatch "Map" invalid

instance DA.ToJSON Map where
    toJSON (Map version width height tilewidth tileheight orientation layers tilesets backgroundcolor renderorder properties nextobjectid) = DA.object
        [ "version" .= version
        , "width" .= width
        , "height" .= height
        , "tilewidth" .= tilewidth
        , "tileheight" .= tileheight
        , "orientation" .= orientation
        , "layers" .= layers
        , "tilesets" .= tilesets
        , "backgroundcolor" .= backgroundcolor
        , "renderorder" .= renderorder
        , "properties" .= properties
        , "nextobjectid" .= nextobjectid
        ]
