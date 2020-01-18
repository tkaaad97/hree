{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Graphics.Format.Tiled.Types
    ( Color
    , CompressionType(..)
    , Coord(..)
    , Ellipse(..)
    , EncodingType(..)
    , Gid
    , Image(..)
    , ImageLayer(..)
    , Layer(..)
    , LayerMid(..)
    , LayerCommon(..)
    , Map(..)
    , MapMid(..)
    , Object(..)
    , ObjectCommon(..)
    , ObjectGroup(..)
    , Orientation(..)
    , Point(..)
    , Polygon(..)
    , Polyline(..)
    , Properties(..)
    , PropertyValue(..)
    , Rectangle(..)
    , RenderOrder(..)
    , Terrain(..)
    , Tile(..)
    , TileLayer(..)
    , TileLayerMid(..)
    , TileLayerData(..)
    , Tileset(..)
    , TilesetSource(..)
    , Wangcolor(..)
    , Wangset(..)
    , Wangtile(..)
    ) where

import Control.Monad (mzero)
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as DA (FromJSON, ToJSON, Value(..), object,
                                   parseJSON, toJSON, withArray, withObject,
                                   withText)
import qualified Data.Aeson.TH as DA (Options(..), defaultOptions, deriveJSON)
import qualified Data.Aeson.Types as DA (Object, Parser, typeMismatch)
import qualified Data.Foldable (toList)
import qualified Data.HashMap.Lazy as HML (lookup, toList)
import qualified Data.Map.Strict as Map (Map, empty, fromList, null, toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as BV (Vector, fromList, mapM, null, toList)
import qualified Data.Vector.Unboxed as UV (Vector, length, (!))
import Data.Word (Word32)
import Graphics.Format.Tiled.Internal
import qualified Linear (V4(..))

type Gid = Int

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

data PropertyValue =
    PropertyString !Text |
    PropertyFloat !Double |
    PropertyBool !Bool |
    PropertyColor !Color |
    PropertyFile !Text
    deriving (Show, Eq)

newtype Properties = Properties (Map.Map Text PropertyValue)
    deriving (Show, Eq)

instance DA.FromJSON Properties where
    parseJSON = DA.withArray "Properties" $
        fmap (Properties . Map.fromList . BV.toList) . BV.mapM parseProperty

parseProperty :: DA.Value -> DA.Parser (Text, PropertyValue)
parseProperty = DA.withObject "Property" $ \v -> do
    propertyName <- v .: "name"
    propertyType <- v .:? "type" .!= "string"
    propertyValue <- parsePropertyValue propertyType =<< v .: "value"
    return (propertyName, propertyValue)

parsePropertyValue :: Text -> DA.Value -> DA.Parser PropertyValue
parsePropertyValue "string" = fmap PropertyString . DA.parseJSON
parsePropertyValue "float" = fmap PropertyFloat . DA.parseJSON
parsePropertyValue "bool" = fmap PropertyBool . DA.parseJSON
parsePropertyValue "color" = fmap PropertyString . DA.parseJSON
parsePropertyValue "file" = fmap PropertyFile . DA.parseJSON
parsePropertyValue invalid = fail $ "unknown property type:" ++ Text.unpack invalid

instance DA.ToJSON Properties where
    toJSON (Properties props) = DA.Array . BV.fromList . map propertyToJSON . Map.toList $ props

propertyToJSON :: (Text, PropertyValue) -> DA.Value
propertyToJSON (name, PropertyString v) = DA.object ["name" .= name, "type" .= ("string" :: Text), "value" .= v]
propertyToJSON (name, PropertyFloat v) = DA.object ["name" .= name, "type" .= ("float" :: Text), "value" .= v]
propertyToJSON (name, PropertyBool v) = DA.object ["name" .= name, "type" .= ("bool" :: Text), "value" .= v]
propertyToJSON (name, PropertyColor v) = DA.object ["name" .= name, "type" .= ("color" :: Text), "value" .= v]
propertyToJSON (name, PropertyFile v) = DA.object ["name" .= name, "type" .= ("file" :: Text), "value" .= v]

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
    , polygonCoords :: !(BV.Vector Coord)
    } deriving (Show, Eq)

data Polyline = Polyline
    { polylineCommon :: !ObjectCommon
    , polylineCoords :: !(BV.Vector Coord)
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
            <*> withDefault v "properties" (Properties Map.empty)
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

data Terrain = Terrain
    { terrainName :: !Text
    , terrainTile :: !Int
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = constructorTagModifier 7 }) ''Terrain)

data Wangcolor = Wangcolor
    { wangcolorColor       :: !Color
    , wangcolorName        :: !Text
    , wangcolorProbability :: !Double
    , wangcolorTile        :: !Int
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = constructorTagModifier 9 }) ''Wangcolor)

data Wangtile = Wangtile
    { wangtileTileId :: !Int
    , wangtileWangId :: !Text
    , wangtileDflip  :: !Bool
    , wangtileHflip  :: !Bool
    , wangtileVflip  :: !Int
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = constructorTagModifier 8 }) ''Wangtile)

data Wangset = Wangset
    { wangsetName         :: !Text
    , wangsetTile         :: !Int
    , wangsetCornerColors :: !(BV.Vector Wangcolor)
    , wangsetEdgeColors   :: !(BV.Vector Wangcolor)
    , wangsetWangtiles    :: !(BV.Vector Wangtile)
    , wangsetProperties   :: !Properties
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = constructorTagModifier 7 }) ''Wangset)

data Image = Image
    { imageSource :: !Text
    , imageWidth  :: !Int
    , imageHeight :: !Int
    } deriving (Show, Eq)

data ObjectGroup = ObjectGroup
    { objectGroupCommon  :: !LayerCommon
    , objectGroupObjects :: !(BV.Vector Object)
    } deriving (Show, Eq)

data Tile = Tile
    { tileId          :: !Word32
    , tileImage       :: !(Maybe Image)
    , tileObjectGroup :: !(Maybe ObjectGroup)
    , tileProbability :: !(Maybe Double)
    , tileTerrain     :: !(Maybe (Linear.V4 Word32))
    , tileType        :: !(Maybe Text)
    , tileProperties  :: !Properties
    } deriving (Show, Eq)

data TilesetSource =
    TilesetSourceFile !(Maybe Gid) !Text |
    TilesetSourceInplace !Tileset
    deriving (Show, Eq)

data Tileset = Tileset
    { tilesetFirstGid   :: !(Maybe Gid)
    , tilesetName       :: !Text
    , tilesetTileWidth  :: !Int
    , tilesetTileHeight :: !Int
    , tilesetSpacing    :: !Int
    , tilesetTileCount  :: !Int
    , tilesetMargin     :: !Int
    , tilesetColumns    :: !Int
    , tilesetImage      :: !(Maybe Image)
    , tilesetTileOffset :: !Coord
    , tilesetTerrains   :: !(BV.Vector Terrain)
    , tilesetTiles      :: !(Map.Map Gid Tile)
    , tilesetWangsets   :: !(BV.Vector Wangset)
    , tilesetProperties :: !Properties
    } deriving (Show, Eq)

instance DA.FromJSON TilesetSource where
    parseJSON o @ (DA.Object v) = do
        firstgid <- v .:? "firstgid"
        source <- v .: "source"
        case source of
            Just sourcePath -> return (TilesetSourceFile firstgid sourcePath)
            Nothing         -> TilesetSourceInplace <$> DA.parseJSON o

    parseJSON invalid = DA.typeMismatch "TilesetSource" invalid

instance DA.ToJSON TilesetSource where
    toJSON (TilesetSourceFile firstgid sourcePath) = DA.object
        [ "firstgid" .= firstgid
        , "source" .= sourcePath
        ]
    toJSON (TilesetSourceInplace tileset) = DA.toJSON tileset

instance DA.FromJSON Tileset where
    parseJSON (DA.Object v) = do
        imgSource <- v .:? "image"
        imgWidth <- v .:? "imagewidth"
        imgHeight <- v .:? "imageheight"
        let image = Image <$> imgSource <*> imgWidth <*> imgHeight
        Tileset
            <$> v .:? "firstgid"
            <*> v .: "name"
            <*> v .: "tilewidth"
            <*> v .: "tileheight"
            <*> v .: "spacing"
            <*> v .: "tilecount"
            <*> v .: "margin"
            <*> v .: "columns"
            <*> return image
            <*> withDefault v "tileoffset" (Coord 0 0)
            <*> withDefault v "terrains" mempty
            <*> withDefault v "tiles" mempty
            <*> withDefault v "wangsets" mempty
            <*> withDefault v "properties" (Properties Map.empty)

    parseJSON invalid = DA.typeMismatch "Tileset" invalid

instance DA.ToJSON Tileset where
    toJSON (Tileset firstgid name tilewidth tileheight spacing tilecount margin columns image tileoffset terrains tiles wangsets properties) =
        let terrains' = if BV.null terrains then Nothing else Just terrains
            tiles' = if Map.null tiles then Nothing else Just tiles
            wangsets' = if BV.null wangsets then Nothing else Just wangsets
        in DA.object
            [ "firstgid" .= firstgid
            , "name" .= name
            , "tilewidth" .= tilewidth
            , "tileheight" .= tileheight
            , "spacing" .= spacing
            , "tilecount" .= tilecount
            , "margin" .= margin
            , "columns" .= columns
            , "image" .= (imageSource <$> image)
            , "imagewidth" .= (imageWidth <$> image)
            , "imageheight" .= (imageHeight <$> image)
            , "tileoffset" .= tileoffset
            , "terrains" .= terrains'
            , "tiles" .= tiles'
            , "wangsets" .= wangsets'
            , "properties" .= properties
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

data LayerMid
    = LayerMidTileLayer !TileLayerMid
    | LayerMidObjectGroup !ObjectGroup
    | LayerMidImageLayer !ImageLayer
    deriving (Show, Eq)

data Layer
    = LayerTileLayer !TileLayer
    | LayerObjectGroup !ObjectGroup
    | LayerImageLayer !ImageLayer
    deriving (Show, Eq)

data TileLayerMid = TileLayerMid
    { tileLayerMidCommon :: !LayerCommon
    , tileLayerMidData   :: !TileLayerData
    } deriving (Show, Eq)

data TileLayer = TileLayer
    { tileLayerCommon      :: !LayerCommon
    , tileLayerData        :: !(UV.Vector Word32)
    , tileLayerEncoding    :: !EncodingType
    , tileLayerCompression :: !CompressionType
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

data ImageLayer = ImageLayer
    { imageLayerCommon :: !LayerCommon
    , imageLayerImage  :: !Text
    } deriving (Show, Eq)

data MapMid = MapMid
    { mapMidVersion         :: !Double
    , mapMidWidth           :: !Int
    , mapMidHeight          :: !Int
    , mapMidTileWidth       :: !Int
    , mapMidTileHeight      :: !Int
    , mapMidOrientation     :: !Orientation
    , mapMidLayers          :: !(BV.Vector LayerMid)
    , mapMidTilesets        :: !(BV.Vector TilesetSource)
    , mapMidBackgroundColor :: !(Maybe Color)
    , mapMidRenderOrder     :: !RenderOrder
    , mapMidProperties      :: !Properties
    , mapMidNextObjectId    :: !Int
    } deriving (Show, Eq)

data Map = Map
    { mapVersion         :: !Double
    , mapWidth           :: !Int
    , mapHeight          :: !Int
    , mapTileWidth       :: !Int
    , mapTileHeight      :: !Int
    , mapOrientation     :: !Orientation
    , mapLayers          :: !(BV.Vector Layer)
    , mapTilesets        :: !(BV.Vector Tileset)
    , mapBackgroundColor :: !(Maybe Color)
    , mapRenderOrder     :: !RenderOrder
    , mapProperties      :: !Properties
    , mapNextObjectId    :: !Int
    } deriving (Show, Eq)

instance DA.FromJSON LayerMid where
    parseJSON (DA.Object v) = do
        layerType <- v .: "type"
        common <- parseLayerCommon v
        parseLayer layerType common v

    parseJSON invalid = DA.typeMismatch "LayerMid" invalid

parseLayerCommon :: DA.Object -> DA.Parser LayerCommon
parseLayerCommon v = LayerCommon
    <$> v .: "width"
    <*> v .: "height"
    <*> v .: "name"
    <*> v .: "opacity"
    <*> v .: "visible"
    <*> v .: "x"
    <*> v .: "y"
    <*> withDefault v "properties" (Properties Map.empty)

parseLayer :: Text -> LayerCommon -> DA.Object -> DA.Parser LayerMid
parseLayer "tilelayer" common v = do
    encoding <- v .:? "encoding" .!= CsvEncoding
    compression <- v .:? "compression" .!= NoCompression
    data_ <- maybe (fail "tilelayer needs data field.") (parseTileLayerData encoding compression) $ HML.lookup "data" v
    return (LayerMidTileLayer (TileLayerMid common data_))
parseLayer "objectgroup" common v = LayerMidObjectGroup . ObjectGroup common
    <$> v .: "objects"
parseLayer "imagelayer" common v = LayerMidImageLayer . ImageLayer common
    <$> v .: "image"
parseLayer _ _ _ = mzero

parseTileLayerData :: EncodingType -> CompressionType -> DA.Value -> DA.Parser TileLayerData
parseTileLayerData CsvEncoding _ = fmap TileLayerDataCsv . DA.parseJSON
parseTileLayerData Base64Encoding compression = DA.withText "tilelayer.data" (return . TileLayerDataBase64 compression)

instance DA.ToJSON LayerMid where
    toJSON (LayerMidTileLayer (TileLayerMid common (TileLayerDataCsv d))) =
        let fields = layerCommonFields common
        in DA.object $ fields ++ ["data" .= d, "type" .= ("tilelayer" :: Text)]

    toJSON (LayerMidTileLayer (TileLayerMid common (TileLayerDataBase64 c d))) =
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

    toJSON (LayerMidObjectGroup (ObjectGroup common objects)) =
        let fields = layerCommonFields common
        in DA.object $ fields ++ ["objects" .= objects, "type" .= ("objectgroup" :: Text)]

    toJSON (LayerMidImageLayer (ImageLayer common image)) =
        let fields = layerCommonFields common
        in DA.object $ fields ++ ["type" .= ("imagelayer" :: Text), "image" .= image]

layerCommonFields :: LayerCommon -> [(Text, DA.Value)]
layerCommonFields = fields . DA.toJSON
    where
    fields (DA.Object v) = HML.toList v
    fields _             = []

instance DA.FromJSON ObjectGroup where
    parseJSON (DA.Object v) = do
        common <- parseLayerCommon v
        ObjectGroup common <$> v .: "objects"
    parseJSON invalid = DA.typeMismatch "ObjectGroup" invalid

instance DA.ToJSON ObjectGroup where
    toJSON = DA.toJSON . LayerMidObjectGroup

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

instance DA.FromJSON MapMid where
    parseJSON (DA.Object v) = MapMid
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
        <*> withDefault v "properties" (Properties Map.empty)
        <*> v .: "nextobjectid"

    parseJSON invalid = DA.typeMismatch "MapMid" invalid

instance DA.ToJSON MapMid where
    toJSON (MapMid version width height tilewidth tileheight orientation layers tilesets backgroundcolor renderorder properties nextobjectid) = DA.object
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

instance DA.FromJSON Tile where
    parseJSON (DA.Object v) = do
        imgSource <- v .:? "image"
        imgWidth <- v .:? "imagewidth"
        imgHeight <- v .:? "imageheight"
        let image = Image <$> imgSource <*> imgWidth <*> imgHeight
        terrain <- maybe (return Nothing) (fmap Just . fromVectorToV4) =<< v .:? "terrain"
        Tile
            <$> v .: "id"
            <*> return image
            <*> v .:? "objectgroup"
            <*> v .:? "probability"
            <*> return terrain
            <*> v .:? "type"
            <*> withDefault v "properties" (Properties Map.empty)

    parseJSON invalid = DA.typeMismatch "Tile" invalid

instance DA.ToJSON Tile where
    toJSON (Tile tid image objectgroup probability terrain type' p @ (Properties props)) =
        let p' = if Map.null props then Nothing else Just p
        in DA.object
            [ "id" .= tid
            , "image" .= (imageSource <$> image)
            , "imagewidth" .= (imageWidth <$> image)
            , "imageheight" .= (imageHeight <$> image)
            , "objectgroup" .= objectgroup
            , "probability" .= probability
            , "terrain" .= (Data.Foldable.toList <$> terrain)
            , "type" .= type'
            , "properties" .= p'
            ]

fromVectorToV4 :: UV.Vector Word32 -> DA.Parser (Linear.V4 Word32)
fromVectorToV4 v
    | UV.length v == 4 =
        let a0 = v UV.! 0
            a1 = v UV.! 1
            a2 = v UV.! 2
            a3 = v UV.! 3
        in return $ Linear.V4 a0 a1 a2 a3
    | otherwise = fail "bad array size"
