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
    , Tileset(..)
    ) where

import Control.Monad (mzero)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as DA (FromJSON, ToJSON, Value(..), object,
                                   parseJSON, toJSON)
import qualified Data.Aeson.TH as DA (Options(..), defaultOptions, deriveJSON)
import qualified Data.Aeson.Types as DAT (Object, Parser, typeMismatch)
import qualified Data.HashMap.Lazy as HML (toList)
import qualified Data.Map as DM (Map, empty)
import Data.Maybe (fromMaybe)
import qualified Data.Text (Text)
import Data.Vector (Vector)
import Graphics.Format.Tiled.Internal

type Gid = Int

type Properties = DM.Map Data.Text.Text Data.Text.Text

withDefault :: (DA.FromJSON b) => DAT.Object -> Data.Text.Text -> b -> DAT.Parser b
withDefault a label b =
    a .:? label >>= return . fromMaybe b

data Coord = Coord
    { coordX :: Int
    , coordY :: Int
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = constructorTagModifier 5 }) ''Coord)

type Color = Data.Text.Text

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
    , objectCommonType       :: !Data.Text.Text
    , objectCommonWidth      :: !Int
    , objectCommonHeight     :: !Int
    , objectCommonName       :: !Data.Text.Text
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

    parseJSON invalid = DAT.typeMismatch "Object" invalid

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

objectCommonFields :: ObjectCommon -> [(Data.Text.Text, DA.Value)]
objectCommonFields = fields . DA.toJSON
    where
    fields (DA.Object v) = HML.toList v
    fields _             = []

data Tile = Tile
    { tileTerrain :: ![Int]
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = constructorTagModifier 4 }) ''Tile)

data Terrain = Terrain
    { terrainName :: !Data.Text.Text
    , terrainTile :: !Int
    } deriving (Show, Eq)
$(DA.deriveJSON (DA.defaultOptions { DA.fieldLabelModifier = constructorTagModifier 7 }) ''Terrain)

data Tileset = Tileset
    { tilesetFirstGid          :: !(Maybe Gid)
    , tilesetImage             :: !Data.Text.Text
    , tilesetName              :: !Data.Text.Text
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

    parseJSON invalid = DAT.typeMismatch "Tileset" invalid

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
    , layerCommonName       :: !Data.Text.Text
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

data TileLayer = TileLayer !LayerCommon !(Vector Int)
    deriving (Show, Eq)

data ObjectGroup = ObjectGroup
    { objectGroupCommon  :: !LayerCommon
    , objectGroupObjects :: !(Vector Object)
    } deriving (Show, Eq)

data ImageLayer = ImageLayer
    { imageLayerCommon :: !LayerCommon
    , imageLayerImage  :: !Data.Text.Text
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

        parseLayer :: Data.Text.Text -> LayerCommon -> DAT.Parser Layer
        parseLayer "tilelayer" common =
            LayerTileLayer . TileLayer common <$> v .: "data"

        parseLayer "objectgroup" common = LayerObjectGroup . ObjectGroup common
            <$> v .: "objects"

        parseLayer "imagelayer" common = LayerImageLayer . ImageLayer common
            <$> v .: "image"

        parseLayer _ _ = mzero

    parseJSON invalid = DAT.typeMismatch "Layer" invalid

instance DA.ToJSON Layer where
    toJSON (LayerTileLayer (TileLayer common d)) =
        let fields = layerCommonFields common
        in DA.object $ fields ++ ["data" .= d, "type" .= ("tilelayer" :: String)]

    toJSON (LayerObjectGroup (ObjectGroup common objects)) =
        let fields = layerCommonFields common
        in DA.object $ fields ++ ["objects" .= objects, "type" .= ("objectgroup" :: String)]

    toJSON (LayerImageLayer (ImageLayer common image)) =
        let fields = layerCommonFields common
        in DA.object $ fields ++ ["type" .= ("imagelayer" :: Data.Text.Text), "image" .= image]

layerCommonFields :: LayerCommon -> [(Data.Text.Text, DA.Value)]
layerCommonFields = fields . DA.toJSON
    where
    fields (DA.Object v) = HML.toList v
    fields _             = []

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

    parseJSON invalid = DAT.typeMismatch "Map" invalid

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
