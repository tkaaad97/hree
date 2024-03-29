{-# LANGUAGE OverloadedStrings #-}
module TiledSpec
    (spec
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as DA (FromJSON(..), ToJSON(..), Value(..), decode,
                                   object)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as Map (empty, singleton)
import Data.Text (Text)
import qualified Data.Vector as BV (fromList)
import qualified Data.Vector.Unboxed as UV (fromList)
import Data.Word (Word32)
import Hree.Loader.Tiled.Types
import qualified Linear (V4(..))
import qualified Test.Hspec as Hspec (Spec, it, shouldBe)

spec :: Hspec.Spec
spec = do
    testJSON "Coord" "{\"x\":1,\"y\":2}" (Coord 1 2) (DA.object ["x" .= (1 :: Int), "y" .= (2 :: Int)])

    testJSON "Orientation1" "\"orthogonal\"" OrientationOrthogonal (DA.String "orthogonal")
    testJSON "Orientation2" "\"isometric\"" OrientationIsometric (DA.String "isometric")
    testJSON "Orientation3" "\"staggered\"" OrientationStaggered (DA.String "staggered")

    testJSON "Tile"
        "{\"id\":1,\"terrain\":[0, 1, 0, 1]}"
        (Tile Nothing 1 Nothing Nothing Nothing (Just $ Linear.V4 0 1 0 1) Nothing (Properties mempty))
        (DA.object [("image", DA.Null), "terrain" .= ([0, 1, 0, 1] :: [Word32]), ("objectgroup", DA.Null), ("probability", DA.Null), ("imageheight", DA.Null), ("imagewidth", DA.Null), "id" .= (1 :: Word32), ("type", DA.Null), ("animation", DA.Null), ("properties", DA.Null)])

    testJSON "Terrain" "{\"name\":\"ground\",\"tile\":12}" (Terrain "ground" 12) (DA.object ["name" .= ("ground" :: Text), "tile" .= (12 :: Int)])

    testJSON "Tileset"
        "{\"columns\":19,\"firstgid\":1,\"image\":\"../image/fishbaddie_parts.png\",\"imageheight\":480,\"imagewidth\":640,\"margin\":3,\"name\":\"tileset1\",\"properties\":[{\"name\":\"myProperty1\",\"type\":\"string\",\"value\":\"myProperty1_value\"}],\"spacing\":1,\"tilecount\":266,\"tileheight\":32,\"tilewidth\":32,\"tileoffset\":{\"x\":-12,\"y\":-12}}"
        (Tileset (Just 1) "tileset1" 32 32 1 266 3 19 (Just $ Image "../image/fishbaddie_parts.png" (Just 640) (Just 480)) (Coord (-12) (-12)) mempty mempty mempty (Properties $ Map.singleton "myProperty1" (PropertyString "myProperty1_value")))
        (DA.object ["firstgid" .= (Just 1 :: Maybe Int), "image" .= ("../image/fishbaddie_parts.png" :: Text), "name" .= ("tileset1" :: Text), "tilewidth" .= (32 :: Int), "tileheight" .= (32 :: Int), "imagewidth" .= (640 :: Int), "imageheight" .= (480 :: Int), "tileoffset" .= Coord (-12) (-12), "properties" .= [DA.object ["name" .= ("myProperty1" :: Text), "type" .= ("string" :: Text), "value" .= ("myProperty1_value" :: Text)]], "margin" .= (3 :: Int), "spacing" .= (1 :: Int), "columns" .= (19 :: Int), "tilecount" .= (266 :: Int), ("terrains", DA.Null), ("tiles", DA.Null), ("wangsets", DA.Null)])

    testJSON "Object Rectangle"
        "{\"height\":0,\"id\":1,\"name\":\"villager\",\"properties\":[{\"name\":\"hp\",\"type\":\"string\",\"value\":\"12\"}],\"rotation\":0,\"type\":\"npc\",\"visible\":true,\"width\":0,\"x\":32,\"y\":48}"
        (ObjectRectangle (Rectangle (ObjectCommon 1 "npc" 0 0 "villager" (Properties $ Map.singleton "hp" (PropertyString "12")) True 32 48 0)))
        (DA.object ["id" .= (1 :: Int), "type" .= ("npc" :: Text), "width" .= (0 :: Int), "height" .= (0 :: Int), "name" .= ("villager" :: Text), "properties" .= [DA.object ["name" .= ("hp" :: Text), "type" .= ("string" :: Text), "value" .= ("12" :: Text)]], "visible" .= True, "x" .= (32 :: Int), "y" .= (48 :: Int), "rotation" .= (0 :: Double)])

    testJSON "Object Ellipse"
        "{\"ellipse\":true,\"height\":152,\"id\":13,\"name\":\"ellipse1\",\"properties\":[],\"rotation\":0,\"type\":\"shape\",\"visible\":true,\"width\":248,\"x\":560,\"y\":808}"
        (ObjectEllipse (Ellipse (ObjectCommon 13 "shape" 248 152 "ellipse1" (Properties Map.empty) True 560 808 0)))
        (DA.object ["ellipse" .= True, "id" .= (13 :: Int), "type" .= ("shape" :: Text), "width" .= (248 :: Int), "height" .= (152 :: Int), "name" .= ("ellipse1" :: Text), "properties" .= ([] :: [DA.Value]), "visible" .= True, "x" .= (560 :: Int), "y" .= (808 :: Int), "rotation" .= (0 :: Double)])

    testJSON "Object Point"
        "{\"point\":true,\"height\":0,\"id\":20,\"name\":\"point2\",\"properties\":[],\"rotation\":0,\"type\":\"shape\",\"visible\":true,\"width\":0,\"x\":220,\"y\":350}"
        (ObjectPoint (Point (ObjectCommon 20 "shape" 0 0 "point2" (Properties Map.empty) True 220 350 0)))
        (DA.object ["point" .= True, "id" .= (20 :: Int), "type" .= ("shape" :: Text), "width" .= (0 :: Int), "height" .= (0 :: Int), "name" .= ("point2" :: Text), "properties" .= ([] :: [DA.Value]), "visible" .= True, "x" .= (220 :: Int), "y" .= (350 :: Int), "rotation" .= (0 :: Double)])

    testJSON "Object Polygon"
        "{\"height\":0,\"id\":15,\"name\":\"polygon3\",\"properties\":[],\"polygon\":[{\"x\":0,\"y\":0},{\"x\":152,\"y\":88},{\"x\":136,\"y\":-128},{\"x\":80,\"y\":-280},{\"x\":16,\"y\":-288}],\"rotation\":0,\"type\":\"shape\",\"visible\":true,\"width\":0,\"x\":-176,\"y\":432}"
        (ObjectPolygon (Polygon (ObjectCommon 15 "shape" 0 0 "polygon3" (Properties Map.empty) True (-176) 432 0) (BV.fromList [Coord 0 0, Coord 152 88, Coord 136 (-128), Coord 80 (-280), Coord 16 (-288)])))
        (DA.object ["id" .= (15 :: Int), "type" .= ("shape" :: Text), "width" .= (0 :: Int), "height" .= (0 :: Int), "name" .= ("polygon3" :: Text), "properties" .= ([] :: [DA.Value]), "visible" .= True, "x" .= (-176 :: Int), "y" .= (432 :: Int), "rotation" .= (0 :: Double), "polygon" .= BV.fromList [Coord 0 0, Coord 152 88, Coord 136 (-128), Coord 80 (-280), Coord 16 (-288)]])

    testJSON "Object Polyline"
        "{\"height\":0,\"id\":15,\"name\":\"polyline4\",\"properties\":[],\"polyline\":[{\"x\":0,\"y\":0},{\"x\":152,\"y\":88},{\"x\":136,\"y\":-128},{\"x\":80,\"y\":-280},{\"x\":16,\"y\":-288}],\"rotation\":0,\"type\":\"shape\",\"visible\":true,\"width\":0,\"x\":-176,\"y\":432}"
        (ObjectPolyline (Polyline (ObjectCommon 15 "shape" 0 0 "polyline4" (Properties Map.empty) True (-176) 432 0) (BV.fromList [Coord 0 0, Coord 152 88, Coord 136 (-128), Coord 80 (-280), Coord 16 (-288)])))
        (DA.object ["id" .= (15 :: Int), "type" .= ("shape" :: Text), "width" .= (0 :: Int), "height" .= (0 :: Int), "name" .= ("polyline4" :: Text), "properties" .= ([] :: [DA.Value]), "visible" .= True, "x" .= (-176 :: Int), "y" .= (432 :: Int), "rotation" .= (0 :: Double), "polyline" .= BV.fromList [Coord 0 0, Coord 152 88, Coord 136 (-128), Coord 80 (-280), Coord 16 (-288)]])

    testJSON "LayerMid TileLayerMid"
        "{\"data\":[1,2,1,2,3,1,3,1,2,2,3,3,4,4,4,1],\"height\":8,\"name\":\"ground\",\"opacity\":1,\"properties\":[{\"name\":\"tileLayerProp\",\"type\":\"string\",\"value\":\"1\"}],\"type\":\"tilelayer\",\"visible\":true,\"width\":4,\"x\":0,\"y\":0}"
        (LayerMidTileLayer (TileLayerMid (LayerCommon "ground" 1 True 0 0 (Properties (Map.singleton "tileLayerProp" (PropertyString "1")))) 4 8 (TileLayerDataCsv $ UV.fromList [1,2,1,2,3,1,3,1,2,2,3,3,4,4,4,1])))
        (DA.object ["type" .= ("tilelayer" :: Text), "width" .= (4 :: Int), "height" .= (8 :: Int), "name" .= ("ground" :: Text), "opacity" .= (1 :: Double), "visible" .= True, "x" .= (0 :: Int), "y" .= (0 :: Int), "properties" .= [DA.object ["name" .= ("tileLayerProp" :: Text), "type" .= ("string" :: Text), "value" .= ("1" :: Text)]], "data" .= ([1,2,1,2,3,1,3,1,2,2,3,3,4,4,4,1] :: [Int])])

    testJSON "LayerMid ImageLayer"
        "{\"image\":\"buch-outdoor.png\",\"name\":\"image01\",\"opacity\":1,\"type\":\"imagelayer\",\"visible\":true,\"x\":295,\"y\":249,\"offsetx\":0,\"offsety\":0}"
        (LayerMidImageLayer (ImageLayer (LayerCommon "image01" 1 True 295 249 (Properties Map.empty)) (Image "buch-outdoor.png" Nothing Nothing) 0 0))
        (DA.object ["type" .= ("imagelayer" :: Text), "name" .= ("image01" :: Text), "opacity" .= (1 :: Double), "visible" .= True, "x" .= (295 :: Int), "y" .= (249 :: Int), "properties" .= ([] :: [DA.Value]), "image" .= ("buch-outdoor.png" :: Text), "offsetx" .= (0 :: Double), "offsety" .= (0 :: Double)])

    testJSON "Layer ObjectGroup"
        "{\"draworder\":\"topdown\",\"name\":\"objects\",\"objects\":[{\"height\":161,\"id\":1,\"name\":\"rect1\",\"properties\":[],\"rotation\":0,\"type\":\"shape\",\"visible\":true,\"width\":136,\"x\":38,\"y\":39},{\"ellipse\":true,\"height\":212,\"id\":2,\"name\":\"ellipse1\",\"properties\":[],\"rotation\":0,\"type\":\"\",\"visible\":true,\"width\":384,\"x\":81,\"y\":100}],\"opacity\":1,\"type\":\"objectgroup\",\"visible\":true,\"x\":0,\"y\":0}"
        (LayerMidObjectGroup (ObjectGroup (LayerCommon "objects" 1 True 0 0 (Properties Map.empty)) DrawOrderTopDown (BV.fromList [ObjectRectangle (Rectangle (ObjectCommon 1 "shape" 136 161 "rect1" (Properties Map.empty) True 38 39 0)), ObjectEllipse (Ellipse (ObjectCommon 2 "" 384 212 "ellipse1" (Properties Map.empty) True 81 100 0))])))
        (DA.object ["type" .= ("objectgroup" :: Text), "draworder" .= DrawOrderTopDown, "name" .= ("objects" :: Text), "opacity" .= (1 :: Double), "x" .= (0 :: Int), "y" .= (0 :: Int), "properties" .= ([] :: [DA.Value]), "visible" .= True, "objects" .= [ObjectRectangle (Rectangle (ObjectCommon 1 "shape" 136 161 "rect1" (Properties Map.empty) True 38 39 0)), ObjectEllipse (Ellipse (ObjectCommon 2 "" 384 212 "ellipse1" (Properties Map.empty) True 81 100 0))]])

    testJSON "MapMid"
        "{\"height\":100,\"layers\":[{\"data\":[1,2,1,2,3,1,3,1,2,2,3,3,4,4,4,1],\"height\":8,\"name\":\"ground\",\"opacity\":1,\"properties\":[{\"name\":\"tileLayerProp\",\"type\":\"string\",\"value\":\"1\"}],\"type\":\"tilelayer\",\"visible\":true,\"width\":4,\"x\":0,\"y\":0}],\"nextlayerid\":2,\"nextobjectid\":17,\"orientation\":\"orthogonal\",\"properties\":[],\"renderorder\":\"right-down\",\"tileheight\":32,\"tilesets\":[],\"tilewidth\":32,\"version\":1,\"tiledversion\":\"1.2\",\"width\":100}"
        (MapMid 1 "1.2" OrientationOrthogonal RenderOrderRightDown 100 100 32 32 0 Nothing Nothing Nothing 2 17
            (BV.fromList [LayerMidTileLayer (TileLayerMid (LayerCommon "ground" 1 True 0 0 (Properties (Map.singleton "tileLayerProp" (PropertyString "1")))) 4 8 (TileLayerDataCsv $ UV.fromList [1,2,1,2,3,1,3,1,2,2,3,3,4,4,4,1]))])
            (BV.fromList [])
            (Properties Map.empty))
        (DA.object
            [ "version" .= (1 :: Double)
            , "tiledversion" .= ("1.2" :: Text)
            , "orientation" .= OrientationOrthogonal
            , "renderorder" .= ("right-down" :: Text)
            , "width" .= (100 :: Int)
            , "height" .= (100 :: Int)
            , "tilewidth" .= (32 :: Int)
            , "tileheight" .= (32 :: Int)
            , "hexsidelength" .= DA.Null
            , "staggeraxis" .= DA.Null
            , "staggerindex" .= DA.Null
            , "nextlayerid" .= (2 :: Int)
            , "nextobjectid" .= (17 :: Int)
            , "backgroundcolor" .= (Nothing :: Maybe Color)
            , "layers" .= [LayerMidTileLayer (TileLayerMid (LayerCommon "ground" 1 True 0 0 (Properties (Map.singleton "tileLayerProp" (PropertyString "1")))) 4 8 (TileLayerDataCsv $ UV.fromList [1,2,1,2,3,1,3,1,2,2,3,3,4,4,4,1]))]
            , "tilesets" .= ([] :: [DA.Value])
            , "properties" .= ([] :: [DA.Value])])

testJSON :: (DA.FromJSON a, DA.ToJSON a, Show a, Eq a) => String -> ByteString -> a -> DA.Value -> Hspec.Spec
testJSON name str x json = do
    Hspec.it (name ++ ": decode") $ do
        let result = DA.decode $ str
        let expected = Just x
        result `Hspec.shouldBe` expected

    Hspec.it (name ++ ": encode") $ do
        let result = DA.toJSON x
        let expected = json
        result `Hspec.shouldBe` expected
