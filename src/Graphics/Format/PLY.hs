{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Graphics.Format.PLY
    ( createGeometryFromPLY
    , loadGeometryFromFile
    , loadPLYFile
    ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import qualified Control.Monad.Trans.State.Strict as State (get, put)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec (IResult(..),
                                                                 Parser,
                                                                 decimal,
                                                                 double, parse,
                                                                 rational)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (breakSubstring, empty, isPrefixOf,
                                        readFile, stripPrefix)
import qualified Data.ByteString.Char8 as BS (dropWhile, readInt, split, unpack)
import qualified Data.Char as Char (isSpace)
import Data.Int
import qualified Data.List as List (filter, span)
import Data.Maybe (fromMaybe, isJust, maybe)
import qualified Data.Serialize as Serialize
import Data.Vector (Vector)
import qualified Data.Vector as Vector (concatMap, find, findIndex, foldM',
                                        foldl', fromList, length, map, mapM,
                                        replicateM, zip, (!))
import qualified Data.Vector.Storable as SV (Vector, freeze, generate,
                                             generateM, length, (!))
import qualified Data.Vector.Storable.Mutable as MSV (STVector, new, write)
import qualified Data.Vector.Unboxed as UV (generate, mapM_)
import Data.Word
import qualified Graphics.GL as GL
import Graphics.Hree.Geometry (addVerticesToGeometry, newGeometry,
                               setIndexBuffer)
import Graphics.Hree.GL.Vertex (BasicVertex(..))
import Graphics.Hree.Scene (addIndexBufferUInt)
import Graphics.Hree.Types (Geometry, Scene)
import Linear (Additive(..), V2(..), V3(..), V4(..))
import qualified Linear (cross, normalize)

type Parser = StateT ByteString (Either String)

data PLY = PLY
    { plyHeader  :: !PLYHeader
    , plyBuffers :: !(Vector Buffer)
    } deriving (Show, Eq)

data PLYHeader = PLYHeader
    { phFormat         :: !PLYFormat
    , phElementHeaders :: !(Vector ElementHeader)
    } deriving (Show, Eq)

newtype Buffer = Buffer
    { unBuffer :: Vector (Vector Value)
    } deriving (Show, Eq)

data PLYFormat =
    PLYFormatAscii |
    PLYFormatBinaryLE |
    PLYFormatBinaryBE
    deriving (Show, Eq)

data ElementHeader = ElementHeader
    { ehName       :: !ByteString
    , ehNum        :: !Int
    , ehProperties :: !(Vector ElementProperty)
    } deriving (Show, Eq)

data ElementProperty =
    EPScalar !ScalarProperty |
    EPList !ListProperty
    deriving (Show, Eq)

data ScalarProperty = ScalarProperty
    { spName         :: !ByteString
    , spPropertyType :: !DataType
    } deriving (Show, Eq)

data ListProperty = ListProperty
    { lpName         :: !ByteString
    , lpSizeType     :: !DataType
    , lpPropertyType :: !DataType
    } deriving (Show, Eq)

data DataType =
    Char' |
    UChar' |
    Short' |
    UShort' |
    Int' |
    UInt' |
    Float' |
    Double'
    deriving (Show, Eq)

data Scalar =
    ScalarChar !Int8 |
    ScalarUChar !Word8 |
    ScalarShort !Int16 |
    ScalarUShort !Word16 |
    ScalarInt !Int32 |
    ScalarUInt !Word32 |
    ScalarFloat !Float |
    ScalarDouble !Double
    deriving (Show, Eq)

data Value =
    ValueS !Scalar |
    ValueL !(Vector Scalar)
    deriving (Show, Eq)

getScalarLE :: DataType -> Serialize.Get Scalar
getScalarLE Char'   = ScalarChar <$> Serialize.getInt8
getScalarLE UChar'  = ScalarUChar <$> Serialize.getWord8
getScalarLE Short'  = ScalarShort <$> Serialize.getInt16le
getScalarLE UShort' = ScalarUShort <$> Serialize.getWord16le
getScalarLE Int'    = ScalarInt <$> Serialize.getInt32le
getScalarLE UInt'   = ScalarUInt <$> Serialize.getWord32le
getScalarLE Float'  = ScalarFloat <$> Serialize.getFloat32le
getScalarLE Double' = ScalarDouble <$> Serialize.getFloat64le

getScalarBE :: DataType -> Serialize.Get Scalar
getScalarBE Char'   = ScalarChar <$> Serialize.getInt8
getScalarBE UChar'  = ScalarUChar <$> Serialize.getWord8
getScalarBE Short'  = ScalarShort <$> Serialize.getInt16be
getScalarBE UShort' = ScalarUShort <$> Serialize.getWord16be
getScalarBE Int'    = ScalarInt <$> Serialize.getInt32be
getScalarBE UInt'   = ScalarUInt <$> Serialize.getWord32be
getScalarBE Float'  = ScalarFloat <$> Serialize.getFloat32be
getScalarBE Double' = ScalarDouble <$> Serialize.getFloat64be

scalarParserAscii :: DataType -> Parser Scalar
scalarParserAscii Char'   = decimalParserAscii ScalarChar
scalarParserAscii UChar'  = decimalParserAscii ScalarUChar
scalarParserAscii Short'  = decimalParserAscii ScalarShort
scalarParserAscii UShort' = decimalParserAscii ScalarUShort
scalarParserAscii Int'    = decimalParserAscii ScalarInt
scalarParserAscii UInt'   = decimalParserAscii ScalarUInt
scalarParserAscii Float'  = ScalarFloat <$> convertParser Attoparsec.rational
scalarParserAscii Double' = ScalarDouble <$> convertParser Attoparsec.double

scalarParser :: PLYFormat -> DataType -> Parser Scalar
scalarParser PLYFormatBinaryLE t = scalarParserLE t
scalarParser PLYFormatBinaryBE t = scalarParserBE t
scalarParser PLYFormatAscii t    = scalarParserAscii t

scalarParserLE :: DataType -> Parser Scalar
scalarParserLE = binaryParser . getScalarLE

scalarParserBE :: DataType -> Parser Scalar
scalarParserBE = binaryParser . getScalarBE

scalarListParser :: PLYFormat -> DataType -> DataType -> Parser (Vector Scalar)
scalarListParser format st dt = do
    s <- scalarParser format st
    n <- lift $ castScalarFromIntegral s
    Vector.replicateM n (scalarParser format dt)

decimalParserAscii :: (Integral a) => (a -> Scalar) -> Parser Scalar
decimalParserAscii f = f <$> convertParser Attoparsec.decimal

convertParser :: Attoparsec.Parser a -> Parser a
convertParser a = do
    bs <- State.get
    (r, rest) <- handleResult $ Attoparsec.parse a bs
    State.put (BS.dropWhile Char.isSpace rest)
    return r
    where
    handleResult (Attoparsec.Done bs r)    = lift . return $ (r, bs)
    handleResult (Attoparsec.Fail _ _ err) = lift . Left $ err
    handleResult _                         = lift . Left $ "parse error"

binaryParser :: Serialize.Get a -> Parser a
binaryParser a = do
    bs <- State.get
    (r, rest) <- lift $ Serialize.runGetState a bs 0
    State.put rest
    return r

class FromScalar a where
    fromScalar :: Scalar -> Either String a

instance FromScalar Int8 where
    fromScalar (ScalarChar a) = Right a
    fromScalar _              = Left "fromScalar failed"

instance FromScalar Word8 where
    fromScalar (ScalarUChar a) = Right a
    fromScalar _               = Left "fromScalar failed"

instance FromScalar Int16 where
    fromScalar (ScalarShort a) = Right a
    fromScalar _               = Left "fromScalar failed"

instance FromScalar Word16 where
    fromScalar (ScalarUShort a) = Right a
    fromScalar _                = Left "fromScalar failed"

instance FromScalar Int32 where
    fromScalar (ScalarInt a) = Right a
    fromScalar _             = Left "fromScalar failed"

instance FromScalar Word32 where
    fromScalar (ScalarUInt a) = Right a
    fromScalar _              = Left "fromScalar failed"

instance FromScalar Float where
    fromScalar (ScalarFloat a) = Right a
    fromScalar _               = Left "fromScalar failed"

instance FromScalar Double where
    fromScalar (ScalarDouble a) = Right a
    fromScalar _                = Left "fromScalar failed"

castScalarFromIntegral :: (Integral a) => Scalar -> Either String a
castScalarFromIntegral (ScalarChar a)   = Right $ fromIntegral a
castScalarFromIntegral (ScalarUChar a)  = Right $ fromIntegral a
castScalarFromIntegral (ScalarShort a)  = Right $ fromIntegral a
castScalarFromIntegral (ScalarUShort a) = Right $ fromIntegral a
castScalarFromIntegral (ScalarInt a)    = Right $ fromIntegral a
castScalarFromIntegral (ScalarUInt a)   = Right $ fromIntegral a
castScalarFromIntegral (ScalarFloat _)  = Left "castScalarToInt failed"
castScalarFromIntegral (ScalarDouble _) = Left "castScalarToInt failed"

headerParser :: Parser PLYHeader
headerParser = do
    bs <- State.get
    let (hbs, rest) = BS.breakSubstring headerEnd bs
    when (rest == BS.empty) $
        lift (Left "\"end_header\" not found")

    xs <-lift . parseStart . removeComments . BS.split '\n' $ hbs
    (format, xs') <- lift $ parseFormat xs
    ehs <- lift $ parseElementHeaders xs'

    State.put $ fromMaybe rest (BS.stripPrefix headerEnd rest)

    return $ PLYHeader format (Vector.fromList ehs)

    where
    headerEnd = "\nend_header\n"

    removeComments = List.filter (not . BS.isPrefixOf "comment")

    parseStart [] = Left "first line should be \"ply\""
    parseStart (ply : rest) = do
        when (ply /= "ply") $
            Left "first line should be \"ply\""
        return rest

    parseFormat [] = Left "no format line"
    parseFormat (line : rest) =
        case line of
            "format ascii 1.0" -> return (PLYFormatAscii, rest)
            "format binary_little_endian 1.0" -> return (PLYFormatBinaryLE, rest)
            "format binary_big_endian 1.0" -> return (PLYFormatBinaryBE, rest)
            _ -> Left ("error at format line. content: \"" ++ show line ++ "\"")

    parseElementHeader [] = Left "no element"
    parseElementHeader (line : xs) = do
        let ws = BS.split ' ' line

        (h, num') <- case ws of
                h : _ : num' : _  -> return (h, num')
                _ -> Left ("element parse error. content: \"" ++ BS.unpack line ++ "\"")

        when (h /= "element") $
            Left "element part should be starts with \"element\""

        (num, _) <- maybe (Left "parse failed on element num") return $ BS.readInt num'

        let (propertyLines, rest) = List.span (BS.isPrefixOf "property") xs
        properties <- mapM parseProperty propertyLines

        let name = ws !! 1
            eh = ElementHeader name num (Vector.fromList properties)
        return (eh, rest)

    parseElementHeaders [] = return []
    parseElementHeaders xs = do
        (eh, rest) <- parseElementHeader xs
        (eh :) <$> parseElementHeaders rest

    parseProperty line = parseProperty' (BS.split ' ' line)

    parseProperty' ["property", "list", st', pt', name] = do
        st <- parseDataType st'
        pt <- parseDataType pt'
        return . EPList $ ListProperty name st pt

    parseProperty' ["property", pt', name] =
        EPScalar . ScalarProperty name <$> parseDataType pt'

    parseProperty' _ = Left "property parse failed"

    parseDataType "char"   = return Char'
    parseDataType "uchar"  = return UChar'
    parseDataType "short"  = return Short'
    parseDataType "ushort" = return UShort'
    parseDataType "int"    = return Int'
    parseDataType "uint"   = return UInt'
    parseDataType "int8"   = return Char'
    parseDataType "uint8"  = return UChar'
    parseDataType "int16"  = return Short'
    parseDataType "uint16" = return UShort'
    parseDataType "int32"  = return Int'
    parseDataType "uint32" = return UInt'
    parseDataType "float"  = return Float'
    parseDataType "double" = return Double'
    parseDataType _        = Left "parse data type failed"

plyParser :: Parser PLY
plyParser = do
    header <- headerParser
    let elems = phElementHeaders header
        format = phFormat header
    buffers <- Vector.mapM (elementBufferParser format) elems
    return (PLY header buffers)

elementBufferParser :: PLYFormat -> ElementHeader -> Parser Buffer
elementBufferParser format eh =
    Buffer <$> Vector.replicateM num (Vector.mapM (propertyParser format) properties)
    where
    num = ehNum eh
    properties = ehProperties eh

propertyParser :: PLYFormat -> ElementProperty -> Parser Value
propertyParser format (EPScalar (ScalarProperty _ t)) =
    ValueS <$> scalarParser format t
propertyParser format (EPList (ListProperty _ st dt)) =
    ValueL <$> scalarListParser format st dt

loadPLYFile :: FilePath -> IO PLY
loadPLYFile path = do
    bs <- BS.readFile path
    either (throwIO . userError) return $ evalStateT plyParser bs

loadGeometryFromFile :: FilePath -> Scene -> IO Geometry
loadGeometryFromFile path scene = do
    ply <- loadPLYFile path
    createGeometryFromPLY ply scene

createGeometryFromPLY :: PLY -> Scene -> IO Geometry
createGeometryFromPLY (PLY (PLYHeader _ ehs) buffers) scene = do
    vertexElemIndex <- maybe (throwIO . userError $ "vertex element not found") return
        $ Vector.findIndex ((== "vertex") . ehName) ehs
    let Buffer vertexBuffer = buffers Vector.! vertexElemIndex
        vertexHeader = ehs Vector.! vertexElemIndex
        vprops = ehProperties vertexHeader

    vs <- either (throwIO . userError) return $
        SV.generateM (Vector.length vertexBuffer) (\i -> toBasicVertex vprops (vertexBuffer Vector.! i))

    let faceElemIndex = Vector.findIndex ((== "face") . ehName) ehs
        maybeIndices = flip fmap faceElemIndex $ \i ->
            let Buffer indexBuffer = buffers Vector.! i
                indexHeader = ehs Vector.! i
                xs = Vector.map (toIndices (ehProperties indexHeader)) indexBuffer
                indices = Vector.concatMap id xs
            in SV.generate (Vector.length indices) (indices Vector.!)

    let (vs', maybeIndices') =
            if not (hasNormalProps vprops)
                then onJust maybeIndices (vs, maybeIndices) $ calculateNormals vs
                else (vs, maybeIndices)

    geo <- addVerticesToGeometry newGeometry vs' GL.GL_STATIC_READ scene

    onJust maybeIndices' (return geo) $
        fmap (setIndexBuffer geo) . addIndexBufferUInt scene

    where
    onJust (Just a) _ f = f a
    onJust  _ b _       = b

    isNormalProperty name (EPScalar (ScalarProperty n Float')) | n == name = True
                                                               | otherwise = False
    isNormalProperty _ _ = False

    hasNormalProps props =
        isJust (Vector.find (isNormalProperty "nx") props) && isJust (Vector.find (isNormalProperty "ny") props) && isJust (Vector.find (isNormalProperty "nz") props)

toBasicVertex :: Vector ElementProperty -> Vector Value -> Either String BasicVertex
toBasicVertex properties vs =
    let vertex = BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 255 255 255 255)
    in Vector.foldM' handleVertexProperty vertex (Vector.zip properties vs)

handleVertexProperty :: BasicVertex -> (ElementProperty, Value) -> Either String BasicVertex
handleVertexProperty v (EPScalar (ScalarProperty "x" _), ValueS x) = do
    p0 <- fromScalar x
    let V3 _ p1 p2 = bvPosition v
        v' = v { bvPosition = V3 p0 p1 p2 }
    return v'
handleVertexProperty v (EPScalar (ScalarProperty "y" _), ValueS x) = do
    p1 <- fromScalar x
    let V3 p0 _ p2 = bvPosition v
        v' = v { bvPosition = V3 p0 p1 p2 }
    return v'
handleVertexProperty v (EPScalar (ScalarProperty "z" _), ValueS x) = do
    p2 <- fromScalar x
    let V3 p0 p1 _ = bvPosition v
        v' = v { bvPosition = V3 p0 p1 p2 }
    return v'
handleVertexProperty v (EPScalar (ScalarProperty "nx" _), ValueS x) = do
    n0 <- fromScalar x
    let V3 _ n1 n2 = bvNormal v
        v' = v { bvNormal = V3 n0 n1 n2 }
    return v'
handleVertexProperty v (EPScalar (ScalarProperty "ny" _), ValueS x) = do
    n1 <- fromScalar x
    let V3 n0 _ n2 = bvNormal v
        v' = v { bvNormal = V3 n0 n1 n2 }
    return v'
handleVertexProperty v (EPScalar (ScalarProperty "nz" _), ValueS x) = do
    n2 <- fromScalar x
    let V3 n0 n1 _ = bvNormal v
        v' = v { bvNormal = V3 n0 n1 n2 }
    return v'
handleVertexProperty v (EPScalar (ScalarProperty "s" _), ValueS x) = do
    s <- fromScalar x
    let V2 _ t = bvUv v
        v' = v { bvUv = V2 s t }
    return v'
handleVertexProperty v (EPScalar (ScalarProperty "t" _), ValueS x) = do
    t <- fromScalar x
    let V2 s _ = bvUv v
        v' = v { bvUv = V2 s t }
    return v'
handleVertexProperty v (EPScalar (ScalarProperty "red" _), ValueS x) = do
    r <- fromScalar x
    let V4 _ g b a = bvColor v
        v' = v { bvColor = V4 r g b a }
    return v'
handleVertexProperty v (EPScalar (ScalarProperty "green" _), ValueS x) = do
    g <- fromScalar x
    let V4 r _ b a = bvColor v
        v' = v { bvColor = V4 r g b a }
    return v'
handleVertexProperty v (EPScalar (ScalarProperty "blue" _), ValueS x) = do
    b <- fromScalar x
    let V4 r g _ a = bvColor v
        v' = v { bvColor = V4 r g b a }
    return v'
handleVertexProperty v _ = return v

toIndices :: Vector ElementProperty -> Vector Value -> Vector Word32
toIndices properties vs = Vector.foldl' handleVertexIndicesProperty mempty (Vector.zip properties vs)

handleVertexIndicesProperty :: Vector Word32 -> (ElementProperty, Value) -> Vector Word32
handleVertexIndicesProperty indices (EPList (ListProperty "vertex_indices" _ _), ValueL xs) =
    let xs' = case Vector.length xs of
                3 -> xs
                4 -> Vector.fromList
                    [ xs Vector.! 0
                    , xs Vector.! 1
                    , xs Vector.! 2
                    , xs Vector.! 2
                    , xs Vector.! 3
                    , xs Vector.! 0
                    ]
                _ -> mempty
        r = Vector.mapM castScalarFromIntegral xs'
    in either (const indices) id r
handleVertexIndicesProperty indices _ = indices

calculateNormals :: SV.Vector BasicVertex -> SV.Vector Word32 -> (SV.Vector BasicVertex, Maybe (SV.Vector Word32))
calculateNormals vertices indices = (go, Nothing)
    where
    inum = SV.length indices
    faces = UV.generate (inum `div` 3) (* 3)
    go = runST $ do
        vs <- MSV.new inum
        UV.mapM_ (calcNormals vs) faces
        SV.freeze vs

    calcNormals :: MSV.STVector s BasicVertex -> Int -> ST s ()
    calcNormals vs face = do
        let i0 = indices SV.! face
            i1 = indices SV.! (face + 1)
            i2 = indices SV.! (face + 2)
            a0 = vertices SV.! fromIntegral i0
            a1 = vertices SV.! fromIntegral i1
            a2 = vertices SV.! fromIntegral i2
            p0 = bvPosition a0
            p1 = bvPosition a1
            p2 = bvPosition a2
            v0 = p1 ^-^ p0
            v1 = p2 ^-^ p0
            n = Linear.normalize $ Linear.cross v0 v1

        MSV.write vs face a0 { bvNormal = n }
        MSV.write vs (face + 1) a1 { bvNormal = n }
        MSV.write vs (face + 2) a2 { bvNormal = n }
