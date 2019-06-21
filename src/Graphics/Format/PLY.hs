{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Graphics.Format.PLY
    ( parsePLYHeader
    ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (breakSubstring, drop, empty, isPrefixOf,
                                        length, readFile, stripPrefix)
import qualified Data.ByteString.Char8 as BS (break, dropWhile, readInt, split,
                                              unpack)
import qualified Data.Char as Char (isSpace)
import Data.Foldable (foldlM)
import Data.Int
import qualified Data.List as List (filter, span)
import Data.Maybe (fromMaybe, maybe)
import Data.Proxy (Proxy(..))
import qualified Data.Serialize as Serialize
import Data.Word
import Foreign (sizeOf)
import Graphics.Hree.GL.Vertex (BasicVertex(..))
import Linear (V2(..), V3(..), V4(..))
import Text.Read (readEither)

data PLYHeader = PLYHeader
    { phFormat         :: !PLYFormat
    , phElementHeaders :: ![ElementHeader]
    } deriving (Show, Eq)

data PLYFormat =
    PLYFormatAscii |
    PLYFormatBinaryLE |
    PLYFormatBinaryBE
    deriving (Show, Eq)

data ElementHeader = ElementHeader
    { ehName       :: !ByteString
    , ehNum        :: !Int
    , ehProperties :: ![ElementProperty]
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
    ScalarChar Int8 |
    ScalarUChar Word8 |
    ScalarShort Int16 |
    ScalarUShort Word16 |
    ScalarInt Int32 |
    ScalarUInt Word32 |
    ScalarFloat Float |
    ScalarDouble Double
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

parseAscii :: DataType -> ByteString -> Either String (Scalar, ByteString)
parseAscii Char' bs   = parseAsciiToIntegral ScalarChar bs
parseAscii UChar' bs  = parseAsciiToIntegral ScalarUChar bs
parseAscii Short' bs  = parseAsciiToIntegral ScalarShort bs
parseAscii UShort' bs = parseAsciiToIntegral ScalarUShort bs
parseAscii Int' bs    = parseAsciiToIntegral ScalarInt bs
parseAscii UInt' bs   = parseAsciiToIntegral ScalarUInt bs
parseAscii Float' bs  = parseAsciiRead ScalarFloat bs
parseAscii Double' bs = parseAsciiRead ScalarDouble bs

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

castScalarToFrac :: (Num a, Fractional a) => Scalar -> a
castScalarToFrac (ScalarChar a)   = fromIntegral a
castScalarToFrac (ScalarUChar a)  = fromIntegral a
castScalarToFrac (ScalarShort a)  = fromIntegral a
castScalarToFrac (ScalarUShort a) = fromIntegral a
castScalarToFrac (ScalarInt a)    = fromIntegral a
castScalarToFrac (ScalarUInt a)   = fromIntegral a
castScalarToFrac (ScalarFloat a)  = realToFrac a
castScalarToFrac (ScalarDouble a) = realToFrac a

castScalarToInt :: (Integral a) => Scalar -> Either String a
castScalarToInt (ScalarChar a)   = Right $ fromIntegral a
castScalarToInt (ScalarUChar a)  = Right $ fromIntegral a
castScalarToInt (ScalarShort a)  = Right $ fromIntegral a
castScalarToInt (ScalarUShort a) = Right $ fromIntegral a
castScalarToInt (ScalarInt a)    = Right $ fromIntegral a
castScalarToInt (ScalarUInt a)   = Right $ fromIntegral a
castScalarToInt (ScalarFloat a)  = Left "castScalarToInt failed"
castScalarToInt (ScalarDouble a) = Left "castScalarToInt failed"

parseAsciiToIntegral :: (Integral a) => (a -> Scalar) -> ByteString -> Either String (Scalar, ByteString)
parseAsciiToIntegral f bs = do
    (a, rest) <- maybe (Left "readInt failed") return (BS.readInt bs)
    return (f (fromIntegral a), BS.dropWhile Char.isSpace rest)

parseAsciiRead :: (Read a) => (a -> Scalar) -> ByteString -> Either String (Scalar, ByteString)
parseAsciiRead f bs = do
    let (token, rest) = BS.break Char.isSpace bs
    a <- readEither . BS.unpack $ token
    return (f a, BS.dropWhile Char.isSpace rest)

parsePLYHeader :: ByteString -> Either String (PLYHeader, ByteString)
parsePLYHeader bs = do
    let (hbs, rest) = BS.breakSubstring headerEnd bs
    when (rest == BS.empty) $
        Left "\"end_header\" not found"

    let lines = removeComments . BS.split '\n' $ hbs

    lines <-parseStart lines
    (format, lines) <- parseFormat lines
    ehs <- parseElementHeaders lines

    let header = PLYHeader format ehs

    return (header, fromMaybe rest $ BS.stripPrefix headerEnd rest)

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
    parseElementHeader (line : lines) = do
        let ws = BS.split ' ' line
        when (length ws /= 3) $
            Left ("element parse error. content: \"" ++ BS.unpack line ++ "\"")

        let h : name : num' : _ = ws

        when (h /= "element") $
            Left "element part should be starts with \"element\""

        (num, _) <- maybe (Left "parse failed on element num") return $ BS.readInt num'

        let (propertyLines, rest) = List.span (BS.isPrefixOf "property") lines
        properties <- mapM parseProperty propertyLines

        let name = ws !! 1
            eh = ElementHeader name num properties
        return (eh, rest)

    parseElementHeaders [] = return []
    parseElementHeaders lines = do
        (eh, rest) <- parseElementHeader lines
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

parseScalar :: PLYFormat -> DataType -> ByteString -> Either String (Scalar, ByteString)
parseScalar PLYFormatBinaryLE t bs = parseScalarLE t bs
parseScalar PLYFormatBinaryBE t bs = parseScalarBE t bs
parseScalar PLYFormatAscii t bs    = parseAscii t bs

parseScalarLE :: DataType -> ByteString -> Either String (Scalar, ByteString)
parseScalarLE t bs =
    Serialize.runGetState (getScalarLE t) bs 0

parseScalarBE :: DataType -> ByteString -> Either String (Scalar, ByteString)
parseScalarBE t bs =
    Serialize.runGetState (getScalarBE t) bs 0

parseList :: PLYFormat -> DataType -> DataType -> ByteString -> Either String ([Scalar], ByteString)
parseList format dt st bs = do
    (s, rest) <- parseScalar format st bs
    n <- castScalarToInt s
    foldlM parseListElem ([], rest) [1..n]
    where
    parseListElem (es, xs) _ = do
        (e, xs') <- parseScalar format dt xs
        return (es ++ [e], xs')

parseVertex :: PLYFormat -> ByteString -> [ElementProperty] -> Either String (BasicVertex, ByteString)
parseVertex format bs properties =
    let vertex = BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 0 0 0 255)
    in foldlM (handleVertexProperty format) (vertex, bs) properties

fromScalar' :: (FromScalar a) => (Scalar, ByteString) -> Either String (a, ByteString)
fromScalar' (s, bs) = do
    a <- fromScalar s
    return (a, bs)

handleVertexProperty :: PLYFormat -> (BasicVertex, ByteString) -> ElementProperty -> Either String (BasicVertex, ByteString)
handleVertexProperty format (v, bs) (EPScalar (ScalarProperty "x" dataType)) = do
    (p0, rest) <- fromScalar' =<< parseScalar format dataType bs
    let V3 _ p1 p2 = bvPosition v
        v' = v { bvPosition = V3 p0 p1 p2 }
    return (v', rest)
handleVertexProperty format (v, bs) (EPScalar (ScalarProperty "y" dataType)) = do
    (p1, rest) <- fromScalar' =<< parseScalar format dataType bs
    let V3 p0 _ p2 = bvPosition v
        v' = v { bvPosition = V3 p0 p1 p2 }
    return (v', rest)
handleVertexProperty format (v, bs) (EPScalar (ScalarProperty "z" dataType)) = do
    (p2, rest) <- fromScalar' =<< parseScalar format dataType bs
    let V3 p0 p1 _ = bvPosition v
        v' = v { bvPosition = V3 p0 p1 p2 }
    return (v', rest)
handleVertexProperty format (v, bs) (EPScalar (ScalarProperty "nx" dataType)) = do
    (n0, rest) <- fromScalar' =<< parseScalar format dataType bs
    let V3 _ n1 n2 = bvNormal v
        v' = v { bvNormal = V3 n0 n1 n2 }
    return (v', rest)
handleVertexProperty format (v, bs) (EPScalar (ScalarProperty "ny" dataType)) = do
    (n1, rest) <- fromScalar' =<< parseScalar format dataType bs
    let V3 n0 _ n2 = bvNormal v
        v' = v { bvNormal = V3 n0 n1 n2 }
    return (v', rest)
handleVertexProperty format (v, bs) (EPScalar (ScalarProperty "nz" dataType)) = do
    (n2, rest) <- fromScalar' =<< parseScalar format dataType bs
    let V3 n0 n1 _ = bvNormal v
        v' = v { bvNormal = V3 n0 n1 n2 }
    return (v', rest)
handleVertexProperty format (v, bs) (EPScalar (ScalarProperty "s" dataType)) = do
    (s, rest) <- fromScalar' =<< parseScalar format dataType bs
    let V2 _ t = bvUv v
        v' = v { bvUv = V2 s t }
    return (v', rest)
handleVertexProperty format (v, bs) (EPScalar (ScalarProperty "t" dataType)) = do
    (t, rest) <- fromScalar' =<< parseScalar format dataType bs
    let V2 s _ = bvUv v
        v' = v { bvUv = V2 s t }
    return (v', rest)
handleVertexProperty format (v, bs) (EPScalar (ScalarProperty "red" dataType)) = do
    (r, rest) <- fromScalar' =<< parseScalar format dataType bs
    let V4 _ g b a = bvColor v
        v' = v { bvColor = V4 r g b a }
    return (v', rest)
handleVertexProperty format (v, bs) (EPScalar (ScalarProperty "green" dataType)) = do
    (g, rest) <- fromScalar' =<< parseScalar format dataType bs
    let V4 r _ b a = bvColor v
        v' = v { bvColor = V4 r g b a }
    return (v', rest)
handleVertexProperty format (v, bs) (EPScalar (ScalarProperty "blue" dataType)) = do
    (b, rest) <- fromScalar' =<< parseScalar format dataType bs
    let V4 r g _ a = bvColor v
        v' = v { bvColor = V4 r g b a }
    return (v', rest)
handleVertexProperty format (v, bs) (EPScalar (ScalarProperty _ dataType)) = do
    (_, rest) <- parseScalar format dataType bs
    return (v, rest)
handleVertexProperty format (v, bs) (EPList (ListProperty _ st dt)) = do
    (_, rest) <- parseList format st dt bs
    return (v, rest)

parseVertexIndices :: PLYFormat -> ByteString -> [ElementProperty] -> Either String ([Int], ByteString)
parseVertexIndices format bs =
    foldlM (handleVertexIndicesProperty format) ([], bs)

handleVertexIndicesProperty :: PLYFormat -> ([Int], ByteString) -> ElementProperty -> Either String ([Int], ByteString)
handleVertexIndicesProperty format (xs, bs) (EPList (ListProperty "vertex_indices" st dt)) = do
    (ys, rest) <- parseList format st dt bs
    ys' <- mapM castScalarToInt ys
    return (xs ++ ys', rest)
handleVertexIndicesProperty format (xs, bs) (EPList (ListProperty _ st dt)) = do
    (_, rest) <- parseList format st dt bs
    return (xs, rest)
handleVertexIndicesProperty format (xs, bs) (EPScalar (ScalarProperty _ dataType)) = do
    (_, rest) <- parseScalar format dataType bs
    return (xs, rest)
