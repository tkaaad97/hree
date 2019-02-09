module Graphics.Hree.GL
    ( mkBuffer
    , mkVertexArray
    , renderMany
    ) where

import Control.Exception (throwIO)
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable (foldrM)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Vector.Storable as V
import qualified Foreign (Storable(..), alloca, allocaArray)
import qualified Graphics.GL as GLRaw
import Graphics.Hree.GL.Types
import qualified Graphics.Rendering.OpenGL as GL
import System.IO.Error (userError)
import Unsafe.Coerce (unsafeCoerce)

render :: [(ByteString, Uniform)] -> RenderInfo -> Maybe GL.Program -> IO (Maybe GL.Program)
render commons a cur = do
    setCurrentProgram cur program
    GL.bindVertexArrayObject GL.$= Just (riVertexArray a)
    bindUniforms uniforms
    drawWith method
    return . Just $ program
    where
    program = programInfoProgram . riProgram $ a
    uniformInfos = programInfoUniforms . riProgram $ a
    method = riDrawMethod a
    uniforms = riUniforms a
    setCurrentProgram (Just p0) p1 | p0 == p1 = return ()
    setCurrentProgram _ p = do
        bindUniforms . mapMaybe toUniformPair $ commons
        GL.currentProgram GL.$= Just p
    toUniformPair (k, u) =
        (,) <$> Map.lookup k uniformInfos <*> Just u

renderMany :: Foldable t => [(ByteString, Uniform)] -> t RenderInfo -> IO ()
renderMany common = void . foldrM (render common) Nothing

drawWith :: DrawMethod -> IO ()
drawWith (DrawArrays mode index num) = GL.drawArrays mode index num
drawWith (DrawElements mode num dataType indicesOffset) = GL.drawElements mode num dataType indicesOffset

mkBuffer :: GL.BufferTarget -> BufferSource -> IO GL.BufferObject
mkBuffer target (BufferSource vec usage) = do
    let n = V.length vec
        size = fromIntegral $ n * Foreign.sizeOf (V.head vec)
    putStrLn $ "n=" ++ show n
    putStrLn $ "size=" ++ show size
    buffer <- Foreign.allocaArray 2 $ \p -> GLRaw.glCreateBuffers 1 p *> Foreign.peek p
    putStrLn "CreateBuffers"
    V.unsafeWith vec $ \ptr -> GLRaw.glNamedBufferData buffer size ptr GLRaw.GL_STREAM_DRAW
    return (unsafeCoerce buffer)

mkVertexArray :: Map ByteString AttribBinding -> IntMap (GL.BufferObject, BindBufferSetting) -> Maybe GL.BufferObject -> ProgramInfo -> IO GL.VertexArrayObject
mkVertexArray attribBindings buffers indexBuffer programInfo =
    Foreign.allocaArray 2 $ \p -> do
        GL.currentProgram GL.$= Just program
        GLRaw.glCreateVertexArrays 1 p
        GL.errors >>= print
        vaoId <- Foreign.peek p
        mapM_ (setAttrib vaoId) (Map.toList attribBindings)
        mapM_ (setBindingBuffer vaoId) . IntMap.toList $ buffers
        setIndexBuffer vaoId (unsafeCoerce indexBuffer)
        GL.currentProgram GL.$= Nothing
        return (unsafeCoerce vaoId)

    where
    program = programInfoProgram programInfo

    attribInfos = programInfoAttribs programInfo

    setAttrib vaoId (k, a) = do
        let binding = attribBindingIndex a
        buffer <- maybe (throwIO . userError $ "binding buffer not found") return (IntMap.lookup binding buffers)
        location <- maybe (throwIO . userError $ "attrib not found") (return . aiAttribLocation) (Map.lookup k attribInfos)
        setVertexArrayAttribFormatAndBinding vaoId location a

    setBindingBuffer vaoId (i, (b, BindBufferSetting offset stride)) =
        GLRaw.glVertexArrayVertexBuffer vaoId (fromIntegral i) (unsafeCoerce b) (fromIntegral offset) (fromIntegral stride)

    setIndexBuffer vaoId (Just b) =
        GLRaw.glVertexArrayElementBuffer vaoId b

    setIndexBuffer _ Nothing = return ()


setVertexArrayAttribFormatAndBinding :: GL.GLuint -> GLRaw.GLuint -> AttribBinding -> IO ()
setVertexArrayAttribFormatAndBinding vaoId attribLocation (AttribBinding binding' format) = do
    GLRaw.glVertexArrayAttribBinding vaoId attribLocation binding
    GLRaw.glVertexArrayAttribFormat vaoId attribLocation formatSize formatDataType formatNormalized formatRelativeOffset
    GLRaw.glEnableVertexArrayAttrib vaoId attribLocation

    where
    binding = fromIntegral binding'
    AttribFormat fsize formatDataType fnormalized foffset = format
    formatSize = fromIntegral fsize
    formatNormalized = fromIntegral . fromEnum $ fnormalized
    formatRelativeOffset = fromIntegral foffset

bindUniforms :: [(UniformInfo, Uniform)] -> IO ()
bindUniforms = mapM_ bindUniform
    where
    bindUniform (ui, Uniform a) =
        GL.uniform (unsafeCoerce $ uiUniformLocation ui) GL.$= a

-- from Graphics.Rendering.OpenGL.GL.DataType
marshalDataType :: GL.DataType -> GLRaw.GLenum
marshalDataType x = case x of
   GL.UnsignedByte             -> GLRaw.GL_UNSIGNED_BYTE
   GL.Byte                     -> GLRaw.GL_BYTE
   GL.UnsignedShort            -> GLRaw.GL_UNSIGNED_SHORT
   GL.Short                    -> GLRaw.GL_SHORT
   GL.UnsignedInt              -> GLRaw.GL_UNSIGNED_INT
   GL.Int                      -> GLRaw.GL_INT
   GL.HalfFloat                -> GLRaw.GL_HALF_FLOAT
   GL.Float                    -> GLRaw.GL_FLOAT
   GL.UnsignedByte332          -> GLRaw.GL_UNSIGNED_BYTE_3_3_2
   GL.UnsignedByte233Rev       -> GLRaw.GL_UNSIGNED_BYTE_2_3_3_REV
   GL.UnsignedShort565         -> GLRaw.GL_UNSIGNED_SHORT_5_6_5
   GL.UnsignedShort565Rev      -> GLRaw.GL_UNSIGNED_SHORT_5_6_5_REV
   GL.UnsignedShort4444        -> GLRaw.GL_UNSIGNED_SHORT_4_4_4_4
   GL.UnsignedShort4444Rev     -> GLRaw.GL_UNSIGNED_SHORT_4_4_4_4_REV
   GL.UnsignedShort5551        -> GLRaw.GL_UNSIGNED_SHORT_5_5_5_1
   GL.UnsignedShort1555Rev     -> GLRaw.GL_UNSIGNED_SHORT_1_5_5_5_REV
   GL.UnsignedInt8888          -> GLRaw.GL_UNSIGNED_INT_8_8_8_8
   GL.UnsignedInt8888Rev       -> GLRaw.GL_UNSIGNED_INT_8_8_8_8_REV
   GL.UnsignedInt1010102       -> GLRaw.GL_UNSIGNED_INT_10_10_10_2
   GL.UnsignedInt2101010Rev    -> GLRaw.GL_UNSIGNED_INT_2_10_10_10_REV
   GL.UnsignedInt248           -> GLRaw.GL_UNSIGNED_INT_24_8
   GL.UnsignedInt10f11f11fRev  -> GLRaw.GL_UNSIGNED_INT_10F_11F_11F_REV
   GL.UnsignedInt5999Rev       -> GLRaw.GL_UNSIGNED_INT_5_9_9_9_REV
   GL.Float32UnsignedInt248Rev -> GLRaw.GL_FLOAT_32_UNSIGNED_INT_24_8_REV
   GL.Bitmap                   -> GLRaw.GL_BITMAP
   GL.UnsignedShort88          -> GLRaw.GL_UNSIGNED_SHORT_8_8_MESA
   GL.UnsignedShort88Rev       -> GLRaw.GL_UNSIGNED_SHORT_8_8_REV_MESA
   GL.Double                   -> GLRaw.GL_DOUBLE
   GL.TwoBytes                 -> GLRaw.GL_2_BYTES
   GL.ThreeBytes               -> GLRaw.GL_3_BYTES
   GL.FourBytes                -> GLRaw.GL_4_BYTES
