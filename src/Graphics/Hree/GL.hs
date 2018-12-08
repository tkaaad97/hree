module Graphics.Hree.GL
    ( mkBuffer
    , mkVertexArray
    , render
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
import qualified Data.Vector.Storable as V
import qualified Foreign (Storable(..), alloca)
import qualified Graphics.GL as GLRaw
import Graphics.Hree.GL.Types
import qualified Graphics.Rendering.OpenGL as GL
import System.IO.Error (userError)
import Unsafe.Coerce (unsafeCoerce)

render :: RenderInfo -> Maybe GL.Program -> IO (Maybe GL.Program)
render a cur = do
    setCurrentProgram cur program
    GL.bindVertexArrayObject GL.$= Just (riVertexArray a)
    bindUniforms uniforms
    drawWith method
    return . Just $ program
    where
    program = programInfoProgram . riProgram $ a
    method = riDrawMethod a
    uniforms = riUniforms a
    setCurrentProgram (Just p0) p1 | p0 == p1 = return ()
    setCurrentProgram _ p          = GL.currentProgram GL.$= Just p

renderMany :: Foldable t => t RenderInfo -> IO ()
renderMany = void . foldrM render Nothing

drawWith :: DrawMethod -> IO ()
drawWith (DrawArrays mode index num) = GL.drawArrays mode index num
drawWith (DrawElements mode num dataType indicesOffset) = GL.drawElements mode num dataType indicesOffset

mkBuffer :: GL.BufferTarget -> BufferSource -> IO GL.BufferObject
mkBuffer target (BufferSource vec usage) = do
    let n = V.length vec
        size = fromIntegral $ n * Foreign.sizeOf (V.head vec)
    buffer <- GL.genObjectName
    GL.bindBuffer target GL.$= Just buffer
    V.unsafeWith vec $ \ptr -> GL.bufferData target GL.$= (size, ptr, usage)
    GL.bindBuffer target GL.$= Nothing
    return buffer

mkVertexArray :: Map ByteString AttribBinding -> IntMap GL.BufferObject -> Maybe GL.BufferObject -> ProgramInfo -> IO GL.VertexArrayObject
mkVertexArray attribBindings buffers indexBuffer programInfo =
    Foreign.alloca $ \p -> do
        GLRaw.glCreateVertexArrays 1 p
        vaoRaw <- Foreign.peek p
        let vao = unsafeCoerce vaoRaw
        setIndexBuffer vaoRaw (unsafeCoerce indexBuffer)
        mapM_ (setAttrib vao) (Map.toList attribBindings)
        return vao

    where
    program = programInfoProgram programInfo

    attribInfos = programInfoAttribs programInfo

    setAttrib vao (k, a) = do
        let binding = attribBindingIndex a
        buffer <- maybe (throwIO . userError $ "binding buffer not found") return (IntMap.lookup binding buffers)
        location <- maybe (throwIO . userError $ "attrib not found") (return . aiAttribLocation) (Map.lookup k attribInfos)
        setVertexArrayAttribFormatAndBinding vao location a buffer

    setIndexBuffer vao (Just b) =
        GLRaw.glVertexArrayElementBuffer vao b

    setIndexBuffer _ Nothing = return ()


setVertexArrayAttribFormatAndBinding :: GL.VertexArrayObject -> GLRaw.GLuint -> AttribBinding -> GL.BufferObject -> IO ()
setVertexArrayAttribFormatAndBinding vao attribLocation (AttribBinding binding' format (BindBufferSetting offset' stride')) buffer = do
    GLRaw.glEnableVertexArrayAttrib vaoId attribLocation
    GLRaw.glVertexArrayAttribFormat vaoId attribLocation formatSize formatDataType formatNormalized formatRelativeOffset

    GLRaw.glVertexArrayAttribBinding vaoId attribLocation binding
    GLRaw.glVertexArrayVertexBuffer vaoId binding bufferId offset stride

    where
    vaoId = unsafeCoerce vao
    bufferId = unsafeCoerce buffer
    binding = fromIntegral binding'
    AttribFormat fsize formatDataType fnormalized foffset = format
    formatSize = fromIntegral fsize
    formatNormalized = fromIntegral . fromEnum $ fnormalized
    formatRelativeOffset = fromIntegral foffset
    offset = fromIntegral offset'
    stride = fromIntegral stride'

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
