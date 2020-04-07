module Graphics.Hree.GL
    ( attribFormat
    , attribIFormat
    , attribLFormat
    , mkBuffer
    , mkVertexArray
    , renderMany
    , updateBuffer
    ) where

import Control.Exception (throwIO)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (length)
import qualified Data.ByteString.Unsafe as ByteString (unsafeUseAsCString)
import Data.Foldable (foldrM)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..), asProxyTypeOf)
import qualified Data.Vector as BV (Vector, generate, mapMaybe, zip)
import qualified Data.Vector.Storable as SV
import qualified Foreign (Storable(..), castPtr)
import qualified GLW
import qualified GLW.Internal.Objects as GLW (Buffer(..))
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import System.IO.Error (userError)

render :: BV.Vector (ByteString, BufferBindingIndex) -> RenderInfo -> Maybe GLW.Program -> IO (Maybe GLW.Program)
render commons a cur = do
    setCurrentProgram cur program
    GLW.glBindVertexArray (riVertexArray a)
    bindUniformBlockBuffers (riUniformBlocks a)
    bindUniforms uniforms
    bindTextures textures
    drawWith method
    return . Just $ program
    where
    program = programInfoProgram . riProgram $ a
    uniformBlockInfos = programInfoUniformBlocks . riProgram $ a
    bindingPoints = programInfoBufferBindingPoints . riProgram $ a
    method = riDrawMethod a
    uniforms = riUniforms a
    textures = riTextures a
    setCurrentProgram (Just p0) p1 | p0 == p1 = return ()
    setCurrentProgram _ p = do
        setUniformBlocksBindingPoints p . BV.mapMaybe toUniformBlockPair $ commons
        setUniformBlocksBindingPoints p . BV.mapMaybe toUniformBlockPair $ bindingPoints
        GLW.glUseProgram p
    toUniformBlockPair (k, BufferBindingIndex bindingIndex) = do
        blockInfo <- Map.lookup k uniformBlockInfos
        return (bindingIndex, ubiUniformBlockIndex blockInfo)

bindUniforms :: BV.Vector (GLW.UniformLocation, Uniform) -> IO ()
bindUniforms = mapM_ bindUniform
    where
    bindUniform (uniformLocation, Uniform a) =
        GLW.uniform uniformLocation a

setUniformBlocksBindingPoints :: GLW.Program -> BV.Vector (GL.GLuint, GL.GLuint) -> IO ()
setUniformBlocksBindingPoints program = mapM_ setUniformBlockBindingPoint
    where
    setUniformBlockBindingPoint (bindingIndex, blockIndex) =
        GLW.glUniformBlockBinding program blockIndex bindingIndex

bindUniformBlockBuffers :: BV.Vector (BufferBindingIndex, GLW.Buffer) -> IO ()
bindUniformBlockBuffers = mapM_ bindUniformBlockBuffer
    where
    bindUniformBlockBuffer (BufferBindingIndex bindingIndex, buffer) =
        GLW.glBindBufferBase GL.GL_UNIFORM_BUFFER bindingIndex buffer

bindTextures :: BV.Vector Texture -> IO ()
bindTextures textures = mapM_ bindTexture $ BV.zip (BV.generate (length textures) id) textures
    where
    bindTexture (index, Texture (texture, sampler)) = do
        GLW.glBindTextureUnit (fromIntegral index) texture
        GLW.glBindSampler (fromIntegral index) sampler

renderMany :: Foldable t => BV.Vector (ByteString, BufferBindingIndex) -> t RenderInfo -> IO ()
renderMany common = void . foldrM (render common) Nothing

drawWith :: DrawMethod -> IO ()
drawWith (DrawArrays mode index num) = GLW.glDrawArrays mode index num
drawWith (DrawElements mode num dataType indices) = GLW.glDrawElements mode num dataType indices
drawWith (DrawArraysInstanced mode index num inum) = GLW.glDrawArraysInstanced mode index num inum
drawWith (DrawElementsInstanced mode num dataType indices inum) = GLW.glDrawElementsInstanced mode num dataType indices inum

mkBuffer :: BufferSource -> IO GLW.Buffer
mkBuffer (BufferSourcePtr ptr usage) = do
    let size = fromIntegral $ Foreign.sizeOf (asProxyTypeOf undefined ptr)
    buffer <- GLW.createObject (Proxy :: Proxy GLW.Buffer)
    GLW.glNamedBufferData buffer size (Foreign.castPtr ptr) usage
    return buffer
mkBuffer (BufferSourceVector vec usage) = do
    let n = SV.length vec
        size = fromIntegral $ n * Foreign.sizeOf (SV.head vec)
    buffer <- GLW.createObject (Proxy :: Proxy GLW.Buffer)
    SV.unsafeWith vec $ \ptr -> GLW.glNamedBufferData buffer size (Foreign.castPtr ptr) usage
    return buffer
mkBuffer (BufferSourceByteString bs usage) = do
    let size = fromIntegral $ ByteString.length bs
    buffer <- GLW.createObject (Proxy :: Proxy GLW.Buffer)
    ByteString.unsafeUseAsCString bs $ \ptr -> GLW.glNamedBufferData buffer size (Foreign.castPtr ptr) usage
    return buffer

updateBuffer :: GLW.Buffer -> BufferSource -> IO ()
updateBuffer buffer (BufferSourcePtr ptr usage) = do
    let size = fromIntegral . Foreign.sizeOf $ asProxyTypeOf undefined ptr
    GLW.glNamedBufferData buffer size (Foreign.castPtr ptr) usage
updateBuffer buffer (BufferSourceVector vec usage) = do
    let n = SV.length vec
        size = fromIntegral $ n * Foreign.sizeOf (SV.head vec)
    SV.unsafeWith vec $ \ptr -> GLW.glNamedBufferData buffer size (Foreign.castPtr ptr) usage
updateBuffer buffer (BufferSourceByteString bs usage) = do
    let size = fromIntegral $ ByteString.length bs
    ByteString.unsafeUseAsCString bs $ \ptr -> GLW.glNamedBufferData buffer size (Foreign.castPtr ptr) usage

mkVertexArray :: Map ByteString AttribBinding -> IntMap (GLW.Buffer, BindBufferSetting) -> Maybe IndexBuffer -> ProgramInfo -> IO GLW.VertexArray
mkVertexArray attribBindings buffers indexBuffer programInfo = do
    GLW.glUseProgram program
    vao <- GLW.createObject (Proxy :: Proxy GLW.VertexArray)
    mapM_ (setAttrib vao) (Map.toList attribBindings)
    mapM_ (setBindingBuffer vao) . IntMap.toList $ buffers
    setIndexBuffer vao indexBuffer
    GLW.glUseProgram GLW.zero
    return vao

    where
    program = programInfoProgram programInfo

    attribInfos = programInfoAttribs programInfo

    setAttrib vao (k, a) = do
        let binding = attribBindingIndex a
        _ <- maybe (throwIO . userError $ "binding buffer not found") return (IntMap.lookup (fromIntegral . GLW.unBindingIndex $ binding) buffers)
        let maybeLocation = aiAttribLocation <$> Map.lookup k attribInfos
        maybe (return ())
            (\location -> setVertexArrayAttribFormatAndBinding vao location a)
            maybeLocation

    setBindingBuffer vao (i, (b, BindBufferSetting offset stride divisor)) = do
        let bi = GLW.BindingIndex . fromIntegral $ i
        GLW.glVertexArrayVertexBuffer vao bi b (fromIntegral offset) (fromIntegral stride)
        when (divisor /= 0) $
            GLW.glVertexArrayBindingDivisor vao bi (fromIntegral divisor)

    setIndexBuffer vao (Just b) =
        GLW.glVertexArrayElementBuffer vao (ibBuffer b)

    setIndexBuffer _ Nothing = return ()


setVertexArrayAttribFormatAndBinding :: GLW.VertexArray -> GLW.AttribLocation -> AttribBinding -> IO ()
setVertexArrayAttribFormatAndBinding vao attribLocation (AttribBinding binding (AttribFormat' (AttribFormat fsize formatComponentType fnormalized foffset))) = do
    GLW.glVertexArrayAttribBinding vao attribLocation binding
    GLW.glVertexArrayAttribFormat vao attribLocation formatSize formatComponentType formatNormalized formatRelativeOffset
    GLW.glEnableVertexArrayAttrib vao attribLocation

    where
    formatSize = fromIntegral fsize
    formatNormalized = fromIntegral . fromEnum $ fnormalized
    formatRelativeOffset = fromIntegral foffset

setVertexArrayAttribFormatAndBinding vao attribLocation (AttribBinding binding (AttribIFormat' (AttribIFormat fsize formatComponentType foffset))) = do
    GLW.glVertexArrayAttribBinding vao attribLocation binding
    GLW.glVertexArrayAttribIFormat vao attribLocation formatSize formatComponentType formatRelativeOffset
    GLW.glEnableVertexArrayAttrib vao attribLocation

    where
    formatSize = fromIntegral fsize
    formatRelativeOffset = fromIntegral foffset

setVertexArrayAttribFormatAndBinding vao attribLocation (AttribBinding binding (AttribLFormat' (AttribLFormat fsize formatComponentType foffset))) = do
    GLW.glVertexArrayAttribBinding vao attribLocation binding
    GLW.glVertexArrayAttribLFormat vao attribLocation formatSize formatComponentType formatRelativeOffset
    GLW.glEnableVertexArrayAttrib vao attribLocation

    where
    formatSize = fromIntegral fsize
    formatRelativeOffset = fromIntegral foffset

attribFormat :: Int -> GL.GLenum -> Bool -> Int -> AttributeFormat
attribFormat size type' normalized relativeOffset = AttribFormat' $ AttribFormat size type' normalized relativeOffset

attribIFormat :: Int -> GL.GLenum -> Int -> AttributeFormat
attribIFormat size type' relativeOffset = AttribIFormat' $ AttribIFormat size type' relativeOffset

attribLFormat :: Int -> GL.GLenum -> Int -> AttributeFormat
attribLFormat size type' relativeOffset = AttribLFormat' $ AttribLFormat size type' relativeOffset
