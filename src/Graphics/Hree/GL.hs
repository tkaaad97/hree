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
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Storable as V
import qualified Foreign (Storable(..), alloca, allocaArray)
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import System.IO.Error (userError)
import Unsafe.Coerce (unsafeCoerce)

render :: [(ByteString, Uniform)] -> RenderInfo -> Maybe GLW.Program -> IO (Maybe GLW.Program)
render commons a cur = do
    setCurrentProgram cur program
    GL.glBindVertexArray (riVertexArray a)
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
        GLW.glCurrentProgram p
    toUniformPair (k, u) =
        (,) <$> Map.lookup k uniformInfos <*> Just u

renderMany :: Foldable t => [(ByteString, Uniform)] -> t RenderInfo -> IO ()
renderMany common = void . foldrM (render common) Nothing

drawWith :: DrawMethod -> IO ()
drawWith (DrawArrays mode index num) = GLW.glDrawArrays mode index num
drawWith (DrawElements mode num dataType indicesOffset) = GL.glDrawElements mode num dataType indicesOffset

mkBuffer :: BufferSource -> IO GLW.Buffer
mkBuffer target (BufferSource vec usage) = do
    let n = V.length vec
        size = fromIntegral $ n * Foreign.sizeOf (V.head vec)
    buffer <- GLW.createObject (Proxy :: Proxy GLW.Object)
    V.unsafeWith vec $ \ptr -> GLW.glNamedBufferData buffer size ptr usage
    return (unsafeCoerce buffer)

mkVertexArray :: Map ByteString AttribBinding -> IntMap (GLW.Buffer, BindBufferSetting) -> Maybe GLW.Buffer -> ProgramInfo -> IO GLW.VertexArray
mkVertexArray attribBindings buffers indexBuffer programInfo = do
    GL.glCurrentProgram program
    vao <- GLW.createObject (Proxy :: Proxy GLW.VertexArray)
    mapM_ (setAttrib vao) (Map.toList attribBindings)
    mapM_ (setBindingBuffer vao) . IntMap.toList $ buffers
    setIndexBuffer vaoId (unsafeCoerce indexBuffer)
    GL.glCurrentProgram GLW.zero
    return vao

    where
    program = programInfoProgram programInfo

    attribInfos = programInfoAttribs programInfo

    setAttrib vaoId (k, a) = do
        let binding = attribBindingIndex a
        buffer <- maybe (throwIO . userError $ "binding buffer not found") return (IntMap.lookup binding buffers)
        location <- maybe (throwIO . userError $ "attrib not found") (return . aiAttribLocation) (Map.lookup k attribInfos)
        setVertexArrayAttribFormatAndBinding vaoId location a

    setBindingBuffer vaoId (i, (b, BindBufferSetting offset stride)) =
        GLW.glVertexArrayVertexBuffer vao (fromIntegral i) (unsafeCoerce b) (fromIntegral offset) (fromIntegral stride)

    setIndexBuffer vaoId (Just b) =
        GLW.glVertexArrayElementBuffer vao b

    setIndexBuffer _ Nothing = return ()


setVertexArrayAttribFormatAndBinding :: GLW.VertexArray -> GLW.AttribLocation -> AttribBinding -> IO ()
setVertexArrayAttribFormatAndBinding vaoId attribLocation (AttribBinding binding' format) = do
    GLW.glVertexArrayAttribBinding vaoId attribLocation binding
    GLW.glVertexArrayAttribFormat vaoId attribLocation formatSize formatDataType formatNormalized formatRelativeOffset
    GLW.glEnableVertexArrayAttrib vaoId attribLocation

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
        GLW.uniform (uiUniformLocation ui) a
