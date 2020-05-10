module Graphics.Hree.GL
    ( attribFormat
    , attribIFormat
    , attribLFormat
    , createBuffer
    , createVertexArray
    , renderMany
    , updateBuffer
    , getRenderOption
    , getDepthOption
    , getBlendingOption
    , getFrontFaceStencilOption
    , getBackFaceStencilOption
    , getStencilOption
    , getColorMask
    , setRenderOption
    , setCullFace
    , setDepthOption
    , setBlendingOption
    , setBlendingSeparateOption
    , setStencilOption
    , setFaceStencilOption
    , setColorMask
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (length)
import qualified Data.ByteString.Unsafe as ByteString (unsafeUseAsCString)
import Data.Foldable (foldrM)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..), asProxyTypeOf)
import qualified Data.Vector as BV (Vector, mapMaybe)
import qualified Data.Vector.Storable as SV
import qualified Foreign (Storable(..), alloca, allocaArray, castPtr)
import qualified GLW
import qualified GLW.Groups.Boolean as Boolean
import qualified GLW.Groups.EnableCap as EnableCap
import qualified GLW.Groups.FrontFaceDirection as FrontFaceDirection
import qualified GLW.Internal.Groups as GLW (CullFaceMode(..),
                                             DepthFunction(..),
                                             StencilFunction(..), StencilOp(..))
import qualified GLW.Internal.Objects as GLW (Buffer(..))
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import Linear (V4(..))
import System.IO.Error (userError)

render :: BV.Vector (ByteString, BufferBindingIndex) -> RenderInfo -> Maybe (GLW.Program, RenderOption) -> IO (Maybe (GLW.Program, RenderOption))
render commons a cur = do
    setCurrentProgram (fst <$> cur) program
    setRenderOption (snd <$> cur) renderOption
    GLW.glBindVertexArray (riVertexArray a)
    bindUniformBlockBuffers (riUniformBlocks a)
    bindUniforms uniforms
    bindTextures textures
    drawWith method
    return . Just $ (program, renderOption)
    where
    program = programInfoProgram . riProgram $ a
    renderOption = riRenderOption a
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

bindTextures :: BV.Vector (GL.GLuint, Texture) -> IO ()
bindTextures textures = mapM_ bindTexture textures
    where
    bindTexture (index, Texture (texture, sampler)) = do
        GLW.glBindTextureUnit index texture
        GLW.glBindSampler index sampler

renderMany :: Foldable t => BV.Vector (ByteString, BufferBindingIndex) -> t RenderInfo -> IO ()
renderMany common = void . foldrM (render common) Nothing

drawWith :: DrawMethod -> IO ()
drawWith (DrawArrays mode index num) = GLW.glDrawArrays mode index num
drawWith (DrawElements mode num dataType indices) = GLW.glDrawElements mode num dataType indices
drawWith (DrawArraysInstanced mode index num inum) = GLW.glDrawArraysInstanced mode index num inum
drawWith (DrawElementsInstanced mode num dataType indices inum) = GLW.glDrawElementsInstanced mode num dataType indices inum

getRenderOption :: IO RenderOption
getRenderOption = do
    cullFaceEnabled <- toBool <$> getBooleanv GL.GL_CULL_FACE
    cullFaceMode <- getInteger64v GL.GL_CULL_FACE_MODE
    let cullFaceOption = if cullFaceEnabled
            then Just . GLW.CullFaceMode . fromIntegral $ cullFaceMode
            else Nothing
    flipSided <- getFlipSided
    depthOption <- getDepthOption
    blendingOption <- getBlendingOption
    stencilOption <- getStencilOption
    colorMask <- getColorMask
    return $ RenderOption cullFaceOption flipSided depthOption blendingOption stencilOption colorMask

setRenderOption :: Maybe RenderOption -> RenderOption -> IO ()
setRenderOption beforeOption option = do
    setCullFace (renderOptionCullFace <$> beforeOption) (renderOptionCullFace option)
    setFlipSided (renderOptionFlipSided <$> beforeOption) (renderOptionFlipSided option)
    setDepthOption (renderOptionDepth <$> beforeOption) (renderOptionDepth option)
    setBlendingOption (renderOptionBlending <$> beforeOption) (renderOptionBlending option)
    setStencilOption (renderOptionStencil <$> beforeOption) (renderOptionStencil option)
    setColorMask (renderOptionColorMask <$> beforeOption) (renderOptionColorMask option)

setCullFace :: Maybe (Maybe GLW.CullFaceMode) -> Maybe GLW.CullFaceMode -> IO ()
setCullFace Nothing Nothing = GLW.glDisable EnableCap.glCullFace
setCullFace Nothing (Just a) = do
    GLW.glEnable EnableCap.glCullFace
    GLW.glCullFace a
setCullFace (Just Nothing) (Just a) = do
    GLW.glEnable EnableCap.glCullFace
    GLW.glCullFace a
setCullFace (Just Nothing) Nothing = return ()
setCullFace (Just (Just a)) (Just b)
    | a == b = return ()
    | otherwise = GLW.glCullFace b
setCullFace (Just (Just _)) Nothing = GLW.glDisable EnableCap.glCullFace

getFlipSided :: IO Bool
getFlipSided =
    isFlipSided <$> getIntegerv GL.GL_FRONT_FACE
    where
    isFlipSided GL.GL_CW = True
    isFlipSided _        = False

setFlipSided :: Maybe Bool -> Bool -> IO ()
setFlipSided Nothing True =
    GLW.glFrontFace FrontFaceDirection.glCw
setFlipSided Nothing False =
    GLW.glFrontFace FrontFaceDirection.glCcw
setFlipSided (Just beforeOption) option
    | beforeOption == option = return ()
    | option = GLW.glFrontFace FrontFaceDirection.glCw
    | otherwise = GLW.glFrontFace FrontFaceDirection.glCcw

getDepthOption :: IO DepthOption
getDepthOption = do
    depthTest <- toBool <$> getBooleanv GL.GL_DEPTH_TEST
    mask <- toBool <$> getBooleanv GL.GL_DEPTH_WRITEMASK
    func <- GLW.DepthFunction . fromIntegral <$> getInteger64v GL.GL_DEPTH_FUNC
    return $ DepthOption depthTest mask func

setDepthOption :: Maybe DepthOption -> DepthOption -> IO ()
setDepthOption Nothing option = do
    if depthOptionDepthTest option
        then GLW.glEnable EnableCap.glDepthTest
        else GLW.glDisable EnableCap.glDepthTest
    GLW.glDepthMask (if depthOptionDepthMask option then Boolean.glTrue else Boolean.glFalse)
    GLW.glDepthFunc (depthOptionDepthFunction option)
setDepthOption (Just beforeOption) option = do
    unless (depthOptionDepthTest beforeOption == depthOptionDepthTest option) $
        if depthOptionDepthTest option
            then GLW.glEnable EnableCap.glDepthTest
            else GLW.glDisable EnableCap.glDepthTest
    unless (depthOptionDepthMask beforeOption == depthOptionDepthMask option) $
        GLW.glDepthMask (if depthOptionDepthMask option then Boolean.glTrue else Boolean.glFalse)
    unless (depthOptionDepthFunction beforeOption == depthOptionDepthFunction option) $
        GLW.glDepthFunc (depthOptionDepthFunction option)

getBlendingOption :: IO BlendingOption
getBlendingOption = do
    enabled <- toBool <$> getBooleanv GL.GL_BLEND
    rgb <- getBlendingSeparateOptionRgb
    alpha <- getBlendingSeparateOptionAlpha
    return $ BlendingOption enabled rgb alpha

setBlendingOption :: Maybe BlendingOption -> BlendingOption -> IO ()
setBlendingOption Nothing option = do
    let BlendingOption enabled rgb alpha = option
    if enabled
        then GLW.glEnable EnableCap.glBlend
        else GLW.glDisable EnableCap.glBlend
    when enabled $
        setBlendingSeparateOption Nothing (rgb, alpha)
setBlendingOption (Just beforeOption) option = do
    let BlendingOption beforeEnabled beforeRgb beforeAlpha = beforeOption
        BlendingOption enabled rgb alpha = option
        beforeSeparateOption = if beforeEnabled
            then Just (beforeRgb, beforeAlpha)
            else Nothing
    unless (beforeEnabled == enabled) $
        if enabled
            then GLW.glEnable EnableCap.glBlend
            else GLW.glDisable EnableCap.glBlend
    when enabled $
        setBlendingSeparateOption beforeSeparateOption (rgb, alpha)

getBlendingSeparateOptionRgb :: IO BlendingSeparateOption
getBlendingSeparateOptionRgb = do
    equation <- fromIntegral <$> getInteger64v GL.GL_BLEND_EQUATION_RGB
    src <- fromIntegral <$> getInteger64v GL.GL_BLEND_SRC_RGB
    dst <- fromIntegral <$> getInteger64v GL.GL_BLEND_DST_RGB
    return $ BlendingSeparateOption equation (src, dst)

getBlendingSeparateOptionAlpha :: IO BlendingSeparateOption
getBlendingSeparateOptionAlpha = do
    equation <- fromIntegral <$> getInteger64v GL.GL_BLEND_EQUATION_ALPHA
    src <- fromIntegral <$> getInteger64v GL.GL_BLEND_SRC_ALPHA
    dst <- fromIntegral <$> getInteger64v GL.GL_BLEND_DST_ALPHA
    return $ BlendingSeparateOption equation (src, dst)

setBlendingSeparateOption :: Maybe (BlendingSeparateOption, BlendingSeparateOption) -> (BlendingSeparateOption, BlendingSeparateOption) -> IO ()
setBlendingSeparateOption Nothing (BlendingSeparateOption rgbEq (rgbSrc, rgbDest), BlendingSeparateOption alphaEq (alphaSrc, alphaDest)) = do
    GL.glBlendEquationSeparate rgbEq alphaEq
    GL.glBlendFuncSeparate rgbSrc rgbDest alphaSrc alphaDest
setBlendingSeparateOption (Just beforeOption) option = do
    let (BlendingSeparateOption beforeRgbEq (beforeRgbSrc, beforeRgbDest), BlendingSeparateOption beforeAlphaEq (beforeAlphaSrc, beforeAlphaDest)) = beforeOption
        (BlendingSeparateOption rgbEq (rgbSrc, rgbDest), BlendingSeparateOption alphaEq (alphaSrc, alphaDest)) = option
    unless (beforeRgbEq == rgbEq && beforeAlphaEq == alphaEq) $
        GL.glBlendEquationSeparate rgbEq alphaEq
    unless (beforeRgbSrc == rgbSrc && beforeRgbDest == rgbDest && beforeAlphaSrc == alphaSrc && beforeAlphaDest == alphaDest) $
        GL.glBlendFuncSeparate rgbSrc rgbDest alphaSrc alphaDest

getStencilOption :: IO StencilOption
getStencilOption = do
    enabled <- toBool <$> getBooleanv GL.GL_STENCIL_TEST
    front <- getFrontFaceStencilOption
    back <- getBackFaceStencilOption
    return $ StencilOption enabled front back

setStencilOption :: Maybe StencilOption -> StencilOption -> IO ()
setStencilOption Nothing option = do
    if stencilOptionStencilTest option
        then GLW.glEnable EnableCap.glStencilTest
        else GLW.glDisable EnableCap.glStencilTest
    let front = stencilOptionFront option
        back = stencilOptionBack option
    if front == back
        then setFaceStencilOption GL.GL_FRONT_AND_BACK Nothing front
        else do
            setFaceStencilOption GL.GL_FRONT Nothing front
            setFaceStencilOption GL.GL_BACK Nothing back
setStencilOption (Just beforeOption) option = do
    let StencilOption beforeTest beforeFront beforeBack = beforeOption
        StencilOption test front back = option

    unless (beforeTest == test) $
        if test
            then GLW.glEnable EnableCap.glStencilTest
            else GLW.glDisable EnableCap.glStencilTest

    unless (beforeFront == front && beforeBack == back) $
        if beforeFront == beforeBack && front == back
            then setFaceStencilOption GL.GL_FRONT_AND_BACK (Just beforeFront) front
            else do
                setFaceStencilOption GL.GL_FRONT (Just beforeFront) front
                setFaceStencilOption GL.GL_BACK (Just beforeBack) back

getFrontFaceStencilOption :: IO FaceStencilOption
getFrontFaceStencilOption = do
    func <- fromIntegral <$> getInteger64v GL.GL_STENCIL_FUNC
    ref <- getIntegerv GL.GL_STENCIL_REF
    fmask <- fromIntegral <$> getInteger64v GL.GL_STENCIL_VALUE_MASK
    sfail <- fromIntegral <$> getInteger64v GL.GL_STENCIL_FAIL
    dpfail <- fromIntegral <$> getInteger64v GL.GL_STENCIL_PASS_DEPTH_FAIL
    dppass <- fromIntegral <$> getInteger64v GL.GL_STENCIL_PASS_DEPTH_PASS
    writeMask <- fromIntegral <$> getInteger64v GL.GL_STENCIL_WRITEMASK
    let funcArgs = StencilFuncArgs (GLW.StencilFunction func) ref fmask
        opArgs = StencilOpArgs (GLW.StencilOp sfail) (GLW.StencilOp dpfail) (GLW.StencilOp dppass)
        option = FaceStencilOption funcArgs opArgs writeMask
    return option

getBackFaceStencilOption :: IO FaceStencilOption
getBackFaceStencilOption = do
    func <- fromIntegral <$> getInteger64v GL.GL_STENCIL_BACK_FUNC
    ref <- getIntegerv GL.GL_STENCIL_BACK_REF
    fmask <- fromIntegral <$> getInteger64v GL.GL_STENCIL_BACK_VALUE_MASK
    sfail <- fromIntegral <$> getInteger64v GL.GL_STENCIL_BACK_FAIL
    dpfail <- fromIntegral <$> getInteger64v GL.GL_STENCIL_BACK_PASS_DEPTH_FAIL
    dppass <- fromIntegral <$> getInteger64v GL.GL_STENCIL_BACK_PASS_DEPTH_PASS
    writeMask <- fromIntegral <$> getInteger64v GL.GL_STENCIL_BACK_WRITEMASK
    let funcArgs = StencilFuncArgs (GLW.StencilFunction func) ref fmask
        opArgs = StencilOpArgs (GLW.StencilOp sfail) (GLW.StencilOp dpfail) (GLW.StencilOp dppass)
        option = FaceStencilOption funcArgs opArgs writeMask
    return option

setFaceStencilOption :: GL.GLenum -> Maybe FaceStencilOption -> FaceStencilOption -> IO ()
setFaceStencilOption face Nothing option = do
    let StencilFuncArgs func ref fmask = faceStencilOptionStencilFunc option
        StencilOpArgs sfail dpfail dppass = faceStencilOptionStencilOp option
        mask = faceStencilOptionStencilMask option
    GLW.glStencilFuncSeparate face func ref fmask
    GLW.glStencilOpSeparate face sfail dpfail dppass
    GLW.glStencilMaskSeparate face mask
setFaceStencilOption face (Just beforeOption) option = do
    let beforeFuncArgs = faceStencilOptionStencilFunc beforeOption
        funcArgs = faceStencilOptionStencilFunc option
        beforeOpArgs = faceStencilOptionStencilOp beforeOption
        opArgs = faceStencilOptionStencilOp option
        StencilFuncArgs func ref fmask = funcArgs
        StencilOpArgs sfail dpfail dppass = opArgs
        beforeMask = faceStencilOptionStencilMask beforeOption
        mask = faceStencilOptionStencilMask option
    unless (beforeFuncArgs == funcArgs) $
        GLW.glStencilFuncSeparate face func ref fmask
    unless (beforeOpArgs == opArgs) $
        GLW.glStencilOpSeparate face sfail dpfail dppass
    unless (beforeMask == mask) $
        GLW.glStencilMaskSeparate face mask

getColorMask :: IO BVec4
getColorMask = Foreign.allocaArray 4 $ \p -> do
    GL.glGetBooleanv GL.GL_COLOR_WRITEMASK p
    red <- Foreign.peekElemOff p 0
    green <- Foreign.peekElemOff p 1
    blue <- Foreign.peekElemOff p 2
    alpha <- Foreign.peekElemOff p 3
    return $ V4 red green blue alpha

setColorMask :: Maybe BVec4 -> BVec4 -> IO ()
setColorMask Nothing (V4 red green blue alpha) = GL.glColorMask red green blue alpha
setColorMask (Just beforeOption) option @ (V4 red green blue alpha)
    | beforeOption == option = return ()
    | otherwise = GL.glColorMask red green blue alpha

createBuffer :: BufferSource -> IO GLW.Buffer
createBuffer (BufferSourcePtr ptr usage) = do
    let size = fromIntegral $ Foreign.sizeOf (asProxyTypeOf undefined ptr)
    buffer <- GLW.createObject (Proxy :: Proxy GLW.Buffer)
    GLW.glNamedBufferData buffer size (Foreign.castPtr ptr) usage
    return buffer
createBuffer (BufferSourceVector vec usage) = do
    let n = SV.length vec
        size = fromIntegral $ n * Foreign.sizeOf (SV.head vec)
    buffer <- GLW.createObject (Proxy :: Proxy GLW.Buffer)
    SV.unsafeWith vec $ \ptr -> GLW.glNamedBufferData buffer size (Foreign.castPtr ptr) usage
    return buffer
createBuffer (BufferSourceByteString bs usage) = do
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

createVertexArray :: Map ByteString AttribBinding -> IntMap (GLW.Buffer, BindBufferSetting) -> Maybe IndexBuffer -> ProgramInfo -> IO GLW.VertexArray
createVertexArray attribBindings buffers indexBuffer programInfo = do
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

toBool :: GL.GLboolean -> Bool
toBool 0 = False
toBool _ = True

getBooleanv :: GL.GLenum -> IO GL.GLboolean
getBooleanv param = Foreign.alloca $ \p -> do
    GL.glGetBooleanv param p
    Foreign.peek p

getIntegerv :: GL.GLenum -> IO GL.GLint
getIntegerv param = Foreign.alloca $ \p -> do
    GL.glGetIntegerv param p
    Foreign.peek p

getInteger64v :: GL.GLenum -> IO GL.GLint64
getInteger64v param = Foreign.alloca $ \p -> do
    GL.glGetInteger64v param p
    Foreign.peek p
