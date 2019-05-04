{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Hree.Scene
    ( MeshInfo(..)
    , Scene(..)
    , SceneState(..)
    , addMesh
    , deleteScene
    , geometryFromVertexVector
    , addSampler
    , addTexture
    , newScene
    , removeMesh
    , renderScene
    ) where

import Control.Exception (bracketOnError, throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (index, length)
import qualified Data.ByteString.Char8 as ByteString (pack)
import qualified Data.ByteString.Internal as ByteString (create)
import Data.Coerce (coerce)
import Data.Foldable (foldr')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.List as List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, mapMaybe, maybeToList)
import Data.Proxy (Proxy(..))
import qualified Data.Traversable as Traversable (mapM)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import Foreign (Ptr, Storable)
import qualified Foreign (castPtr, copyArray, nullPtr, with, withArray)
import qualified GLW
import qualified GLW.Groups.ClearBufferMask as GLW (glColorBufferBit)
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified GLW.Groups.PrimitiveType as PrimitiveType
import qualified GLW.Internal.Groups as GLW (PixelFormat(..))
import qualified GLW.Internal.Objects as GLW (Buffer(..))
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import Graphics.Hree.Geometry
import Graphics.Hree.GL
import Graphics.Hree.GL.Types
import Graphics.Hree.GL.Vertex
import Graphics.Hree.Material
import Graphics.Hree.Mesh
import Graphics.Hree.Program
import Graphics.Hree.Texture
import qualified System.Random.MWC as Random (asGenIO, uniformR,
                                              withSystemRandom)
import Unsafe.Coerce (unsafeCoerce)

data MeshInfo = MeshInfo
    { mishInfoId          :: !Int
    , meshInfoMesh        :: !Mesh
    , meshInfoBuffers     :: ![GLW.Buffer]
    , meshInfoProgram     :: !ProgramInfo
    , meshInfoVertexArray :: !GLW.VertexArray
    } deriving (Show)

newtype Scene = Scene { unScene :: IORef SceneState }

data SceneState = SceneState
    { ssMeshCounter      :: !Int
    , ssMeshes           :: !(IntMap MeshInfo)
    , ssBufferRefCounter :: !(IntMap Int)
    , ssTextures         :: !(Map ByteString (GLW.Texture 'GLW.GL_TEXTURE_2D))
    , ssSamplers         :: !(Map ByteString GLW.Sampler)
    , ssDefaultTexture   :: !(Maybe Texture)
    , ssPrograms         :: !(Map ProgramSpec ProgramInfo)
    }

renderScene :: Scene -> Camera -> IO ()
renderScene scene camera = do
    projectionViewMatrix <- getCameraMatrix camera
    GLW.glClearColor 1 1 1 1
    GLW.glClear GLW.glColorBufferBit
    state <- readIORef . unScene $ scene
    let meshes = ssMeshes state
        defaultTexture = ssDefaultTexture state
    renderMeshes [("projectionViewMatrix", Uniform projectionViewMatrix)] defaultTexture meshes

renderMeshes :: [(ByteString, Uniform)] -> Maybe Texture -> IntMap MeshInfo ->  IO ()
renderMeshes uniforms defaultTexture = renderMany uniforms . fmap (toRenderInfo defaultTexture)

toRenderInfo :: Maybe Texture -> MeshInfo -> RenderInfo
toRenderInfo defaultTexture m = renderInfo
    where
    mesh = meshInfoMesh m
    material = meshMaterial mesh
    program = meshInfoProgram m
    vao = meshInfoVertexArray m
    dm = resolveDrawMethod . meshGeometry . meshInfoMesh $ m
    uniformInfos = programInfoUniforms program
    uniforms = Map.elems $ Map.mapMaybeWithKey toUniformEntry (materialUniforms material)
    toUniformEntry uniformName uniform = do
        uniformInfo <- Map.lookup uniformName uniformInfos
        return (uniformInfo, uniform)
    mtextures = materialTextures material
    textures = if null mtextures
        then maybeToList defaultTexture
        else mtextures
    renderInfo = RenderInfo program dm vao uniforms textures

resolveDrawMethod :: Geometry -> DrawMethod
resolveDrawMethod geo | isNothing (geometryIndexBuffer geo) =
    let count = fromIntegral . geometryCount $ geo
    in DrawArrays PrimitiveType.glTriangles 0 count
resolveDrawMethod geo =
    let count = fromIntegral . geometryCount $ geo
    in DrawElements PrimitiveType.glTriangles count GL.GL_UNSIGNED_INT Foreign.nullPtr

addMesh :: Scene -> Mesh -> IO Int
addMesh scene mesh = do
    (program, programAdded) <- mkProgramIfNotExists scene pspec
    GLW.glUseProgram (programInfoProgram program)
    vao <- mkVertexArray (geometryAttribBindings geo) buffers maybeIndexBuffer program
    atomicModifyIORef' (unScene scene) (addMeshFunc program programAdded vao)

    where
    insertsWith f kvs m = foldr' (uncurry $ IntMap.insertWith f) m kvs
    addMeshFunc program programAdded vao state =
        let meshId = ssMeshCounter state
            meshCounter = meshId + 1
            bos = map fst . IntMap.elems $ buffers
            bos' = maybe bos (: bos) maybeIndexBuffer
            nubBufferIds = List.nub . map (fromIntegral . GLW.unBuffer) $ bos'
            minfo = MeshInfo meshId mesh bos' program vao
            meshes = IntMap.insert meshId minfo (ssMeshes state)
            bufferRefCounter =
                insertsWith (+) (nubBufferIds `zip` repeat 1) (ssBufferRefCounter state)
            programs = if programAdded
                        then Map.insert pspec program (ssPrograms state)
                        else ssPrograms state
            newState = state
                { ssMeshCounter = meshCounter
                , ssMeshes = meshes
                , ssBufferRefCounter = bufferRefCounter
                , ssPrograms = programs
                }
        in (newState, meshId)

    geo = meshGeometry mesh
    buffers = geometryBuffers geo
    maybeIndexBuffer = geometryIndexBuffer geo
    pspec = resolveProgramSpec mesh

removeMesh :: Scene -> Int -> IO ()
removeMesh scene meshId = do
    (vao, deletedBuffers) <- atomicModifyIORef' state (removeMeshFunc meshId)
    maybe (return ()) GLW.deleteObject vao
    GLW.deleteObjects deletedBuffers
    where
    state = unScene scene

removeMeshFunc :: Int -> SceneState -> (SceneState, (Maybe GLW.VertexArray, [GLW.Buffer]))
removeMeshFunc meshId state =
    let (result, meshes) = IntMap.updateLookupWithKey (const $ const Nothing) meshId (ssMeshes state)
        vao = fmap meshInfoVertexArray result
        bos = maybe [] meshInfoBuffers result
        (bufferRefCounter, deletedBuffers) = decrementAndLookupDeletion bos (ssBufferRefCounter state)
        newState = state { ssMeshes = meshes, ssBufferRefCounter = bufferRefCounter }
    in (newState, (vao, deletedBuffers))
    where

    updateLookupWith' f k (xs, m) =
        let (x, m') = IntMap.updateLookupWithKey f k m
        in (x : xs, m')

    decrementAndLookupDeletion [] m = (m, [])
    decrementAndLookupDeletion bs m =
        let ks = List.nub . map (fromIntegral . GLW.unBuffer) $ bs
            decrementOrDelete a
                | a <= 1 = Nothing
                | otherwise = Just (a - 1)
            (xs, m') = foldr' (updateLookupWith' (const decrementOrDelete)) ([], m) ks
            ds = map fst . filter ((== Just 1) . snd) $ (ks `zip` xs)
            ds' = map (GLW.Buffer . fromIntegral) ds
        in (m', ds')

addBuffer :: Scene -> BufferSource -> IO GLW.Buffer
addBuffer scene bufferSource = do
    buffer <- mkBuffer bufferSource
    let bufferId = fromIntegral . GLW.unBuffer $ buffer
    atomicModifyIORef' (unScene scene) (addBufferFunc bufferId)
    return buffer
    where
    addBufferFunc bufferId state =
        let bufferRefCounter = IntMap.insert bufferId 0 (ssBufferRefCounter state)
        in (state { ssBufferRefCounter = bufferRefCounter }, ())

geometryFromVertexVector :: forall a. (Storable a, Vertex a) => GLW.BindingIndex -> Vector a -> GL.GLenum -> Scene -> IO Geometry
geometryFromVertexVector bindingIndex storage usage scene = do
    buffer <- addBuffer scene (BufferSource storage usage)
    let buffers = IntMap.singleton (fromIntegral . GLW.unBindingIndex $ bindingIndex) (buffer, bbs)
    return (Geometry attribBindings buffers Nothing num)
    where
    VertexSpec bbs fields = vertexSpec (Proxy :: Proxy a)
    keys = map vertexFieldAttribName fields
    bindings = map toAttribBinding fields
    toAttribBinding (VertexField name format) = AttribBinding bindingIndex format
    attribBindings = Map.fromList $ zip keys bindings
    num = Vector.length storage

addTexture :: Scene -> ByteString -> TextureSettings -> TextureSourceData -> IO (ByteString, GLW.Texture 'GLW.GL_TEXTURE_2D)
addTexture scene name settings source =
    maybe (throwIO . userError $ "addTextureInternal returned Nothing unexpectedly") return =<< addTextureInternal True scene name settings source

addTextureInternal :: Bool -> Scene -> ByteString -> TextureSettings -> TextureSourceData -> IO (Maybe (ByteString, GLW.Texture 'GLW.GL_TEXTURE_2D))
addTextureInternal renameOnConflict scene name settings source =
    bracketOnError
        (GLW.createObject Proxy)
        GLW.deleteObject
        (addTextureAction renameOnConflict)

    where
    levels = textureLevels settings
    internalFormat = textureInternalFormat settings
    width = textureWidth settings
    height = textureHeight settings
    generateMipmap = textureGenerateMipmap settings
    swidth = sourceWidth source
    sheight = sourceHeight source
    format = coerce $ sourceFormat source
    dataType = sourceDataType source
    pixels = sourcePixels source

    tryCount = 10

    addTextureAction True texture = do
        r <- atomicModifyIORef' (unScene scene) (addTextureFunc name texture)
        name' <- maybe (tryAddTexture name texture tryCount) return r
        initializeTexture texture
        return $ Just (name', texture)

    addTextureAction False texture = do
        r <- atomicModifyIORef' (unScene scene) (addTextureFunc name texture)
        maybe (return Nothing) (\_ -> initializeTexture texture >> return (Just (name, texture))) r

    initializeTexture texture = do
        GLW.glTextureStorage2D texture levels internalFormat width height
        when (pixels /= Foreign.nullPtr) $ GLW.glTextureSubImage2D texture 0 0 0 swidth sheight format dataType pixels
        when (levels > 1 && generateMipmap) $ GLW.glGenerateTextureMipmap texture
        return texture

    addTextureFunc name' texture state =
        let (maybeHit, textures') = Map.insertLookupWithKey (\_ _ a -> a) name' texture (ssTextures state)
        in maybe
            (state { ssTextures = textures' }, Just name')
            (const (state, Nothing))
            maybeHit

    randomLen = 8

    tryAddTexture prefix texture count
        | count == 0 = throwIO . userError $ "failed to addTexture"
        | otherwise = do
            name' <- genRandomName prefix randomLen
            r <- atomicModifyIORef' (unScene scene) (addTextureFunc name' texture)
            maybe (tryAddTexture prefix texture (count - 1)) return r

addSampler :: Scene -> ByteString -> IO (ByteString, GLW.Sampler)
addSampler scene name =
    maybe (throwIO . userError $ "addSamplerInternal returned Nothing unexpectedly") return =<< addSamplerInternal True scene name

addSamplerInternal :: Bool -> Scene -> ByteString -> IO (Maybe (ByteString, GLW.Sampler))
addSamplerInternal renameOnConflict scene name =
    bracketOnError
        (GLW.createObject Proxy)
        GLW.deleteObject
        (addSamplerAction renameOnConflict)

    where
    tryCount = 10
    addSamplerAction True sampler = do
        r <- atomicModifyIORef' (unScene scene) (addSamplerFunc name sampler)
        name' <- maybe (tryAddSampler name sampler tryCount) return r
        return $ Just (name', sampler)

    addSamplerAction False sampler = do
        r <- atomicModifyIORef' (unScene scene) (addSamplerFunc name sampler)
        maybe (return Nothing) (\name' -> return (Just (name', sampler))) r

    addSamplerFunc name' sampler state =
        let (maybeHit, samplers') = Map.insertLookupWithKey (\_ _ a -> a) name' sampler (ssSamplers state)
        in maybe
            (state { ssSamplers = samplers' }, Just name')
            (const (state, Nothing))
            maybeHit

    randomLen = 8

    tryAddSampler prefix sampler count
        | count == 0 = throwIO . userError $ "failed to addSampler"
        | otherwise = do
            name' <- genRandomName prefix randomLen
            r <- atomicModifyIORef' (unScene scene) (addSamplerFunc name' sampler)
            maybe (tryAddSampler prefix sampler (count - 1)) return r

--setBackground

mkProgramIfNotExists :: Scene -> ProgramSpec -> IO (ProgramInfo, Bool)
mkProgramIfNotExists scene pspec = do
    programs <- fmap ssPrograms . readIORef . unScene $ scene
    let maybeProgram = Map.lookup pspec programs
    maybe (flip (,) True <$> mkProgramAndInsert scene pspec) (return . flip (,) False) maybeProgram

mkProgramAndInsert :: Scene -> ProgramSpec -> IO ProgramInfo
mkProgramAndInsert scene pspec = do
    program <- mkProgram pspec
    atomicModifyIORef' (unScene scene) (insertProgram program)
    _ <- mkDefaultTextureIfNotExists scene
    return program
    where
    insertProgram program state =
        let programs = Map.insert pspec program (ssPrograms state)
            newState = state { ssPrograms = programs }
        in (newState, ())

mkDefaultTextureIfNotExists :: Scene -> IO Texture
mkDefaultTextureIfNotExists scene = do
    maybeDefaultTexture <- ssDefaultTexture <$> (readIORef . unScene $ scene)
    maybe (mkDefaultTexture scene) return maybeDefaultTexture

mkDefaultTexture :: Scene -> IO Texture
mkDefaultTexture scene = Foreign.withArray [0, 0, 0, 1] $ \p -> do
    let settings = TextureSettings 1 GL.GL_RGBA8 1 1 False
        source = TextureSourceData 1 1 PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr Word8))
    (_, texture) <- addTexture scene defaultTextureName settings source
    (_, sampler) <- addSampler scene defaultSamplerName
    atomicModifyIORef' (unScene scene) (setDefaultTexture (Texture (texture, sampler)))
    return $ Texture (texture, sampler)

    where
    defaultTextureName = "default_texture"
    defaultSamplerName = "default_sampler"
    setDefaultTexture a s =
        (s { ssDefaultTexture = Just a }, ())

newScene :: IO Scene
newScene = Scene <$> newIORef initialSceneState

initialSceneState :: SceneState
initialSceneState =
    let counter = 1
        meshes = mempty
        buffers = mempty
        textures = mempty
        samplers = mempty
        defaultTexture = Nothing
        programs = mempty
    in SceneState counter meshes buffers textures samplers defaultTexture programs

deleteScene :: Scene -> IO ()
deleteScene scene = do
    (vaos, buffers, textures) <- atomicModifyIORef' (unScene scene) deleteSceneFunc
    GLW.deleteObjects vaos
    GLW.deleteObjects buffers
    GLW.deleteObjects textures

    where
    deleteSceneFunc state =
        let meshIds = IntMap.keys . ssMeshes $ state
            (state', xs) = foldr' removeMeshFunc' (state, []) meshIds
            vaos = mapMaybe fst xs
            buffers = map (GLW.Buffer . fromIntegral) . IntMap.keys . ssBufferRefCounter $ state'
            textures = Map.elems . ssTextures $ state'
        in (initialSceneState, (vaos, buffers, textures))

    removeMeshFunc' i (s, xs) =
        let (s', x) = removeMeshFunc i s
        in (s, x : xs)

genRandomName :: ByteString -> Int -> IO ByteString
genRandomName prefix len = do
        v <- Vector.generateM len (const randomCharacter)
        a <- Vector.unsafeWith v $ \source ->
            ByteString.create len $ \dest -> Foreign.copyArray dest source len
        return (prefix `mappend` a)
    where
    charsLen = ByteString.length charactersForRandomName
    randomCharacter = Random.withSystemRandom . Random.asGenIO $
        fmap (ByteString.index charactersForRandomName) . Random.uniformR (0, charsLen - 1)

charactersForRandomName :: ByteString
charactersForRandomName = ByteString.pack $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
