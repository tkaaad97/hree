{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hree.Scene
    ( MeshId(..)
    , MeshInfo(..)
    , Scene(..)
    , SceneState(..)
    , addBuffer
    , addMesh
    , addNode
    , deleteScene
    , addIndexBufferUByte
    , addIndexBufferUShort
    , addIndexBufferUInt
    , addSampler
    , addTexture
    , newNode
    , newScene
    , removeMesh
    , removeNode
    , renderScene
    , translateNode
    , rotateNode
    , updateMeshInstanceCount
    , applyTransformToNode
    ) where

import Control.Exception (bracketOnError, throwIO)
import Control.Monad (void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (index, length)
import qualified Data.ByteString.Char8 as ByteString (pack)
import qualified Data.ByteString.Internal as ByteString (create)
import Data.Coerce (coerce)
import qualified Data.Component as Component
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Proxy (Proxy(..))
import qualified Data.Vector as BV
import qualified Data.Vector.Storable as SV
import Data.Word (Word16, Word32, Word8)
import Foreign (Ptr)
import qualified Foreign (castPtr, copyArray, nullPtr, withArray)
import qualified GLW
import qualified GLW.Groups.ClearBufferMask as GLW (glColorBufferBit,
                                                    glDepthBufferBit)
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified GLW.Groups.PrimitiveType as PrimitiveType
import qualified GLW.Internal.Groups as GLW (PixelFormat(..))
import qualified GLW.Internal.Objects as GLW (Buffer(..))
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import Graphics.Hree.GL
import Graphics.Hree.GL.Types
import Graphics.Hree.Math
import Graphics.Hree.Mesh
import Graphics.Hree.Program
import Graphics.Hree.Texture
import Graphics.Hree.Types
import Linear ((!*!), (^+^))
import qualified Linear
import qualified System.Random.MWC as Random (asGenIO, uniformR,
                                              withSystemRandom)

renderScene :: Scene -> Camera -> IO ()
renderScene scene camera = do
    projectionViewMatrix <- getCameraMatrix camera
    GLW.glClearColor 1 1 1 1
    GLW.glClear (GLW.glColorBufferBit .|. GLW.glDepthBufferBit)
    state <- readIORef . sceneState $ scene
    renderNodes [("projectionViewMatrix", Uniform projectionViewMatrix)] scene state

renderNodes :: [(ByteString, Uniform)] -> Scene -> SceneState -> IO ()
renderNodes uniforms scene state = do
    let nodeIds = ssRootNodes state
    rs <- BV.concatMap id <$> BV.mapM (nodeToRenderInfos scene state zeroTransform Linear.identity False) nodeIds
    renderMany uniforms rs

nodeToRenderInfos :: Scene -> SceneState -> Transform -> Mat4 -> Bool -> NodeId -> IO (BV.Vector RenderInfo)
nodeToRenderInfos scene state parentTransform parentGlobalMatrix parentGlobalUpdated nodeId = fmap (fromMaybe BV.empty) . runMaybeT $ do
    node <- MaybeT $ Component.readComponent nodeId nodeStore
    meshId <- MaybeT . return . nodeMesh . nodeInfoNode $ node
    meshInfo <- MaybeT $ Component.readComponent meshId meshStore
    transform <- MaybeT $ Component.readComponent nodeId transformStore

    localMatrix <- lift $
        if transformUpdated transform
            then do
                let matrix = transformMatrix transform
                _ <- Component.writeComponent nodeId matrix matrixStore
                return matrix
            else fromMaybe Linear.identity <$> Component.readComponent nodeId matrixStore

    let globalUpdated = parentGlobalUpdated || transformUpdated parentTransform || transformUpdated transform
    globalMatrix <- lift $
        if globalUpdated
            then do
                let matrix = parentGlobalMatrix !*! localMatrix
                _ <- Component.writeComponent nodeId matrix globalMatrixStore
                return matrix
            else fromMaybe Linear.identity <$> Component.readComponent nodeId globalMatrixStore

    let renderInfo = toRenderInfo defaultTexture meshInfo globalMatrix
        childNodeIds = nodeChildren . nodeInfoNode $ node
    children <- BV.concatMap id <$>
        lift (BV.mapM (nodeToRenderInfos scene state transform globalMatrix globalUpdated) childNodeIds)

    return $ BV.snoc children renderInfo

    where
    defaultTexture = ssDefaultTexture state
    meshStore = sceneMeshStore scene
    nodeStore = sceneNodeStore scene
    transformStore = sceneNodeTransformStore scene
    matrixStore = sceneNodeTransformMatrixStore scene
    globalMatrixStore = sceneNodeGlobalTransformMatrixStore scene

toRenderInfo :: Maybe Texture -> MeshInfo -> Mat4 -> RenderInfo
toRenderInfo defaultTexture meshInfo matrix =
    let maybeUniform = toUniformEntry "modelMatrix" (Uniform matrix)
        uniforms = Map.elems $ Map.mapMaybeWithKey toUniformEntry (materialUniforms material)
        uniforms' = maybe uniforms (: uniforms) maybeUniform
        renderInfo = RenderInfo program dm vao uniforms' textures
    in renderInfo
    where
    mesh = meshInfoMesh meshInfo
    material = meshMaterial mesh
    program = meshInfoProgram meshInfo
    vao = meshInfoVertexArray meshInfo
    dm = resolveDrawMethod mesh
    uniformInfos = programInfoUniforms program
    toUniformEntry uniformName uniform = do
        uniformInfo <- Map.lookup uniformName uniformInfos
        return (uniformInfo, uniform)
    mtextures = materialTextures material
    textures = if null mtextures
        then maybeToList defaultTexture
        else mtextures

resolveDrawMethod :: Mesh -> DrawMethod
resolveDrawMethod mesh =
    let indicesCount = fromIntegral . geometryVerticesCount . meshGeometry $ mesh
        indexBuffer = geometryIndexBuffer . meshGeometry $ mesh
        instanceCount = fromIntegral <$> meshInstanceCount mesh
    in resolve indicesCount indexBuffer instanceCount

    where
    resolve indicesCount Nothing Nothing =
        DrawArrays PrimitiveType.glTriangles 0 indicesCount
    resolve _ (Just (IndexBuffer _ dt indicesCount)) Nothing =
        DrawElements PrimitiveType.glTriangles indicesCount dt Foreign.nullPtr
    resolve indicesCount Nothing (Just instanceCount) =
        DrawArraysInstanced PrimitiveType.glTriangles 0 indicesCount instanceCount
    resolve _ (Just (IndexBuffer _ dt indicesCount)) (Just instanceCount) =
        DrawElementsInstanced PrimitiveType.glTriangles indicesCount dt Foreign.nullPtr instanceCount

addMesh :: Scene -> Mesh -> IO MeshId
addMesh scene mesh = do
    (program, programAdded) <- mkProgramIfNotExists scene pspec
    GLW.glUseProgram (programInfoProgram program)
    vao <- mkVertexArray (geometryAttribBindings geo) buffers maybeIndexBuffer program
    minfo <- atomicModifyIORef' (sceneState scene) (addMeshFunc program programAdded vao)
    let meshId = meshInfoId minfo
    Component.addComponent meshId minfo (sceneMeshStore scene)
    return meshId

    where
    addMeshFunc program programAdded vao state =
        let meshId = ssMeshCounter state
            meshIdNext = meshId + 1
            bos = map fst . IntMap.elems $ buffers
            bos' = maybe bos ((: bos) . ibBuffer) maybeIndexBuffer
            minfo = MeshInfo meshId mesh bos' program vao
            programs = if programAdded
                        then Map.insert pspec program (ssPrograms state)
                        else ssPrograms state
            newState = state
                { ssMeshCounter = meshIdNext
                , ssPrograms = programs
                }
        in (newState, minfo)

    geo = meshGeometry mesh
    buffers = geometryBuffers geo
    maybeIndexBuffer = geometryIndexBuffer geo
    pspec = resolveProgramSpec mesh

removeMesh :: Scene -> MeshId -> IO ()
removeMesh scene meshId = do
    vao <- fmap meshInfoVertexArray <$> Component.readComponent meshId (sceneMeshStore scene)
    maybe (return ()) GLW.deleteObject vao
    void $ Component.removeComponent meshId (sceneMeshStore scene)

addNode :: Scene -> Node -> Bool -> IO NodeId
addNode scene node isRoot = do
    nodeInfo <- atomicModifyIORef' (sceneState scene) addNodeFunc
    let nodeId = nodeInfoId nodeInfo
        transform = Transform (nodeTranslation node) (nodeRotation node) (nodeScale node) False
        matrix = transformMatrix transform
    Component.addComponent nodeId nodeInfo (sceneNodeStore scene)
    Component.addComponent nodeId transform (sceneNodeTransformStore scene)
    Component.addComponent nodeId matrix (sceneNodeTransformMatrixStore scene)
    return nodeId
    where
    addNodeFunc state =
        let nodeId = ssNodeCounter state
            nodeIdNext = nodeId + 1
            rootNodes = if isRoot
                then BV.snoc (ssRootNodes state) nodeId
                else ssRootNodes state
            nodeInfo = NodeInfo nodeId node
            newState = state
                { ssNodeCounter = nodeIdNext
                , ssRootNodes = rootNodes
                }
        in (newState, nodeInfo)

removeNode :: Scene -> NodeId -> IO ()
removeNode scene nodeId = do
    void $ Component.removeComponent nodeId (sceneNodeStore scene)
    void $ Component.removeComponent nodeId (sceneNodeTransformStore scene)
    void $ Component.removeComponent nodeId (sceneNodeTransformMatrixStore scene)

newNode :: Node
newNode = Node Nothing Nothing Nothing BV.empty (Linear.V3 0 0 0) (Linear.Quaternion 1 (Linear.V3 0 0 0)) (Linear.V3 1 1 1)

translateNode :: Scene -> NodeId -> Vec3 -> IO ()
translateNode scene nodeId v = applyTransformToNode scene nodeId f
    where
    f transform = transform
        { transformTranslation = transformTranslation transform ^+^ v
        , transformUpdated = True
        }

rotateNode :: Scene -> NodeId -> Vec3 -> Float -> IO ()
rotateNode scene nodeId axis angle = applyTransformToNode scene nodeId f
    where
    f transform = transform
        { transformQuaternion = transformQuaternion transform * Linear.axisAngle axis angle
        , transformUpdated = True
        }

applyTransformToNode :: Scene -> NodeId -> (Transform -> Transform) -> IO ()
applyTransformToNode scene nodeId f =
    void $ Component.modifyComponent nodeId f transformStore
    where
    transformStore = sceneNodeTransformStore scene

updateMeshInstanceCount :: Scene -> MeshId -> Maybe Int -> IO ()
updateMeshInstanceCount scene meshId c =
    void $ Component.modifyComponent meshId f meshStore
    where
    f m = m { meshInfoMesh = (meshInfoMesh m) { meshInstanceCount = c } }
    meshStore = sceneMeshStore scene

addBuffer :: Scene -> BufferSource -> IO GLW.Buffer
addBuffer scene bufferSource = do
    buffer <- mkBuffer bufferSource
    atomicModifyIORef' (sceneState scene) (addBufferFunc buffer)
    where
    addBufferFunc buffer state =
        let buffers = buffer : ssBuffers state
        in (state { ssBuffers = buffers }, buffer)

addIndexBufferUByte :: Scene -> SV.Vector Word8 -> IO IndexBuffer
addIndexBufferUByte scene v = do
    buffer <- addBuffer scene (BufferSource v GL.GL_STATIC_READ)
    return (IndexBuffer buffer GL.GL_UNSIGNED_BYTE (fromIntegral . SV.length $ v))

addIndexBufferUShort :: Scene -> SV.Vector Word16 -> IO IndexBuffer
addIndexBufferUShort scene v = do
    buffer <- addBuffer scene (BufferSource v GL.GL_STATIC_READ)
    return (IndexBuffer buffer GL.GL_UNSIGNED_SHORT (fromIntegral . SV.length $ v))

addIndexBufferUInt :: Scene -> SV.Vector Word32 -> IO IndexBuffer
addIndexBufferUInt scene v = do
    buffer <- addBuffer scene (BufferSource v GL.GL_STATIC_READ)
    return (IndexBuffer buffer GL.GL_UNSIGNED_INT (fromIntegral . SV.length $ v))

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

    tryCount :: Int
    tryCount = 10

    addTextureAction True texture = do
        r <- atomicModifyIORef' (sceneState scene) (addTextureFunc name texture)
        name' <- maybe (tryAddTexture name texture tryCount) return r
        _ <- initializeTexture texture
        return $ Just (name', texture)

    addTextureAction False texture = do
        r <- atomicModifyIORef' (sceneState scene) (addTextureFunc name texture)
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
            r <- atomicModifyIORef' (sceneState scene) (addTextureFunc name' texture)
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
    tryCount :: Int
    tryCount = 10
    addSamplerAction True sampler = do
        r <- atomicModifyIORef' (sceneState scene) (addSamplerFunc name sampler)
        name' <- maybe (tryAddSampler name sampler tryCount) return r
        return $ Just (name', sampler)

    addSamplerAction False sampler = do
        r <- atomicModifyIORef' (sceneState scene) (addSamplerFunc name sampler)
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
            r <- atomicModifyIORef' (sceneState scene) (addSamplerFunc name' sampler)
            maybe (tryAddSampler prefix sampler (count - 1)) return r

--setBackground

mkProgramIfNotExists :: Scene -> ProgramSpec -> IO (ProgramInfo, Bool)
mkProgramIfNotExists scene pspec = do
    programs <- fmap ssPrograms . readIORef . sceneState $ scene
    let maybeProgram = Map.lookup pspec programs
    maybe (flip (,) True <$> mkProgramAndInsert scene pspec) (return . flip (,) False) maybeProgram

mkProgramAndInsert :: Scene -> ProgramSpec -> IO ProgramInfo
mkProgramAndInsert scene pspec = do
    program <- mkProgram pspec
    atomicModifyIORef' (sceneState scene) (insertProgram program)
    _ <- mkDefaultTextureIfNotExists scene
    return program
    where
    insertProgram program state =
        let programs = Map.insert pspec program (ssPrograms state)
            newState = state { ssPrograms = programs }
        in (newState, ())

mkDefaultTextureIfNotExists :: Scene -> IO Texture
mkDefaultTextureIfNotExists scene = do
    maybeDefaultTexture <- ssDefaultTexture <$> (readIORef . sceneState $ scene)
    maybe (mkDefaultTexture scene) return maybeDefaultTexture

mkDefaultTexture :: Scene -> IO Texture
mkDefaultTexture scene = Foreign.withArray [0, 0, 0, 1] $ \p -> do
    let settings = TextureSettings 1 GL.GL_RGBA8 1 1 False
        source = TextureSourceData 1 1 PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr Word8))
    (_, texture) <- addTexture scene defaultTextureName settings source
    (_, sampler) <- addSampler scene defaultSamplerName
    atomicModifyIORef' (sceneState scene) (setDefaultTexture (Texture (texture, sampler)))
    return $ Texture (texture, sampler)

    where
    defaultTextureName = "default_texture"
    defaultSamplerName = "default_sampler"
    setDefaultTexture a s =
        (s { ssDefaultTexture = Just a }, ())

newScene :: IO Scene
newScene = do
    ref <- newIORef initialSceneState
    meshes <- Component.newComponentStore defaultPreserveSize Proxy
    nodes <- Component.newComponentStore defaultPreserveSize Proxy
    transforms <- Component.newComponentStore defaultPreserveSize Proxy
    matrices <- Component.newComponentStore defaultPreserveSize Proxy
    globalMatrices <- Component.newComponentStore defaultPreserveSize Proxy
    skins <- Component.newComponentStore defaultPreserveSize Proxy
    return $ Scene ref meshes nodes transforms matrices globalMatrices skins

defaultPreserveSize :: Int
defaultPreserveSize = 10

initialSceneState :: SceneState
initialSceneState =
    let meshCounter = MeshId 1
        nodeCounter = NodeId 1
        rootNodes = BV.empty
        buffers = mempty
        textures = mempty
        samplers = mempty
        defaultTexture = Nothing
        programs = mempty
    in SceneState meshCounter nodeCounter rootNodes buffers textures samplers defaultTexture programs

deleteScene :: Scene -> IO ()
deleteScene scene = do
    (buffers, textures) <- atomicModifyIORef' (sceneState scene) deleteSceneFunc
    meshes <- Component.getComponentSlice (sceneMeshStore scene)
    vaos <- BV.map meshInfoVertexArray <$> BV.freeze meshes
    BV.mapM_ GLW.deleteObject vaos
    GLW.deleteObjects buffers
    GLW.deleteObjects textures
    Component.cleanComponentStore meshStore defaultPreserveSize
    Component.cleanComponentStore transformStore defaultPreserveSize
    Component.cleanComponentStore matrixStore defaultPreserveSize

    where
    meshStore = sceneMeshStore scene
    transformStore = sceneNodeTransformStore scene
    matrixStore = sceneNodeTransformStore scene
    deleteSceneFunc state =
        let buffers = ssBuffers state
            textures = Map.elems . ssTextures $ state
        in (initialSceneState, (buffers, textures))

genRandomName :: ByteString -> Int -> IO ByteString
genRandomName prefix len = do
        v <- SV.generateM len (const randomCharacter)
        a <- SV.unsafeWith v $ \source ->
            ByteString.create len $ \dest -> Foreign.copyArray dest source len
        return (prefix `mappend` a)
    where
    charsLen = ByteString.length charactersForRandomName
    randomCharacter = Random.withSystemRandom . Random.asGenIO $
        fmap (ByteString.index charactersForRandomName) . Random.uniformR (0, charsLen - 1)

charactersForRandomName :: ByteString
charactersForRandomName = ByteString.pack $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
