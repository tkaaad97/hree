{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Hree.Scene
    ( MeshId(..)
    , MeshInfo(..)
    , Scene(..)
    , SceneState(..)
    , addBuffer
    , addIndexBufferUByte
    , addIndexBufferUInt
    , addIndexBufferUShort
    , addLight
    , addMesh
    , addNode
    , addRootNodes
    , addSampler
    , addSkin
    , addTexture
    , applyTransformToNode
    , deleteScene
    , mkDefaultTextureIfNotExists
    , newNode
    , newScene
    , readNodeTransform
    , removeLight
    , removeMesh
    , removeNode
    , renderScene
    , rotateNode
    , translateNode
    , updateLight
    , updateMeshInstanceCount
    , updateNode
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
import Data.Either (either)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, maybe)
import Data.Proxy (Proxy(..))
import qualified Data.Vector as BV
import qualified Data.Vector.Generic as GV (imapM_)
import qualified Data.Vector.Storable as SV
import Data.Word (Word16, Word32, Word8)
import Foreign (Ptr)
import qualified Foreign (castPtr, copyArray, nullPtr, plusPtr, withArray)
import GHC.TypeNats (KnownNat)
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
import Graphics.Hree.GL.Block (Block(..), Elem(..), Element(..))
import Graphics.Hree.GL.Types
import Graphics.Hree.GL.UniformBlock
import Graphics.Hree.Light
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
    GLW.glClearColor 1 1 1 1
    GLW.glClear (GLW.glColorBufferBit .|. GLW.glDepthBufferBit)
    state <- readIORef . sceneState $ scene
    bindCamera (ssCameraBlockBinder state)
    bindLight (ssLightBlockBinder state)
    renderNodes
        ubbs
        scene
        state
    where
    ubbs = BV.fromList
        [ ("CameraBlock", cameraBlockBindingIndex)
        , ("LightBlock", lightBlockBindingIndex)
        , ("SkinJointMatricesBlock", skinJointMatricesBlockBindingIndex)
        , ("SkinJointInverseMatricesBlock", skinJointInverseMatricesBlockBindingIndex)
        ]
    bindCamera maybeBinder = do
        cameraBlock <- updateCameraBlock camera
        ubb <- maybe (mkCameraBlockBinder scene cameraBlock) return maybeBinder
        updateAndBindUniformBuffer ubb cameraBlock cameraBlockBindingIndex
    bindLight maybeBinder = do
        lightBlock <- getLightBlock (sceneLightStore scene)
        ubb <- maybe (mkLightBlockBinder scene lightBlock) return maybeBinder
        updateAndBindUniformBuffer ubb lightBlock lightBlockBindingIndex

cameraBlockBindingIndex, lightBlockBindingIndex, skinJointMatricesBlockBindingIndex, skinJointInverseMatricesBlockBindingIndex :: BufferBindingIndex
cameraBlockBindingIndex = BufferBindingIndex 1
lightBlockBindingIndex = BufferBindingIndex 2
skinJointMatricesBlockBindingIndex = BufferBindingIndex 3
skinJointInverseMatricesBlockBindingIndex = BufferBindingIndex 4

renderNodes :: BV.Vector (ByteString, BufferBindingIndex) -> Scene -> SceneState -> IO ()
renderNodes ubbs scene state = do
    _ <- runMaybeT $ updateNodeMatrices scene state
    maybe (return ()) (renderMany ubbs) =<< runMaybeT (nodeToRenderInfos scene state)
    return ()

foldNodes :: Scene -> SceneState -> (NodeInfo -> a -> b -> MaybeT IO (a, b)) -> a -> b -> MaybeT IO b
foldNodes scene state f a b = do
    let nodeIds = ssRootNodes state
    nodes <- BV.mapM (MaybeT . flip Component.readComponent nodeStore) nodeIds
    BV.foldM' (go a) b nodes
    where
    nodeStore = sceneNodeStore scene
    go x0 y0 node = do
        (x1, y1) <- f node x0 y0
        let nodeIds = nodeChildren . nodeInfoNode $ node
        nodes <- BV.mapM (MaybeT . flip Component.readComponent nodeStore) nodeIds
        BV.foldM' (go x1) y1 nodes

updateNodeMatrices :: Scene -> SceneState -> MaybeT IO ()
updateNodeMatrices scene state = foldNodes scene state go (zeroTransform, Linear.identity, False) ()
    where
    transformStore = sceneNodeTransformStore scene
    matrixStore = sceneNodeTransformMatrixStore scene
    globalMatrixStore = sceneNodeGlobalTransformMatrixStore scene

    go node (parentTransform, parentGlobalMatrix, parentGlobalUpdated) _ = do
        let nodeId = nodeInfoId node
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

        return ((transform, globalMatrix, globalUpdated), ())

nodeToRenderInfos :: Scene -> SceneState -> MaybeT IO [RenderInfo]
nodeToRenderInfos scene state =
    catMaybes <$> foldNodes scene state go () []
    where
    meshStore = sceneMeshStore scene
    skinStore = sceneSkinStore scene
    globalMatrixStore = sceneNodeGlobalTransformMatrixStore scene

    go node () xs = do
        maybeRenderInfo <- lift . runMaybeT . f $ node
        return ((), maybeRenderInfo : xs)

    f node = do
        let nodeId = nodeInfoId node
            inverseBindMatrix = nodeInverseBindMatrix . nodeInfoNode $ node
            maybeSkinId = nodeSkin . nodeInfoNode $ node
        meshId <- MaybeT . return . nodeMesh . nodeInfoNode $ node
        meshInfo <- MaybeT $ Component.readComponent meshId meshStore
        program <- either (lift . fmap fst . mkProgramIfNotExists scene) return $ meshInfoProgram meshInfo
        globalMatrix <- lift $ fromMaybe Linear.identity <$> Component.readComponent nodeId globalMatrixStore
        defaultTexture <- lift $ mkDefaultTextureIfNotExists scene
        skin <- maybe (return Nothing) (lift . flip Component.readComponent skinStore) maybeSkinId
        let matrix = globalMatrix !*! inverseBindMatrix
        let renderInfo = toRenderInfo program defaultTexture meshInfo skin matrix
        return renderInfo

toRenderInfo :: ProgramInfo -> Texture -> MeshInfo -> Maybe Skin -> Mat4 -> RenderInfo
toRenderInfo program defaultTexture meshInfo skin matrix =
    let maybeUniform = toUniformEntry ("modelMatrix", Uniform matrix)
        uniforms = BV.mapMaybe toUniformEntry $ (BV.fromList . Map.toList $ materialUniforms material) `mappend` textureUniforms
        uniforms' = maybe uniforms (BV.snoc uniforms) maybeUniform
        ubs = maybe mempty getSkinUbs skin
        renderInfo = RenderInfo program dm vao uniforms' ubs textures
    in renderInfo
    where
    mesh = meshInfoMesh meshInfo
    material = meshMaterial mesh
    vao = meshInfoVertexArray meshInfo
    dm = resolveDrawMethod mesh
    uniformLocations = programInfoUniformLocations program
    toUniformEntry (uniformName, uniform) = do
        uniformLocation <- Map.lookup uniformName uniformLocations
        return (uniformLocation, uniform)
    mtextures = BV.fromList . Map.toList $ materialTextures material
    textures = if null mtextures
        then BV.singleton defaultTexture
        else BV.map snd mtextures
    textureUniforms = BV.zip (BV.map fst mtextures) (BV.generate (BV.length textures) (Uniform . (fromIntegral :: Int -> GL.GLuint)))
    getSkinUbs x =
        let joint = (skinJointMatricesBlockBindingIndex, getMatricesBlockBinderBuffer . skinJointMatricesBinder $ x)
            inv = (skinJointInverseMatricesBlockBindingIndex, getMatricesBlockBinderBuffer . skinJointInverseMatricesBinder $ x)
        in BV.fromList [joint, inv]
    getMatricesBlockBinderBuffer (MatricesBlockBinder binder) = uniformBlockBinderBuffer binder

resolveDrawMethod :: Mesh -> DrawMethod
resolveDrawMethod mesh =
    let indicesCount = fromIntegral . geometryVerticesCount . meshGeometry $ mesh
        indexBuffer = geometryIndexBuffer . meshGeometry $ mesh
        instanceCount = fromIntegral <$> meshInstanceCount mesh
    in resolve indicesCount indexBuffer instanceCount

    where
    resolve indicesCount Nothing Nothing =
        DrawArrays PrimitiveType.glTriangles 0 indicesCount
    resolve _ (Just (IndexBuffer _ dt indicesCount offset)) Nothing =
        DrawElements PrimitiveType.glTriangles indicesCount dt (Foreign.nullPtr `Foreign.plusPtr` offset)
    resolve indicesCount Nothing (Just instanceCount) =
        DrawArraysInstanced PrimitiveType.glTriangles 0 indicesCount instanceCount
    resolve _ (Just (IndexBuffer _ dt indicesCount offset)) (Just instanceCount) =
        DrawElementsInstanced PrimitiveType.glTriangles indicesCount dt (Foreign.nullPtr `Foreign.plusPtr` offset) instanceCount

addMesh :: Scene -> Mesh -> IO MeshId
addMesh scene mesh = do
    (program, _) <- mkProgramIfNotExists scene pspec
    maybeProgram <- lookupProgramInfo
    GLW.glUseProgram (programInfoProgram program)
    vao <- mkVertexArray (geometryAttribBindings geo) buffers maybeIndexBuffer program
    minfo <- atomicModifyIORef' (sceneState scene) (addMeshFunc maybeProgram vao)
    let meshId = meshInfoId minfo
    Component.addComponent meshId minfo (sceneMeshStore scene)
    return meshId

    where
    addMeshFunc maybeProgram vao state =
        let meshId = ssMeshCounter state
            meshIdNext = meshId + 1
            bos = map fst . IntMap.elems $ buffers
            bos' = maybe bos ((: bos) . ibBuffer) maybeIndexBuffer
            programInfo = maybe (Left pspec) Right maybeProgram
            minfo = MeshInfo meshId mesh bos' programInfo vao
            programs = maybe
                        (ssPrograms state)
                        (\p -> Map.insert (getProgramName pspec) p (ssPrograms state))
                        maybeProgram
            newState = state
                { ssMeshCounter = meshIdNext
                , ssPrograms = programs
                }
        in (newState, minfo)

    lookupProgramInfo = do
        programs <- fmap ssPrograms . readIORef . sceneState $ scene
        return . Map.lookup (getProgramName pspec) $ programs

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
        globalMatrix = Linear.identity
    Component.addComponent nodeId nodeInfo (sceneNodeStore scene)
    Component.addComponent nodeId transform (sceneNodeTransformStore scene)
    Component.addComponent nodeId matrix (sceneNodeTransformMatrixStore scene)
    Component.addComponent nodeId globalMatrix (sceneNodeGlobalTransformMatrixStore scene)
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

readNodeTransform :: Scene -> NodeId -> IO (Maybe Transform)
readNodeTransform =
    flip Component.readComponent . sceneNodeTransformStore

removeNode :: Scene -> NodeId -> IO ()
removeNode scene nodeId = do
    void $ Component.removeComponent nodeId (sceneNodeStore scene)
    void $ Component.removeComponent nodeId (sceneNodeTransformStore scene)
    void $ Component.removeComponent nodeId (sceneNodeTransformMatrixStore scene)

updateNode :: Scene -> NodeId -> (Node -> Node) -> IO Bool
updateNode scene nodeId f =
    Component.modifyComponent nodeId g nodeStore
    where
    nodeStore = sceneNodeStore scene
    g a = a { nodeInfoNode = f (nodeInfoNode a) }

addRootNodes :: Scene -> BV.Vector NodeId -> IO ()
addRootNodes scene nodeIds = atomicModifyIORef' (sceneState scene) addRootNodesFunc
    where
    addRootNodesFunc state =
        let rootNodeIds = ssRootNodes state
            state' = state { ssRootNodes = rootNodeIds `mappend` nodeIds }
        in (state', ())

newNode :: Node
newNode = Node Nothing Nothing Nothing BV.empty (Linear.V3 0 0 0) (Linear.Quaternion 1 (Linear.V3 0 0 0)) (Linear.V3 1 1 1) Linear.identity

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
    addBufferResource scene buffer
    return buffer

addBufferResource :: Scene -> GLW.Buffer -> IO ()
addBufferResource scene buffer =
    atomicModifyIORef' (sceneState scene) addBufferFunc
    where
    addBufferFunc state =
        let buffers = buffer : ssBuffers state
        in (state { ssBuffers = buffers }, ())

addIndexBufferUByte :: Scene -> SV.Vector Word8 -> IO IndexBuffer
addIndexBufferUByte scene v = do
    buffer <- addBuffer scene (BufferSourceVector v GL.GL_STATIC_READ)
    return (IndexBuffer buffer GL.GL_UNSIGNED_BYTE (fromIntegral . SV.length $ v) 0)

addIndexBufferUShort :: Scene -> SV.Vector Word16 -> IO IndexBuffer
addIndexBufferUShort scene v = do
    buffer <- addBuffer scene (BufferSourceVector v GL.GL_STATIC_READ)
    return (IndexBuffer buffer GL.GL_UNSIGNED_SHORT (fromIntegral . SV.length $ v) 0)

addIndexBufferUInt :: Scene -> SV.Vector Word32 -> IO IndexBuffer
addIndexBufferUInt scene v = do
    buffer <- addBuffer scene (BufferSourceVector v GL.GL_STATIC_READ)
    return (IndexBuffer buffer GL.GL_UNSIGNED_INT (fromIntegral . SV.length $ v) 0)

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

addLight :: Scene -> Light -> IO LightId
addLight scene light = do
    lightId <- atomicModifyIORef' (sceneState scene) addLightFunc
    let lightElem = Elem . marshalLight $ light
    Component.addComponent lightId lightElem (sceneLightStore scene)
    return lightId
    where
    addLightFunc state =
        let lightId = ssLightCounter state
            lightIdNext = lightId + 1
            newState = state { ssLightCounter = lightIdNext }
        in (newState, lightId)

removeLight :: Scene -> LightId -> IO ()
removeLight scene lightId =
    void $ Component.removeComponent lightId (sceneLightStore scene)

updateLight :: Scene -> LightId -> (Light -> Light) -> IO Bool
updateLight scene lightId f =
    Component.modifyComponent lightId g (sceneLightStore scene)
    where
    g = Elem . marshalLight . f . unmarshalLight . unElem

addSkin :: Scene -> NodeId -> SV.Vector NodeId -> SV.Vector Mat4 -> IO SkinId
addSkin scene nodeId joints inverseBindMatrices = do
    skinId <- atomicModifyIORef' (sceneState scene) addSkinFunc
    let len = SV.length joints
        invLen = SV.length inverseBindMatrices
        jointMats = SV.replicate len (Elem Linear.identity)
        invMats =
            if invLen < len
                then SV.map Elem inverseBindMatrices `mappend` SV.replicate (len - invLen) (Elem Linear.identity)
                else SV.map Elem . SV.slice 0 len $ inverseBindMatrices
    jointMatBinder <- mkMatricesBlockBinder scene jointMats
    invMatBinder <- mkMatricesBlockBinder scene invMats
    let skin = Skin nodeId inverseBindMatrices joints jointMatBinder invMatBinder
    Component.addComponent skinId skin (sceneSkinStore scene)
    return skinId

    where
    addSkinFunc state =
        let skinId = ssSkinCounter state
            skinIdNext = skinId + 1
            newState = state { ssSkinCounter = skinIdNext }
        in (newState, skinId)

updateSkinJoints :: Scene -> Skin -> IO ()
updateSkinJoints scene skin = go . skinJointMatricesBinder $ skin
    where
    joints = skinJoints skin
    store = sceneNodeTransformMatrixStore scene
    stride = elemStrideStd140 (Proxy :: Proxy Mat4)
    off = alignmentStd140 (Proxy :: Proxy Mat4)
    go (MatricesBlockBinder binder) =
        updateUniformBlockWith binder $ \p ->
        flip GV.imapM_ joints $ \i nodeId -> runMaybeT $ do
            mat <- MaybeT $ Component.readComponent nodeId store
            lift $ pokeByteOffStd140 (Foreign.castPtr p) (off + stride * i) mat

getLightBlock :: LightStore -> IO LightBlock
getLightBlock store = do
    (v, c) <- Component.unsafeGetComponentVector store
    let lights = SV.unsafeSlice 0 c v
        block = LightBlock . LimitedVector $ lights
    return block

mkProgramIfNotExists :: Scene -> ProgramSpec -> IO (ProgramInfo, Bool)
mkProgramIfNotExists scene pspec = do
    programs <- fmap ssPrograms . readIORef . sceneState $ scene
    let pname = getProgramName pspec
        maybeProgram = Map.lookup pname programs
    maybe (flip (,) True <$> mkProgramAndInsert scene pspec) (return . flip (,) False) maybeProgram

mkProgramAndInsert :: Scene -> ProgramSpec -> IO ProgramInfo
mkProgramAndInsert scene pspec = do
    program <- mkProgram pspec
    atomicModifyIORef' (sceneState scene) (insertProgram program)
    _ <- mkDefaultTextureIfNotExists scene
    return program
    where
    insertProgram program state =
        let pname = getProgramName pspec
            programs = Map.insert pname program (ssPrograms state)
            newState = state { ssPrograms = programs }
        in (newState, ())

mkDefaultTextureIfNotExists :: Scene -> IO Texture
mkDefaultTextureIfNotExists scene = do
    maybeDefaultTexture <- ssDefaultTexture <$> (readIORef . sceneState $ scene)
    maybe (mkDefaultTexture scene) return maybeDefaultTexture

mkDefaultTexture :: Scene -> IO Texture
mkDefaultTexture scene = Foreign.withArray [255, 255, 255, 255] $ \p -> do
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

mkBlockBinder :: Block a => Scene -> a -> IO (UniformBlockBinder a)
mkBlockBinder scene block = do
    buffer <- GLW.createObject (Proxy :: Proxy GLW.Buffer)
    addBufferResource scene buffer
    newUniformBlockBinder buffer block

mkCameraBlockBinder :: Scene -> CameraBlock -> IO (UniformBlockBinder CameraBlock)
mkCameraBlockBinder scene block = do
    ubb <- mkBlockBinder scene block
    atomicModifyIORef' (sceneState scene) (setCameraBlockBinder ubb)
    return ubb
    where
    setCameraBlockBinder ubb s =
        (s { ssCameraBlockBinder = Just ubb }, ())

mkLightBlockBinder :: Scene -> LightBlock -> IO (UniformBlockBinder LightBlock)
mkLightBlockBinder scene block = do
    ubb <- mkBlockBinder scene block
    atomicModifyIORef' (sceneState scene) (setLightBlockBinder ubb)
    return ubb
    where
    setLightBlockBinder ubb s =
        (s { ssLightBlockBinder = Just ubb }, ())

mkMatricesBlockBinder :: Scene -> SV.Vector (Elem Mat4) -> IO MatricesBlockBinder
mkMatricesBlockBinder scene mats = do
    buffer <- GLW.createObject (Proxy :: Proxy GLW.Buffer)
    addBufferResource scene buffer
    go buffer

    where
    len = SV.length mats
    go buffer
        | len <= 128 = mk (Proxy :: Proxy 128) buffer
        | len <= 256 = mk (Proxy :: Proxy 256) buffer
        | otherwise  = mk (Proxy :: Proxy 512) buffer

    mk :: forall p n. KnownNat n => p n -> GLW.Buffer -> IO MatricesBlockBinder
    mk _ buffer =
        MatricesBlockBinder <$> newUniformBlockBinder buffer (LimitedVector mats :: LimitedVector n (Elem Mat4))

newScene :: IO Scene
newScene = do
    ref <- newIORef initialSceneState
    meshes <- Component.newComponentStore defaultPreserveSize Proxy
    nodes <- Component.newComponentStore defaultPreserveSize Proxy
    transforms <- Component.newComponentStore defaultPreserveSize Proxy
    matrices <- Component.newComponentStore defaultPreserveSize Proxy
    globalMatrices <- Component.newComponentStore defaultPreserveSize Proxy
    lights <- Component.newComponentStore maxLightCount Proxy
    skins <- Component.newComponentStore defaultPreserveSize Proxy
    return $ Scene ref meshes nodes transforms matrices globalMatrices lights skins

defaultPreserveSize :: Int
defaultPreserveSize = 10

initialSceneState :: SceneState
initialSceneState =
    let meshCounter = MeshId 1
        nodeCounter = NodeId 1
        lightCounter = LightId 1
        skinCounter = SkinId 1
        rootNodes = BV.empty
        buffers = mempty
        textures = mempty
        samplers = mempty
        defaultTexture = Nothing
        cameraBlockBinder = Nothing
        lightBlockBinder = Nothing
        programs = mempty
    in SceneState meshCounter nodeCounter lightCounter skinCounter rootNodes buffers textures samplers defaultTexture cameraBlockBinder lightBlockBinder programs

deleteScene :: Scene -> IO ()
deleteScene scene = do
    (buffers, textures) <- atomicModifyIORef' (sceneState scene) deleteSceneFunc
    meshes <- Component.getComponentSlice (sceneMeshStore scene)
    vaos <- BV.map meshInfoVertexArray <$> BV.freeze meshes
    BV.mapM_ GLW.deleteObject vaos
    GLW.deleteObjects buffers
    GLW.deleteObjects textures
    Component.cleanComponentStore meshStore defaultPreserveSize
    Component.cleanComponentStore nodeStore defaultPreserveSize
    Component.cleanComponentStore transformStore defaultPreserveSize
    Component.cleanComponentStore matrixStore defaultPreserveSize
    Component.cleanComponentStore globalMatrixStore defaultPreserveSize
    Component.cleanComponentStore lightStore maxLightCount
    Component.cleanComponentStore skinStore defaultPreserveSize

    where
    meshStore = sceneMeshStore scene
    nodeStore = sceneNodeStore scene
    transformStore = sceneNodeTransformStore scene
    matrixStore = sceneNodeTransformMatrixStore scene
    globalMatrixStore = sceneNodeGlobalTransformMatrixStore scene
    lightStore = sceneLightStore scene
    skinStore = sceneSkinStore scene
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
