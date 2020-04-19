{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Hree.Scene
    ( AddedMesh(..)
    , MeshId(..)
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
    , addNodeUniformBlock
    , addRootNodes
    , addSampler
    , addSkin
    , addSkinnedMesh
    , addTexture
    , applyTransformToNode
    , deleteRenderer
    , deleteScene
    , mkDefaultTextureIfNotExists
    , newNode
    , newRenderer
    , newScene
    , readNode
    , readNodeTransform
    , removeBuffer
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

import qualified Chronos as Time (Time, epoch, now)
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
import Graphics.Hree.Material
import Graphics.Hree.Math
import Graphics.Hree.Mesh
import Graphics.Hree.Program
import Graphics.Hree.Texture
import Graphics.Hree.Types
import Linear ((!*!), (^+^))
import qualified Linear
import qualified System.Random.MWC as Random (asGenIO, uniformR,
                                              withSystemRandom)

data AddedMesh b = AddedMesh
    { addedMeshId                         :: !MeshId
    , addedMeshMaterialUniformBlockBinder :: !(UniformBlockBinder b)
    }

renderScene :: Renderer -> Scene -> Camera -> IO ()
renderScene renderer scene camera = do
    t <- Time.now
    GLW.glClearColor 1 1 1 1
    GLW.glClear (GLW.glColorBufferBit .|. GLW.glDepthBufferBit)
    state <- readIORef . sceneState $ scene
    bindCamera (ssCameraBlockBinder state)
    bindLight (ssLightBlockBinder state)
    renderNodes
        t
        ubbs
        renderer
        scene
        state
    where
    ubbs = BV.fromList
        [ ("MaterialBlock", materialBlockBindingIndex)
        , ("CameraBlock", cameraBlockBindingIndex)
        , ("LightBlock", lightBlockBindingIndex)
        , ("JointMatricesBlock", skinJointMatricesBlockBindingIndex)
        , ("JointInverseBindMatricesBlock", skinJointInverseMatricesBlockBindingIndex)
        ]
    bindCamera maybeBinder = do
        cameraBlock <- updateCameraBlock camera
        ubb <- maybe (mkCameraBlockBinder scene cameraBlock) return maybeBinder
        updateAndBindUniformBuffer ubb cameraBlock cameraBlockBindingIndex
    bindLight maybeBinder = do
        lightBlock <- getLightBlock (sceneLightStore scene)
        ubb <- maybe (mkLightBlockBinder scene lightBlock) return maybeBinder
        updateAndBindUniformBuffer ubb lightBlock lightBlockBindingIndex

materialBlockBindingIndex, cameraBlockBindingIndex, lightBlockBindingIndex, skinJointMatricesBlockBindingIndex, skinJointInverseMatricesBlockBindingIndex :: BufferBindingIndex
materialBlockBindingIndex = BufferBindingIndex 1
cameraBlockBindingIndex = BufferBindingIndex 2
lightBlockBindingIndex = BufferBindingIndex 3
skinJointMatricesBlockBindingIndex = BufferBindingIndex 4
skinJointInverseMatricesBlockBindingIndex = BufferBindingIndex 5

renderNodes :: Time.Time -> BV.Vector (ByteString, BufferBindingIndex) -> Renderer -> Scene -> SceneState -> IO ()
renderNodes t ubbs renderer scene state = do
    _ <- runMaybeT $ updateNodeMatrices t scene state
    skins <- Component.getComponentSlice (sceneSkinStore scene)
    BV.mapM_ (updateSkinJoints scene) =<< BV.unsafeFreeze skins
    maybe (return ()) (renderMany ubbs) =<< runMaybeT (nodeToRenderInfos renderer scene state)
    return ()

foldNodes :: Scene -> SceneState -> (NodeInfo -> a -> b -> MaybeT IO (a, b)) -> a -> b -> MaybeT IO b
foldNodes scene state f a b = do
    let nodeIds = ssRootNodes state
    nodes <- BV.mapM (MaybeT . Component.readComponent nodeStore) nodeIds
    BV.foldM' (go a) b nodes
    where
    nodeStore = sceneNodeStore scene
    go x0 y0 node = do
        (x1, y1) <- f node x0 y0
        let nodeIds = nodeChildren . nodeInfoNode $ node
        nodes <- BV.mapM (MaybeT . Component.readComponent nodeStore) nodeIds
        BV.foldM' (go x1) y1 nodes

updateNodeMatrices :: Time.Time -> Scene -> SceneState -> MaybeT IO ()
updateNodeMatrices t scene state = foldNodes scene state go (Linear.identity, False) ()
    where
    transformStore = sceneNodeTransformStore scene
    matrixStore = sceneNodeTransformMatrixStore scene
    globalMatrixStore = sceneNodeGlobalTransformMatrixStore scene

    go node (parentGlobalMatrix, parentGlobalUpdated) _ = do
        let nodeId = nodeInfoId node
        transformInfo <- MaybeT $ Component.readComponent transformStore nodeId
        let transform = transformInfoTransform transformInfo
            updated = transformInfoUpdated transformInfo

        localMatrix <- lift $
            if updated
                then do
                    let matrix = transformMatrix transform
                        tinfo = transformInfo { transformInfoUpdated = False, transformInfoSyncedAt = t }
                    _ <- Component.writeComponent matrixStore nodeId matrix
                    _ <- Component.writeComponent transformStore nodeId tinfo
                    return matrix
                else fromMaybe Linear.identity <$> Component.readComponent matrixStore nodeId

        let globalUpdated = parentGlobalUpdated || updated
        globalMatrix <- lift $
            if globalUpdated
                then do
                    let matrix = parentGlobalMatrix !*! localMatrix
                    _ <- Component.writeComponent globalMatrixStore nodeId matrix
                    return matrix
                else fromMaybe Linear.identity <$> Component.readComponent globalMatrixStore nodeId

        return ((globalMatrix, globalUpdated), ())

nodeToRenderInfos :: Renderer -> Scene -> SceneState -> MaybeT IO [RenderInfo]
nodeToRenderInfos renderer scene state =
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
        meshId <- MaybeT . return . nodeMesh . nodeInfoNode $ node
        meshInfo <- MaybeT $ Component.readComponent meshStore meshId
        let (pspec, pname) = meshInfoProgram meshInfo
            maybeVao = meshInfoVertexArray meshInfo
            maybeSkinId = meshInfoSkin meshInfo
        (program, _) <- lift $ mkProgramIfNotExists renderer pspec pname
        vao <- maybe (lift $ mkMeshVertexArray scene meshInfo program) return maybeVao
        globalMatrix <- lift $ fromMaybe Linear.identity <$> Component.readComponent globalMatrixStore nodeId
        defaultTexture <- lift $ mkDefaultTextureIfNotExists scene
        maybeSkin <- maybe (return Nothing) (lift . Component.readComponent skinStore) $ maybeSkinId
        let matrix = globalMatrix !*! inverseBindMatrix
        let renderInfo = toRenderInfo program defaultTexture meshInfo vao maybeSkin matrix
        return renderInfo

toRenderInfo :: ProgramInfo -> Texture -> MeshInfo -> GLW.VertexArray -> Maybe Skin -> Mat4 -> RenderInfo
toRenderInfo program defaultTexture meshInfo vao skin matrix =
    let uniforms = BV.mapMaybe toUniformEntry $ ("modelMatrix", Uniform matrix) `BV.cons` textureUniforms
        ubs = BV.fromList $ (materialBlockBindingIndex, materialInfoUniformBlock material) : maybe mempty getSkinUbs skin
        renderInfo = RenderInfo program dm vao uniforms ubs textures
    in renderInfo
    where
    material = meshInfoMaterial meshInfo
    dm = resolveDrawMethod meshInfo
    uniformLocations = programInfoUniformLocations program
    toUniformEntry (uniformName, uniform) = do
        uniformLocation <- Map.lookup uniformName uniformLocations
        return (uniformLocation, uniform)
    mtextures = materialInfoTextures material
    mtextureUnits = BV.imap (\i (_, t) -> (fromIntegral i, t)) mtextures :: BV.Vector (GL.GLuint, Texture)
    textureUniforms = BV.imap (\i (a, _) -> (a, Uniform (fromIntegral i :: GL.GLuint))) mtextures
    textures = if BV.null mtextures
        then BV.singleton (0, defaultTexture)
        else mtextureUnits
    getSkinUbs x =
        let joint = (skinJointMatricesBlockBindingIndex, getMatricesBlockBinderBuffer . skinJointMatricesBinder $ x)
            inv = (skinJointInverseMatricesBlockBindingIndex, getMatricesBlockBinderBuffer . skinJointInverseMatricesBinder $ x)
        in [joint, inv]
    getMatricesBlockBinderBuffer (MatricesBlockBinder binder) = uniformBlockBinderBuffer binder

resolveDrawMethod :: MeshInfo -> DrawMethod
resolveDrawMethod mesh =
    let indicesCount = fromIntegral . geometryVerticesCount . meshInfoGeometry $ mesh
        indexBuffer = geometryIndexBuffer . meshInfoGeometry $ mesh
        instanceCount = fromIntegral <$> meshInfoInstanceCount mesh
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

addMesh :: Material b => Scene -> Mesh b -> IO (AddedMesh (MaterialUniformBlock b))
addMesh scene mesh = addMesh_ scene mesh Nothing

addSkinnedMesh :: Material b => Scene -> Mesh b -> SkinId -> IO (AddedMesh (MaterialUniformBlock b))
addSkinnedMesh scene mesh skin = addMesh_ scene mesh (Just skin)

addMesh_ :: Material b => Scene -> Mesh b -> Maybe SkinId -> IO (AddedMesh (MaterialUniformBlock b))
addMesh_ scene mesh maybeSkinId = do
    maybeSkin <- maybe (return Nothing) (Component.readComponent (sceneSkinStore scene)) maybeSkinId
    let pspec = resolveProgramSpec mesh maybeSkin
        pname = getProgramName pspec
    (materialInfo, ubb) <- mkMaterialInfo
    minfo <- atomicModifyIORef' (sceneState scene) (addMeshFunc materialInfo (pspec, pname))
    let meshId = meshInfoId minfo
    Component.addComponent (sceneMeshStore scene) meshId minfo
    return $ AddedMesh meshId ubb

    where
    addMeshFunc materialInfo p state =
        let meshId = ssMeshCounter state
            meshIdNext = meshId + 1
            bos = map fst . IntMap.elems $ buffers
            bos' = SV.fromList $ (materialInfoUniformBlock materialInfo) : maybe bos ((: bos) . ibBuffer) maybeIndexBuffer
            minfo = MeshInfo meshId geo materialInfo instanceCount maybeSkinId bos' p Nothing
            newState = state
                { ssMeshCounter = meshIdNext
                }
        in (newState, minfo)

    mkMaterialInfo = do
        let material = meshMaterial mesh
            textures = materialTextures material
            block = materialUniformBlock material
        buffer <- GLW.createObject (Proxy :: Proxy GLW.Buffer)
        ubb <- newUniformBlockBinder buffer block
        return (MaterialInfo buffer textures, ubb)

    geo = meshGeometry mesh
    instanceCount = meshInstanceCount mesh
    buffers = geometryBuffers geo
    maybeIndexBuffer = geometryIndexBuffer geo

mkMeshVertexArray :: Scene -> MeshInfo -> ProgramInfo -> IO GLW.VertexArray
mkMeshVertexArray scene meshInfo program =
    maybe f return (meshInfoVertexArray meshInfo)
    where
    meshId = meshInfoId meshInfo
    geo = meshInfoGeometry $ meshInfo
    meshStore = sceneMeshStore scene
    setVao vao a = a { meshInfoVertexArray = Just vao }
    f = do
        vao <- mkVertexArray (geometryAttribBindings geo) (geometryBuffers geo) (geometryIndexBuffer geo) program
        void $ Component.modifyComponent meshStore (setVao vao) meshId
        return vao

removeMesh :: Scene -> MeshId -> IO ()
removeMesh scene meshId = do
    vao <- ((id =<<) . fmap meshInfoVertexArray) <$> Component.readComponent (sceneMeshStore scene) meshId
    maybe (return ()) GLW.deleteObject vao
    void $ Component.removeComponent (sceneMeshStore scene) meshId

addNode :: Scene -> Node -> Bool -> IO NodeId
addNode scene node isRoot = do
    nodeInfo <- atomicModifyIORef' (sceneState scene) addNodeFunc
    let nodeId = nodeInfoId nodeInfo
        transform = Transform (nodeTranslation node) (nodeRotation node) (nodeScale node)
        transformInfo = TransformInfo transform True Time.epoch
        matrix = transformMatrix transform
        globalMatrix = Linear.identity
    Component.addComponent (sceneNodeStore scene) nodeId nodeInfo
    Component.addComponent (sceneNodeTransformStore scene) nodeId transformInfo
    Component.addComponent (sceneNodeTransformMatrixStore scene) nodeId matrix
    Component.addComponent (sceneNodeGlobalTransformMatrixStore scene) nodeId globalMatrix
    return nodeId
    where
    addNodeFunc state =
        let nodeId = ssNodeCounter state
            nodeIdNext = nodeId + 1
            rootNodes = if isRoot
                then BV.snoc (ssRootNodes state) nodeId
                else ssRootNodes state
            nodeInfo = NodeInfo nodeId node mempty
            newState = state
                { ssNodeCounter = nodeIdNext
                , ssRootNodes = rootNodes
                }
        in (newState, nodeInfo)

addNodeUniformBlock :: (Block a) => Scene -> NodeId -> BufferBindingIndex -> a -> IO (UniformBlockBinder a)
addNodeUniformBlock scene nodeId bindingIndex a = do
    binder <- mkBlockBinder scene a
    let buffer = uniformBlockBinderBuffer binder
    void $ Component.modifyComponent nodeStore (f buffer) nodeId
    return binder
    where
    nodeStore = sceneNodeStore scene
    f buffer nodeInfo =
        let ubs = nodeInfoUniformBlocks nodeInfo `BV.snoc` (bindingIndex, buffer)
        in nodeInfo { nodeInfoUniformBlocks = ubs }

readNode :: Scene -> NodeId -> IO (Maybe Node)
readNode scene nodeId = fmap nodeInfoNode <$> Component.readComponent store nodeId
    where
    store = sceneNodeStore scene

readNodeTransform :: Scene -> NodeId -> IO (Maybe Transform)
readNodeTransform scene nodeId =
    fmap transformInfoTransform <$> Component.readComponent store nodeId
    where
    store = sceneNodeTransformStore scene

removeNode :: Scene -> NodeId -> IO ()
removeNode scene nodeId = do
    maybeNodeInfo <- Component.readComponent (sceneNodeStore scene) nodeId
    case maybeNodeInfo of
        Just nodeInfo -> do
            let node = nodeInfoNode nodeInfo
            void $ Component.removeComponent nodeStore nodeId
            void $ Component.removeComponent (sceneNodeTransformStore scene) nodeId
            void $ Component.removeComponent (sceneNodeTransformMatrixStore scene) nodeId
            void $ Component.removeComponent (sceneNodeGlobalTransformMatrixStore scene) nodeId

            -- remove node uniform buffers
            BV.mapM_ (removeBuffer scene . snd) (nodeInfoUniformBlocks nodeInfo)

            -- remove from root node list
            isRoot <- BV.elem nodeId . ssRootNodes <$> readIORef (sceneState scene)
            when isRoot $
                atomicModifyIORef' (sceneState scene) removeRootNode
            BV.mapM_ (removeNode scene) (nodeChildren node)

            where
            nodeStore = sceneNodeStore scene
            removeRootNode state =
                let rootNodes = BV.filter (/= nodeId) (ssRootNodes state)
                    state' = state { ssRootNodes = rootNodes }
                in (state', ())
        Nothing -> return ()

updateNode :: Scene -> NodeId -> (Node -> Node) -> IO Bool
updateNode scene nodeId f =
    Component.modifyComponent nodeStore g nodeId
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
newNode = Node Nothing Nothing BV.empty (Linear.V3 0 0 0) (Linear.Quaternion 1 (Linear.V3 0 0 0)) (Linear.V3 1 1 1) Linear.identity

translateNode :: Scene -> NodeId -> Vec3 -> IO ()
translateNode scene nodeId v = applyTransformToNode scene nodeId f
    where
    f transform = transform
        { transformTranslation = transformTranslation transform ^+^ v
        }

rotateNode :: Scene -> NodeId -> Vec3 -> Float -> IO ()
rotateNode scene nodeId axis angle = applyTransformToNode scene nodeId f
    where
    f transform = transform
        { transformQuaternion = transformQuaternion transform * Linear.axisAngle axis angle
        }

applyTransformToNode :: Scene -> NodeId -> (Transform -> Transform) -> IO ()
applyTransformToNode scene nodeId f =
    void $ Component.modifyComponent transformStore g nodeId
    where
    transformStore = sceneNodeTransformStore scene
    g tinfo @ (TransformInfo transform _ _) = tinfo { transformInfoTransform = f transform, transformInfoUpdated = True }

updateMeshInstanceCount :: Scene -> MeshId -> Maybe Int -> IO ()
updateMeshInstanceCount scene meshId c =
    void $ Component.modifyComponent meshStore f meshId
    where
    f m = m { meshInfoInstanceCount = c }
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

removeBuffer :: Scene -> GLW.Buffer -> IO ()
removeBuffer scene buffer = do
    GLW.deleteObject buffer
    atomicModifyIORef' (sceneState scene) removeBufferFunc
    where
    removeBufferFunc state =
        let buffers = filter (/= buffer) (ssBuffers state)
        in (state { ssBuffers = buffers }, ())

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
    Component.addComponent (sceneLightStore scene) lightId lightElem
    return lightId
    where
    addLightFunc state =
        let lightId = ssLightCounter state
            lightIdNext = lightId + 1
            newState = state { ssLightCounter = lightIdNext }
        in (newState, lightId)

removeLight :: Scene -> LightId -> IO ()
removeLight scene lightId =
    void $ Component.removeComponent (sceneLightStore scene) lightId

updateLight :: Scene -> LightId -> (Light -> Light) -> IO Bool
updateLight scene lightId f =
    Component.modifyComponent (sceneLightStore scene) g lightId
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
    Component.addComponent (sceneSkinStore scene) skinId skin
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
    store = sceneNodeGlobalTransformMatrixStore scene
    stride = elemStrideStd140 (Proxy :: Proxy Mat4)
    off = alignmentStd140 (Proxy :: Proxy Mat4)
    go (MatricesBlockBinder binder) =
        updateUniformBlockWith binder $ \p ->
        flip GV.imapM_ joints $ \i nodeId -> runMaybeT $ do
            mat <- MaybeT $ Component.readComponent store nodeId
            lift $ pokeByteOffStd140 (Foreign.castPtr p) (off + stride * i) mat

getLightBlock :: LightStore -> IO LightBlock
getLightBlock store =
    LightBlock . LimitedVector <$> (SV.unsafeFreeze =<< Component.getComponentSlice store)

mkProgramIfNotExists :: Renderer -> ProgramSpec -> ProgramName -> IO (ProgramInfo, Bool)
mkProgramIfNotExists renderer pspec pname = do
    programs <- fmap rendererStatePrograms . readIORef . rendererState $ renderer
    let maybeProgram = Map.lookup pname programs
    maybe (flip (,) True <$> mkProgramAndInsert renderer pspec pname) (return . flip (,) False) maybeProgram

mkProgramAndInsert :: Renderer -> ProgramSpec -> ProgramName -> IO ProgramInfo
mkProgramAndInsert renderer pspec pname = do
    program <- mkProgram pspec
    atomicModifyIORef' (rendererState renderer) (insertProgram program)
    return program
    where
    insertProgram program state =
        let programs = Map.insert pname program (rendererStatePrograms state)
            newState = state { rendererStatePrograms = programs }
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
    in SceneState meshCounter nodeCounter lightCounter skinCounter rootNodes buffers textures samplers defaultTexture cameraBlockBinder lightBlockBinder

deleteScene :: Scene -> IO ()
deleteScene scene = do
    (buffers, textures) <- atomicModifyIORef' (sceneState scene) deleteSceneFunc
    meshes <- Component.getComponentSlice (sceneMeshStore scene)
    vaos <- BV.mapMaybe meshInfoVertexArray <$> BV.unsafeFreeze meshes
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

newRenderer :: IO Renderer
newRenderer =
    newIORef initialRendererState >>= return . Renderer

initialRendererState :: RendererState
initialRendererState = RendererState mempty

deleteRenderer :: Renderer -> IO ()
deleteRenderer renderer = do
    programs <- fmap rendererStatePrograms . readIORef . rendererState $ renderer
    GLW.deleteObjects . map programInfoProgram . Map.elems $ programs

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
