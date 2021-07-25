{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hree.Scene
    ( addLight
    , addMaterial
    , addMesh
    , addNode
    , addNodeUniformBlock
    , addRootNodes
    , addSkin
    , applyTransformToNode
    , defaultRendererOption
    , deleteRenderer
    , deleteScene
    , mkDefaultTextureIfNotExists
    , newRenderer
    , newRendererWithOption
    , newScene
    , readNode
    , readNodeTransform
    , removeLight
    , removeMaterialIfUnused
    , removeMesh
    , removeNode
    , removeSkinIfUnused
    , renderScene
    , rotateNode
    , translateNode
    , updateLight
    , updateMaterialMappingSubImage
    , updateMeshInstanceCount
    , updateMeshMaterialUniformBlock
    , updateMeshVertexBuffer
    , updateNode
    , updateNodeMesh
    , materialBlockBindingIndex
    , cameraBlockBindingIndex
    , lightBlockBindingIndex
    , skinJointMatricesBlockBindingIndex
    , skinJointInverseMatricesBlockBindingIndex
    ) where

import qualified Chronos as Time (Time, epoch, now)
import Control.Exception (bracket, bracketOnError, throwIO)
import Control.Monad (unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import qualified Data.Component as Component
import Data.Function ((&))
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Proxy (Proxy(..))
import qualified Data.Vector as BV
import qualified Data.Vector.Generic as GV (imapM_)
import qualified Data.Vector.Storable as SV
import Data.Word (Word8)
import Foreign (Ptr)
import qualified Foreign
import GHC.TypeNats (KnownNat)
import qualified GLW
import qualified GLW.Groups.ClearBufferMask as ClearBufferMask
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified GLW.Groups.PrimitiveType as PrimitiveType
import qualified GLW.Internal.Groups as GLW (ClearBufferMask(..),
                                             PixelFormat(..))
import qualified Graphics.GL as GL
import Hree.Camera
import Hree.GL
import Hree.GL.Block (Block(..), Elem(..), Element(..), Std140(..))
import Hree.GL.Sampler (setSamplerParamValue)
import Hree.GL.Types
import Hree.GL.UniformBlock
import Hree.Light
import Hree.Material (defaultRenderOption, textureMappingUniformName)
import Hree.Math
import Hree.Program
import Hree.Skin (maxJointCount)
import Hree.Types
import Linear (V2(..), V4(..), (!*!), (^+^))
import qualified Linear

renderScene :: Renderer -> Scene -> Camera -> IO ()
renderScene renderer scene camera = do
    t <- Time.now
    autoClear (rendererOptionAutoClear . rendererOption $ renderer)
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

autoClear :: ClearOption -> IO ()
autoClear (ClearOption colorOption depthOption stencilOption) = do
    clearColor colorOption
    clearDepth depthOption
    clearStencil stencilOption
    unless (clearFlag == zeroFlag) $ GLW.glClear clearFlag
    where
    clearColor Nothing = return ()
    clearColor (Just (V4 red green blue alpha)) = GL.glClearColor red green blue alpha
    clearDepth Nothing      = return ()
    clearDepth (Just depth) = GL.glClearDepth depth
    clearStencil Nothing        = return ()
    clearStencil (Just stencil) = GL.glClearStencil stencil
    zeroFlag = GLW.ClearBufferMask 0
    clearFlag = (if isJust colorOption then ClearBufferMask.glColorBufferBit else zeroFlag)
        .|. (if isJust depthOption then ClearBufferMask.glDepthBufferBit else zeroFlag)
        .|. (if isJust stencilOption then ClearBufferMask.glStencilBufferBit else zeroFlag)

materialBlockBindingIndex, cameraBlockBindingIndex, lightBlockBindingIndex, skinJointMatricesBlockBindingIndex, skinJointInverseMatricesBlockBindingIndex :: UniformBufferBindingIndex
materialBlockBindingIndex = UniformBufferBindingIndex 1
cameraBlockBindingIndex = UniformBufferBindingIndex 2
lightBlockBindingIndex = UniformBufferBindingIndex 3
skinJointMatricesBlockBindingIndex = UniformBufferBindingIndex 4
skinJointInverseMatricesBlockBindingIndex = UniformBufferBindingIndex 5

renderNodes :: Time.Time -> BV.Vector (ByteString, UniformBufferBindingIndex) -> Renderer -> Scene -> SceneState -> IO ()
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
        meshId <- MaybeT . return . nodeInfoMesh $ node
        meshInfo <- MaybeT $ Component.readComponent meshStore meshId
        let meshMaterial = meshInfoMaterial meshInfo
            pname = meshMaterialProgramName meshMaterial
            pspec = meshMaterialProgramSpec meshMaterial
            poption = meshMaterialProgramOption meshMaterial
            maybeVao = meshInfoVertexArray meshInfo
            maybeSkinId = meshInfoSkin meshInfo
        (program, _) <- lift $ mkProgramIfNotExists renderer pspec poption pname
        vao <- maybe (lift $ mkMeshVertexArray scene meshInfo program) return maybeVao
        globalMatrix <- lift $ fromMaybe Linear.identity <$> Component.readComponent globalMatrixStore nodeId
        defaultTexture <- lift $ mkDefaultTextureIfNotExists scene
        maybeSkin <- maybe (return Nothing) (lift . Component.readComponent skinStore) maybeSkinId
        let matrix = globalMatrix !*! inverseBindMatrix
        let renderInfo = toRenderInfo program defaultTexture meshInfo vao maybeSkin matrix (nodeInfoUniformBlocks node)
        return renderInfo

toRenderInfo :: ProgramInfo -> TextureAndSampler -> MeshInfo -> GLW.VertexArray -> Maybe Skin -> Mat4 -> [(UniformBufferBindingIndex, GLW.Buffer)] -> RenderInfo
toRenderInfo program defaultTexture meshInfo vao skin matrix nodeUbs =
    let uniforms = BV.mapMaybe toUniformEntry $ ("modelMatrix", Uniform matrix) `BV.cons` textureUniforms
        renderInfo = RenderInfo program dm vao uniforms ubs textures roption
    in renderInfo
    where
    meshMaterial = meshInfoMaterial meshInfo
    roption = meshMaterialRenderOption meshMaterial
    dm = resolveDrawMethod meshInfo
    uniformLocations = programInfoUniformLocations program
    toUniformEntry (uniformName, uniform) = do
        uniformLocation <- Map.lookup uniformName uniformLocations
        return (uniformLocation, uniform)
    mappings = meshMaterialMappings meshMaterial
    mtextureUnits = BV.fromList . Map.elems $ mappings
    textureUniforms = BV.fromList (map (\(i, a) -> (a, Uniform (i :: GL.GLint))) ([0..] `zip` Map.keys mappings))
    textures = if null mappings
        then pure defaultTexture
        else mtextureUnits
    skinUbs Nothing = []
    skinUbs (Just x) =
        let joint = (skinJointMatricesBlockBindingIndex, getMatricesBlockBinderBuffer . skinJointMatricesBinder $ x)
            inv = (skinJointInverseMatricesBlockBindingIndex, getMatricesBlockBinderBuffer . skinJointInverseMatricesBinder $ x)
        in [joint, inv]
    ubs = BV.fromList . nubBy (\a b -> fst a == fst b)
            $ nodeUbs ++ (materialBlockBindingIndex, snd (meshMaterialUniformBlock meshMaterial)) : skinUbs skin
    getMatricesBlockBinderBuffer (MatricesBlockBinder binder) = uniformBlockBinderBuffer binder

resolveDrawMethod :: MeshInfo -> DrawMethod
resolveDrawMethod mesh =
    let indicesCount = fromIntegral . geometryInfoVerticesCount . meshInfoGeometry $ mesh
        indexBufferSource = geometryInfoIndexBuffer . meshInfoGeometry $ mesh
        instanceCount = fromIntegral <$> meshInfoInstanceCount mesh
    in resolve indicesCount indexBufferSource instanceCount

    where
    resolve indicesCount Nothing Nothing =
        DrawArrays PrimitiveType.glTriangles 0 indicesCount
    resolve _ (Just (IndexBuffer _ dt indicesCount offset)) Nothing =
        DrawElements PrimitiveType.glTriangles indicesCount dt (Foreign.nullPtr `Foreign.plusPtr` offset)
    resolve indicesCount Nothing (Just instanceCount) =
        DrawArraysInstanced PrimitiveType.glTriangles 0 indicesCount instanceCount
    resolve _ (Just (IndexBuffer _ dt indicesCount offset)) (Just instanceCount) =
        DrawElementsInstanced PrimitiveType.glTriangles indicesCount dt (Foreign.nullPtr `Foreign.plusPtr` offset) instanceCount

castMaterialId :: MaterialId a -> MaterialId b
castMaterialId (MaterialId a) = MaterialId a

addMaterial :: Block b => Scene -> Material b -> IO (MaterialId b)
addMaterial scene material = do
    let mappingSources = materialMappings material
    mappings <- Map.fromList <$> mapM (\(k, v) -> (,) k <$> instantiateMapping v) mappingSources
    materialInfo <- atomicModifyIORef' (sceneState scene) (addMaterialFunc mappings)
    let materialId = materialInfoId materialInfo
    Component.addComponent (sceneMaterialStore scene) materialId materialInfo
    return . castMaterialId $ materialId

    where
    uniformBlockData = SV.unsafeCast . SV.singleton . Std140 . materialUniformBlock $ material
    addMaterialFunc mappings state =
        let materialId = ssMaterialCounter state
            materialIdNext = materialId + 1
            roption = applyPartialRenderOption defaultRenderOption $ materialRenderOption material
            materialInfo = MaterialInfo
                { materialInfoId = materialId
                , materialInfoUniformBlock = uniformBlockData
                , materialInfoMappings = mappings
                , materialInfoRenderOption = roption
                , materialInfoProgramOption = materialProgramOption material
                , materialInfoProgramSpec = materialProgramSpec material
                }
            newState = state
                { ssMaterialCounter = materialIdNext
                }
        in (newState, materialInfo)

updateMaterialMappingSubImage :: Scene -> MaterialId a -> TextureMappingType  -> V2 Int -> V2 Int -> (Ptr ()) -> IO ()
updateMaterialMappingSubImage scene materialId mappingType (V2 x y) (V2 width height) sourceData = do
    materialInfo <- maybe throwNotFound return
        =<< Component.readComponent (sceneMaterialStore scene) (castMaterialId materialId)
    TextureAndSampler texture _ <- maybe throwNotFound return $ Map.lookup mappingType (materialInfoMappings materialInfo)
    GLW.glTextureSubImage2D texture 0 (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height) GL.GL_RGBA GL.GL_UNSIGNED_BYTE (Foreign.castPtr sourceData)
    where
    throwNotFound = throwIO . userError $ "material not found. materialId: " ++ show materialId

removeMaterialIfUnused :: Scene -> MaterialId b -> IO ()
removeMaterialIfUnused scene materialId = do
    maybeMaterialInfo <- Component.readComponent materialStore materialId'
    flip (maybe (return ())) maybeMaterialInfo $ \materialInfo -> do
        meshes <- BV.freeze =<< Component.getComponentSlice (sceneMeshStore scene)
        let usingMaterial = BV.any ((== materialId') . meshMaterialMaterialId . meshInfoMaterial) meshes
        unless usingMaterial $ do
            let mappings = Map.elems . materialInfoMappings $ materialInfo
            mapM_ (\(TextureAndSampler texture sampler) -> GLW.deleteObject sampler >> GLW.deleteObject texture) mappings
            void $ Component.removeComponent materialStore materialId'
    where
    materialId' = castMaterialId materialId
    materialStore = sceneMaterialStore scene

castMeshId :: MeshId a -> MeshId b
castMeshId (MeshId a) = MeshId a

addMesh :: Block b => Scene -> Mesh b -> IO (MeshId b)
addMesh scene mesh = do
    maybeSkin <- maybe (return Nothing) (Component.readComponent (sceneSkinStore scene)) maybeSkinId
    materialInfo <- maybe
        (throwIO . userError $ "material not found. materialId: " ++ show materialId) return
        =<< Component.readComponent (sceneMaterialStore scene) materialId
    let roption = materialInfoRenderOption materialInfo
        poption = meshProgramOption mesh materialInfo maybeSkin
        pname = getProgramName (materialInfoProgramSpec materialInfo) poption
    meshMaterial <- createMeshMaterialInfo materialInfo roption poption pname
    geoInfo <- instantiateGeometry geo
    minfo <- atomicModifyIORef' (sceneState scene) (addMeshFunc meshMaterial geoInfo)
    let meshId = meshInfoId minfo
    Component.addComponent (sceneMeshStore scene) meshId minfo
    return . castMeshId $ meshId

    where
    maybeSkinId = meshSkinId mesh
    materialId = castMaterialId . meshMaterialId $ mesh
    geo = meshGeometry mesh
    instanceCount = meshInstanceCount mesh

    addMeshFunc meshMaterial geoInfo state =
        let meshId = ssMeshCounter state
            meshIdNext = meshId + 1
            minfo = MeshInfo meshId geoInfo meshMaterial instanceCount maybeSkinId Nothing
            newState = state
                { ssMeshCounter = meshIdNext
                }
        in (newState, minfo)

    createMeshMaterialInfo materialInfo roption poption pname = do
        let mappings = Map.fromList . map (\(ttype, mapping) -> (textureMappingUniformName ttype, mapping)) . Map.toList $ materialInfoMappings materialInfo
            pspec = materialInfoProgramSpec materialInfo
            block = fromMaybe (unStd140 . SV.head . SV.unsafeCast . materialInfoUniformBlock $ materialInfo) (meshBlock mesh)

        ptr <- Foreign.mallocForeignPtrBytes (sizeOfStd140 mesh)
        Foreign.withForeignPtr ptr $ \p -> pokeBlock p mesh block

        buffer <- GLW.createObject (Proxy :: Proxy GLW.Buffer)
        updateBuffer buffer (BufferSourceVector (materialInfoUniformBlock materialInfo) GL.GL_DYNAMIC_DRAW)

        return (MeshMaterial materialId (Foreign.castForeignPtr ptr, buffer) mappings roption poption pspec pname)

    pokeBlock :: Block b => Ptr () -> proxy b -> b -> IO ()
    pokeBlock ptr _ block =
        Foreign.poke (Foreign.castPtr ptr) (Std140 block)

meshProgramOption :: Mesh b -> MaterialInfo -> Maybe Skin -> ProgramOption
meshProgramOption mesh materialInfo maybeSkin =
    let geo = meshGeometry mesh
        options = defaultProgramOption
            & applyWhen (hasAttribute geo "jointIndices") (`setHasJointIndices` True)
            & applyWhen (hasAttribute geo "jointWeights") (`setHasJointWeights` True)
            & applyWhen (hasAttribute geo "normal") (`setHasVertexNormal` True)
            & applyWhen (hasAttribute geo "tangent") (`setHasVertexTangent` True)
            & applyWhen (hasAttribute geo "color") (`setHasVertexColor` True)
            & applyWhen (hasTextureMapping NormalMapping) (`setHasNormalMap` True)
            & applyWhen (hasTextureMapping MetallicRoughnessMapping) (`setHasMetallicRoughnessMap` True)
            & applyWhen (hasTextureMapping EmissiveMapping) (`setHasEmissiveMap` True)
            & applyWhen (hasTextureMapping OcclusionMapping) (`setHasOcclusionMap` True)
            & applySkinOptions maybeSkin
            & flip applyPartialProgramOption (materialInfoProgramOption materialInfo)
    in options
    where
    applyWhen True f a  = f a
    applyWhen False _ a = a
    applySkinOptions (Just skin) =
        (`setUseVertexSkinning` True) . (`setMaxJointCount` maxJointCount skin)
    applySkinOptions Nothing = id
    mappings = materialInfoMappings materialInfo
    hasTextureMapping = flip Map.member mappings
    hasAttribute geo attribName = Map.member attribName (geometryAttribBindings geo)

instantiateGeometry :: Geometry -> IO GeometryInfo
instantiateGeometry geo = do
    buffers <- fmap IntMap.fromList . mapM createBuffer' . IntMap.assocs $ bufferSources
    indexBuffer <- maybe (return Nothing) (fmap Just . instantiateIndexBuffer) indexBufferSource
    return (GeometryInfo attribBindings buffers indexBuffer verticesCount)
    where
    Geometry attribBindings bufferSources indexBufferSource verticesCount = geo
    createBuffer' (i, (bs, bbs)) = do
            b <- createBuffer bs
            return (i, (b, bbs))

instantiateIndexBuffer :: IndexBufferSource -> IO IndexBuffer
instantiateIndexBuffer (IndexBufferSource s dataType count offset) = do
    b <- createBuffer s
    return (IndexBuffer b dataType count offset)

mkMeshVertexArray :: Scene -> MeshInfo -> ProgramInfo -> IO GLW.VertexArray
mkMeshVertexArray scene meshInfo program =
    maybe f return (meshInfoVertexArray meshInfo)
    where
    meshId = meshInfoId meshInfo
    geo = meshInfoGeometry meshInfo
    meshStore = sceneMeshStore scene
    setVao vao a = a { meshInfoVertexArray = Just vao }
    f = do
        vao <- createVertexArray (geometryInfoAttribBindings geo) (geometryInfoBuffers geo) (geometryInfoIndexBuffer geo) program
        void $ Component.modifyComponent meshStore (setVao vao) meshId
        return vao

removeMesh :: Scene -> MeshId b -> IO ()
removeMesh scene meshId = do
    meshInfo <- maybe
        (throwIO . userError $ "mesh not found. meshId: " ++ show meshId)
        return =<< Component.readComponent (sceneMeshStore scene) (castMeshId meshId)
    let vao = meshInfoVertexArray meshInfo
        geo = meshInfoGeometry meshInfo
        meshMaterial = meshInfoMaterial meshInfo
        uniformBuffer = snd . meshMaterialUniformBlock $ meshMaterial
    maybe (return ()) GLW.deleteObject vao
    deleteGeometryBuffers geo
    GLW.deleteObject uniformBuffer
    meshRemoved <- Component.removeComponent (sceneMeshStore scene) (castMeshId meshId)
    unless meshRemoved
        . throwIO . userError $ "failed to remove mesh component. meshId: " ++ show meshId
    where
    deleteGeometryBuffers geo = do
        let buffers = fmap fst . IntMap.elems . geometryInfoBuffers $ geo
        mapM_ GLW.deleteObject buffers

addNode :: Scene -> Node -> Maybe (MeshId a) -> Bool -> IO NodeId
addNode scene node mesh isRoot = do
    nodeInfo <- atomicModifyIORef' (sceneState scene) addNodeFunc
    let nodeId = nodeInfoId nodeInfo
        transform = Transform (nodeTranslation node) (nodeRotation node) (nodeScale node)
        transformInfo = TransformInfo transform True Time.epoch
        matrix = Linear.identity
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
            nodeInfo = NodeInfo nodeId node (castMeshId <$> mesh) mempty
            newState = state
                { ssNodeCounter = nodeIdNext
                , ssRootNodes = rootNodes
                }
        in (newState, nodeInfo)

addNodeUniformBlock :: (Block a) => Scene -> NodeId -> UniformBufferBindingIndex -> a -> IO (UniformBlockBinder a)
addNodeUniformBlock scene nodeId bindingIndex a = do
    binder <- mkBlockBinder a
    let buffer = uniformBlockBinderBuffer binder
    void $ Component.modifyComponent nodeStore (f buffer) nodeId
    return binder
    where
    nodeStore = sceneNodeStore scene
    f buffer nodeInfo =
        let ubs = nodeInfoUniformBlocks nodeInfo `mappend` [(bindingIndex, buffer)]
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
            mapM_ (GLW.deleteObject . snd) (nodeInfoUniformBlocks nodeInfo)

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

updateNodeMesh :: Scene -> NodeId -> (Maybe (MeshId a)) -> IO Bool
updateNodeMesh scene nodeId mesh =
    Component.modifyComponent nodeStore g nodeId
    where
    nodeStore = sceneNodeStore scene
    g a = a { nodeInfoMesh = castMeshId <$> mesh }

addRootNodes :: Scene -> BV.Vector NodeId -> IO ()
addRootNodes scene nodeIds = atomicModifyIORef' (sceneState scene) addRootNodesFunc
    where
    addRootNodesFunc state =
        let rootNodeIds = ssRootNodes state
            state' = state { ssRootNodes = rootNodeIds `mappend` nodeIds }
        in (state', ())

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

updateMeshInstanceCount :: Scene -> MeshId b -> Maybe Int -> IO ()
updateMeshInstanceCount scene meshId c =
    void $ Component.modifyComponent meshStore f (castMeshId meshId)
    where
    f m = m { meshInfoInstanceCount = c }
    meshStore = sceneMeshStore scene

updateMeshMaterialUniformBlock :: (Block b) => Scene -> MeshId b -> (b -> b) -> IO ()
updateMeshMaterialUniformBlock scene meshId f = do
    meshInfo <- maybe
        (throwIO . userError $ "mesh not found. meshId: " ++ show meshId)
        return =<< Component.readComponent meshStore (castMeshId meshId)
    let (p, buffer) = meshMaterialUniformBlock . meshInfoMaterial $ meshInfo
    Foreign.withForeignPtr p $ \ptr -> do
        updated <- fmap (Std140 . f . unStd140) . Foreign.peek . Foreign.castPtr $ ptr
        Foreign.poke (Foreign.castPtr ptr) updated
        updateBuffer' buffer ptr
    where
    meshStore = sceneMeshStore scene
    size = sizeOfStd140 meshId
    updateBuffer' buffer ptr =
        GLW.glNamedBufferSubData buffer 0 (fromIntegral size) (Foreign.castPtr ptr)

updateMeshVertexBuffer :: Scene -> MeshId b -> Int -> (Ptr () -> IO ()) -> IO ()
updateMeshVertexBuffer scene meshId bindingIndex f = do
    meshInfo <- maybe
        (throwIO . userError $ "mesh not found. meshId: " ++ show meshId)
        return =<< Component.readComponent meshStore (castMeshId meshId)
    let geo = meshInfoGeometry meshInfo
    buffer <- maybe
        (throwIO . userError $ "bindingIndex not found. meshId: " ++ show meshId ++ " bindingIndex: " ++ show bindingIndex)
        (return . fst) . IntMap.lookup bindingIndex . geometryInfoBuffers $ geo
    bracket
        (GLW.glMapNamedBuffer buffer GL.GL_READ_WRITE)
        f
        (const . void $ GLW.glUnmapNamedBuffer buffer)
    where
    meshStore = sceneMeshStore scene

instantiateMapping :: MappingSource -> IO TextureAndSampler
instantiateMapping (MappingSource settings textureSource samplerParamValues) =
    bracketOnError
        ((,) <$> GLW.createObject Proxy <*> GLW.createObject Proxy)
        (\(a, b) -> GLW.deleteObject b >> GLW.deleteObject a)
        initialize

    where
    levels = textureLevels settings
    internalFormat = textureInternalFormat settings
    width = textureWidth settings
    height = textureHeight settings
    generateMipmap = textureGenerateMipmap settings
    swidth = sourceWidth textureSource
    sheight = sourceHeight textureSource
    format = coerce $ sourceFormat textureSource
    dataType = sourceDataType textureSource
    pixels = sourcePixels textureSource

    initialize (texture, sampler) = do
        GLW.glTextureStorage2D texture levels internalFormat width height
        Foreign.withForeignPtr pixels $ \ptr ->
            when (ptr /= Foreign.nullPtr) $ GLW.glTextureSubImage2D texture 0 0 0 swidth sheight format dataType ptr
        when (levels > 1 && generateMipmap) $ GLW.glGenerateTextureMipmap texture
        mapM_ (setSamplerParamValue sampler) samplerParamValues
        return (TextureAndSampler texture sampler)

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
    skinId' <- atomicModifyIORef' (sceneState scene) addSkinFunc
    let len = SV.length joints
        invLen = SV.length inverseBindMatrices
        jointMats = SV.replicate len (Elem Linear.identity)
        invMats =
            if invLen < len
                then SV.map Elem inverseBindMatrices `mappend` SV.replicate (len - invLen) (Elem Linear.identity)
                else SV.map Elem . SV.slice 0 len $ inverseBindMatrices
    jointMatBinder <- mkMatricesBlockBinder jointMats
    invMatBinder <- mkMatricesBlockBinder invMats
    let skin = Skin skinId' nodeId inverseBindMatrices joints jointMatBinder invMatBinder
    Component.addComponent (sceneSkinStore scene) skinId' skin
    return skinId'

    where
    addSkinFunc state =
        let skinId' = ssSkinCounter state
            skinIdNext = skinId' + 1
            newState = state { ssSkinCounter = skinIdNext }
        in (newState, skinId')

removeSkinIfUnused :: Scene -> SkinId -> IO ()
removeSkinIfUnused scene skinId' = do
    maybeSkinInfo <- Component.readComponent (sceneSkinStore scene) skinId'
    flip (maybe (return ())) maybeSkinInfo $ \(Skin _ _ _ _ (MatricesBlockBinder block1) (MatricesBlockBinder block2))  -> do
        meshes <- BV.freeze =<< Component.getComponentSlice (sceneMeshStore scene)
        let usingSkin = BV.any ((== Just skinId') . meshInfoSkin) meshes
        unless usingSkin $ do
            GLW.deleteObject . uniformBlockBinderBuffer $ block1
            GLW.deleteObject . uniformBlockBinderBuffer $ block2
            void $ Component.removeComponent (sceneSkinStore scene) skinId'

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

mkProgramIfNotExists :: Renderer -> ProgramSpec -> ProgramOption -> ProgramName -> IO (ProgramInfo, Bool)
mkProgramIfNotExists renderer pspec poption pname = do
    programs <- fmap rendererStatePrograms . readIORef . rendererState $ renderer
    let maybeProgram = Map.lookup pname programs
    maybe (flip (,) True <$> mkProgramAndInsert renderer pspec poption pname) (return . flip (,) False) maybeProgram

mkProgramAndInsert :: Renderer -> ProgramSpec -> ProgramOption -> ProgramName -> IO ProgramInfo
mkProgramAndInsert renderer pspec poption pname = do
    program <- mkProgram pspec poption
    atomicModifyIORef' (rendererState renderer) (insertProgram program)
    return program
    where
    insertProgram program state =
        let programs = Map.insert pname program (rendererStatePrograms state)
            newState = state { rendererStatePrograms = programs }
        in (newState, ())

mkDefaultTextureIfNotExists :: Scene -> IO TextureAndSampler
mkDefaultTextureIfNotExists scene = do
    maybeDefaultTexture <- ssDefaultTexture <$> (readIORef . sceneState $ scene)
    maybe (mkDefaultTexture scene) return maybeDefaultTexture

mkDefaultTexture :: Scene -> IO TextureAndSampler
mkDefaultTexture scene = do
    p <- Foreign.newArray [255, 255, 255, 255]
    pixels <- Foreign.newForeignPtr_ $ Foreign.castPtr (p :: Ptr Word8)
    let settings = TextureSettings 1 GL.GL_RGBA8 1 1 False
        source = TextureSourceData 1 1 PixelFormat.glRgba GL.GL_UNSIGNED_BYTE pixels
    defaultTexture <- instantiateMapping (MappingSource settings source [])
    atomicModifyIORef' (sceneState scene) (setDefaultTexture defaultTexture)
    return defaultTexture

    where
    setDefaultTexture a s =
        (s { ssDefaultTexture = Just a }, ())

mkBlockBinder :: Block a => a -> IO (UniformBlockBinder a)
mkBlockBinder block = do
    buffer <- GLW.createObject (Proxy :: Proxy GLW.Buffer)
    newUniformBlockBinder buffer block

mkCameraBlockBinder :: Scene -> CameraBlock -> IO (UniformBlockBinder CameraBlock)
mkCameraBlockBinder scene block = do
    ubb <- mkBlockBinder block
    atomicModifyIORef' (sceneState scene) (setCameraBlockBinder ubb)
    return ubb
    where
    setCameraBlockBinder ubb s =
        (s { ssCameraBlockBinder = Just ubb }, ())

mkLightBlockBinder :: Scene -> LightBlock -> IO (UniformBlockBinder LightBlock)
mkLightBlockBinder scene block = do
    ubb <- mkBlockBinder block
    atomicModifyIORef' (sceneState scene) (setLightBlockBinder ubb)
    return ubb
    where
    setLightBlockBinder ubb s =
        (s { ssLightBlockBinder = Just ubb }, ())

mkMatricesBlockBinder :: SV.Vector (Elem Mat4) -> IO MatricesBlockBinder
mkMatricesBlockBinder mats = do
    buffer <- GLW.createObject (Proxy :: Proxy GLW.Buffer)
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
    materials <- Component.newComponentStore defaultPreserveSize Proxy
    meshes <- Component.newComponentStore defaultPreserveSize Proxy
    nodes <- Component.newComponentStore defaultPreserveSize Proxy
    transforms <- Component.newComponentStore defaultPreserveSize Proxy
    matrices <- Component.newComponentStore defaultPreserveSize Proxy
    globalMatrices <- Component.newComponentStore defaultPreserveSize Proxy
    lights <- Component.newComponentStore maxLightCount Proxy
    skins <- Component.newComponentStore defaultPreserveSize Proxy
    return $ Scene ref materials meshes nodes transforms matrices globalMatrices lights skins

defaultPreserveSize :: Int
defaultPreserveSize = 10

initialSceneState :: SceneState
initialSceneState =
    let materialCounter = MaterialId 1
        meshCounter = MeshId 1
        nodeCounter = NodeId 1
        lightCounter = LightId 1
        skinCounter = SkinId 1
        rootNodes = BV.empty
        defaultTexture = Nothing
        cameraBlockBinder = Nothing
        lightBlockBinder = Nothing
    in SceneState materialCounter meshCounter nodeCounter lightCounter skinCounter rootNodes defaultTexture cameraBlockBinder lightBlockBinder

deleteScene :: Scene -> IO ()
deleteScene scene = do
    -- delete meshes
    meshes <- BV.freeze =<< Component.getComponentSlice meshStore
    BV.mapM_ (removeMesh scene . meshInfoId) meshes
    Component.cleanComponentStore meshStore defaultPreserveSize

    -- delete skins
    skins <- BV.freeze =<< Component.getComponentSlice skinStore
    BV.mapM_ (removeSkinIfUnused scene . skinId) skins

    -- delete nodes
    nodes <- BV.freeze =<< Component.getComponentSlice nodeStore
    BV.mapM_ (removeNode scene . nodeInfoId) nodes

    -- delete default texture and buffers
    (maybeDefaultTexture, buffers) <- atomicModifyIORef' (sceneState scene) deleteSceneFunc
    mapM_ GLW.deleteObject buffers
    flip (maybe (return ())) maybeDefaultTexture $
        \(TextureAndSampler texture sampler) -> GLW.deleteObject sampler >> GLW.deleteObject texture

    -- delete materials
    materialInfos <- Component.getComponentSlice materialStore
    BV.mapM_ (removeMaterialIfUnused scene . materialInfoId) =<< BV.freeze materialInfos

    Component.cleanComponentStore nodeStore defaultPreserveSize
    Component.cleanComponentStore transformStore defaultPreserveSize
    Component.cleanComponentStore matrixStore defaultPreserveSize
    Component.cleanComponentStore globalMatrixStore defaultPreserveSize
    Component.cleanComponentStore lightStore maxLightCount
    Component.cleanComponentStore skinStore defaultPreserveSize

    where
    materialStore = sceneMaterialStore scene
    meshStore = sceneMeshStore scene
    nodeStore = sceneNodeStore scene
    transformStore = sceneNodeTransformStore scene
    matrixStore = sceneNodeTransformMatrixStore scene
    globalMatrixStore = sceneNodeGlobalTransformMatrixStore scene
    lightStore = sceneLightStore scene
    skinStore = sceneSkinStore scene
    deleteSceneFunc state =
        let maybeCameraBuffer = uniformBlockBinderBuffer <$> ssCameraBlockBinder state
            maybeLightBuffer = uniformBlockBinderBuffer <$> ssLightBlockBinder state
            buffers = catMaybes [ maybeCameraBuffer, maybeLightBuffer ]
        in (initialSceneState, (ssDefaultTexture state, buffers))

defaultRendererOption :: RendererOption
defaultRendererOption = RendererOption autoClearOption
    where
    autoClearOption = ClearOption (Just (V4 1 1 1 1)) (Just 1) (Just 0)

newRenderer :: IO Renderer
newRenderer = newRendererWithOption defaultRendererOption

newRendererWithOption :: RendererOption -> IO Renderer
newRendererWithOption option = do
    s <- newIORef initialRendererState
    return $ Renderer option s

initialRendererState :: RendererState
initialRendererState =
    RendererState mempty

deleteRenderer :: Renderer -> IO ()
deleteRenderer renderer = do
    programs <- fmap rendererStatePrograms . readIORef . rendererState $ renderer
    GLW.deleteObjects . map programInfoProgram . Map.elems $ programs
