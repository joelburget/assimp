{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.Formats.Assimp.Storable where

import C2HS
import Prelude hiding (replicate)
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Array
import Control.Monad (liftM, liftM2, (>>=), join)
import Data.Vector.Storable hiding (length, mapM)

import Graphics.Formats.Assimp.Types

#include "../../assimp/include/assimp.h"        // Plain-C interface
#include "../../assimp/include/aiScene.h"       // Output data structure
#include "../../assimp/include/aiPostProcess.h" // Post processing flags
#include "./typedefs.h"

#include <stddef.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- Same as peekArray but checks for a null pointer
peekArray' :: Storable a => Int -> Ptr a -> IO [a]
peekArray' n ptr = if ptr == nullPtr
                   then return []
                   else peekArray n ptr

peekArrayPtr :: (Storable a) => Int -> (Ptr (Ptr a)) -> IO [a]
peekArrayPtr n p = do
  arr' <- peekArray' (fromIntegral n) p
  arr  <- mapM peek arr'
  return arr

instance Storable AiPlane where
  sizeOf _ = #size aiPlane
  alignment _ = #alignment aiPlane
  peek p = do
    a <- (#peek aiPlane, a) p
    b <- (#peek aiPlane, b) p
    c <- (#peek aiPlane, c) p
    d <- (#peek aiPlane, d) p
    return $ AiPlane a b c d
  poke p (AiPlane a b c d) = do
    (#poke aiPlane, a) p a
    (#poke aiPlane, b) p b
    (#poke aiPlane, c) p c
    (#poke aiPlane, d) p d

instance Storable AiRay where
  sizeOf _ = #size aiRay
  alignment _ = #alignment aiRay
  peek p = do
    pos <- (#peek aiRay, pos) p
    dir <- (#peek aiRay, dir) p
    return $ AiRay pos dir
  poke p (AiRay pos dir) = do
    (#poke aiRay, pos) p pos
    (#poke aiRay, dir) p dir

instance Storable AiColor3D where
  sizeOf _ = #size aiColor3D
  alignment _ = #alignment aiColor3D
  peek p = do
    r <- (#peek aiColor3D, r) p
    g <- (#peek aiColor3D, g) p
    b <- (#peek aiColor3D, b) p
    return $ AiColor3D r g b
  poke p (AiColor3D r g b) = do
    (#poke aiColor3D, r) p r
    (#poke aiColor3D, g) p g
    (#poke aiColor3D, b) p b

instance Storable AiColor4D where
  sizeOf _ = #size aiColor4D
  alignment _ = #alignment aiColor4D
  peek p = do
    r <- (#peek aiColor4D, r) p
    g <- (#peek aiColor4D, g) p
    b <- (#peek aiColor4D, b) p
    a <- (#peek aiColor4D, a) p
    return $ AiColor4D r g b a
  poke p (AiColor4D r g b a) = do
    (#poke aiColor4D, r) p r
    (#poke aiColor4D, g) p g
    (#poke aiColor4D, b) p b
    (#poke aiColor4D, a) p a

instance Storable AiMemoryInfo where
  sizeOf _ = #size aiMemoryInfo
  alignment _ = #alignment aiMemoryInfo
  peek p = do
    text <- (#peek aiMemoryInfo, textures) p
    materials <- (#peek aiMemoryInfo, materials) p
    meshes <- (#peek aiMemoryInfo, meshes) p
    nodes <- (#peek aiMemoryInfo, nodes) p
    animations <- (#peek aiMemoryInfo, animations) p
    cameras <- (#peek aiMemoryInfo, cameras) p
    lights <- (#peek aiMemoryInfo, lights) p
    total <- (#peek aiMemoryInfo, total) p
    return $ AiMemoryInfo text materials meshes nodes animations cameras lights total
  poke p (AiMemoryInfo te ma me no an ca li to) = do
    (#poke aiMemoryInfo, textures) p te
    (#poke aiMemoryInfo, materials) p ma
    (#poke aiMemoryInfo, meshes) p me
    (#poke aiMemoryInfo, nodes) p no
    (#poke aiMemoryInfo, animations) p an
    (#poke aiMemoryInfo, cameras) p ca
    (#poke aiMemoryInfo, lights) p li
    (#poke aiMemoryInfo, total) p to

instance Storable AiQuaternion where
  sizeOf _ = #size aiQuaternion
  alignment _ = #alignment aiQuaternion
  peek p = do
    w <- (#peek aiQuaternion, w) p
    x <- (#peek aiQuaternion, x) p
    y <- (#peek aiQuaternion, y) p
    z <- (#peek aiQuaternion, z) p
    return $ AiQuaternion w x y z
  poke p (AiQuaternion w x y z) = do
    (#poke aiQuaternion, w) p w
    (#poke aiQuaternion, x) p x
    (#poke aiQuaternion, y) p y
    (#poke aiQuaternion, z) p z

instance Storable AiVector2D where
  sizeOf _ = #size aiVector2D
  alignment _ = #alignment aiVector2D
  peek p = do
    x <- (#peek aiVector2D, x) p
    y <- (#peek aiVector2D, y) p
    return $ AiVector2D x y
  poke p (AiVector2D x y) = do
    (#poke aiVector2D, x) p x
    (#poke aiVector2D, y) p y

instance Storable AiVector3D where
  sizeOf _ = #size aiVector3D
  alignment _ = #alignment aiVector3D
  peek p = do
    x <- (#peek aiVector3D, x) p
    y <- (#peek aiVector3D, y) p
    z <- (#peek aiVector3D, z) p
    return $ AiVector3D x y z
  poke p (AiVector3D x y z) = do
    (#poke aiVector3D, x) p x
    (#poke aiVector3D, y) p y
    (#poke aiVector3D, z) p z

instance Storable AiString where
  sizeOf _ = #size aiString
  alignment _ = #alignment aiString
  peek p = do
    l <- (#peek aiString, length) p :: IO CUInt
    start <- ((#peek aiString, data) p) :: IO (Ptr CChar)
    if start == nullPtr
      then return $ AiString ""
      else do
        len <- (#peek aiString, length) p
        -- So the string is stored as an array, we need to pass a pointer to
        -- peekCStringLen, so we can't just (#peek aiString, data) because that
        -- would give us the value of the first word of the string instead of
        -- the pointer, so we have to create a pointer.
        str <- peekCStringLen (p `plusPtr` (#offset aiString, data), len)
        return $ AiString str
          
  poke p (AiString dat) = do
    (#poke aiString, length) p $ length dat
    str <- newCString $ dat
    (#poke aiString, data) p str

aiStringToString :: AiString -> String
aiStringToString (AiString s) = s

instance Storable AiMatrix3x3 where
  sizeOf _ = #size aiMatrix3x3
  alignment _ = #alignment aiMatrix3x3
  peek p = return $ AiMatrix3x3 $ replicate 9 0
  poke = undefined

instance Storable AiMatrix4x4 where
  sizeOf _ = #size aiMatrix4x4
  alignment _ = #alignment aiMatrix4x4
  peek p = return $ AiMatrix4x4 $ replicate 16 0
  poke = undefined

instance Storable AiNode where
  sizeOf _ = #size aiNode
  alignment _ = #alignment aiNode
  peek p = do
    mName <- liftM aiStringToString $ (#peek aiNode, mName) p
    mTransformation <- (#peek aiNode, mTransformation) p
    -- mParentPtr <- (#peek aiNode, mParent) p
    -- mParent <- if mParentPtr == nullPtr then return Nothing else return peek mParentPtr
    let mParent = Nothing -- Temporary workaround so we don't end up in an infinite loop
    mNumChildren <- (#peek aiNode, mNumChildren) p :: IO CUInt
    mChildrenP' <- (#peek aiNode, mChildren) p :: IO (Ptr (Ptr AiNode))
    mChildrenP'' <- peekArray' (fromIntegral mNumChildren) mChildrenP' :: IO [Ptr AiNode]
    mChildren <- mapM peek mChildrenP''
    mNumMeshes <- (#peek aiNode, mNumMeshes) p :: IO CUInt
    mMeshes' <- (#peek aiNode, mMeshes) p
    mMeshes <- peekArray (fromIntegral mNumMeshes) mMeshes'
    return $ AiNode mName mTransformation mParent mChildren mMeshes
  poke p (AiNode name trans par chil mes) = do
    (#poke aiNode, mName) p $ AiString name
    (#poke aiNode, mTransformation) p trans
    (#poke aiNode, mParent) p par
    (#poke aiNode, mNumChildren) p $ length chil
    newArray chil >>= (#poke aiNode, mChildren) p
    (#poke aiNode, mNumMeshes) p $ length mes
    newArray mes >>= (#poke aiNode, mMeshes) p

instance Storable AiFace where
  sizeOf _ = #size aiFace
  alignment _ = #alignment aiFace
  peek p = do
    mNumIndices <- liftM fromIntegral ((#peek aiFace, mNumIndices) p :: IO CUInt) :: IO Int
    mIndices <- (#peek aiFace, mIndices) p
    lst <- peekArray mNumIndices mIndices
    return $ AiFace lst
  poke p (AiFace mIndices) = do
    (#poke aiFace, mNumIndices) p $ length mIndices
    newArray mIndices >>= ((#poke aiFace, mIndices) p)

instance Storable AiVertexWeight where
  sizeOf _ = #size aiVertexWeight
  alignment _ = #alignment aiVertexWeight
  peek p = do
    mV <- (#peek aiVertexWeight, mVertexId) p
    mW <- (#peek aiVertexWeight, mWeight) p
    return $ AiVertexWeight mV mW
  poke p (AiVertexWeight mV mW) = do
    (#poke aiVertexWeight, mVertexId) p mV
    (#poke aiVertexWeight, mWeight) p mW

instance Storable AiBone where
  sizeOf _ = #size aiBone
  alignment _ = #alignment aiBone
  peek p = do
    mN <- liftM aiStringToString $ (#peek aiBone, mName) p
    mNW <- (#peek aiBone, mNumWeights) p
    mW <- (#peek aiBone, mWeights) p
    lst <- peekArray mNW mW
    mO <- (#peek aiBone, mOffsetMatrix) p
    return $ AiBone mN lst mO
  poke p (AiBone mN mW mO) = do
    (#poke aiBone, mName) p $ AiString mN
    (#poke aiBone, mNumWeights) p $ length mW
    newArray mW >>= ((#poke aiBone, mWeights) p)
    (#poke aiBone, mOffsetMatrix) p mO

instance Storable AiMesh where
  sizeOf _ = #size aiMesh
  alignment _ = #alignment aiMesh
  peek p = do
    -- mPrimitiveTypes <- liftM (cToEnum :: CUInt -> AiPrimitiveType) $ (#peek aiMesh, mPrimitiveTypes) p
    let mPrimitiveTypes = [AiprimitivetypePoint, AiprimitivetypeTriangle, AiprimitivetypeLine, AiprimitivetypePolygon]
    mNumVertices' <- (#peek aiMesh, mNumVertices) p :: IO CUInt
    let mNumVertices = fromIntegral mNumVertices'
    mVertices' <- (#peek aiMesh, mVertices) p
    mVertices <- peekArray mNumVertices mVertices'
    mNormals' <- (#peek aiMesh, mNormals) p
    mNormals <- peekArray mNumVertices mNormals'
    mTangents' <- (#peek aiMesh, mTangents) p
    mTangents <- peekArray' mNumVertices mTangents'
    mBitangents' <- (#peek aiMesh, mBitangents) p
    mBitangents <- peekArray' mNumVertices mBitangents'
    mColors <- ((#peek aiMesh, mColors) p 
                 >>= (peekArray' mNumVertices)) :: IO [AiColor4D]
    mTextureCoords <- ((#peek aiMesh, mTextureCoords) p 
                        >>= (peekArray' mNumVertices)) :: IO [AiVector3D]
    -- mNumUVComponents <- (#peek aiMesh, mNumUVComponents) p
    -- Looking for a more elegant way to do this
    -- mFaces <- join $ liftM2 peekArray' ((#peek aiMesh, mNumFaces) p) ((#peek aiMesh, mFaces) p)
    mNumFaces' <- (#peek aiMesh, mNumFaces) p :: IO CUInt
    let mNumFaces = fromIntegral mNumFaces'
    mfcs <- (#peek aiMesh, mFaces) p :: IO (Ptr AiFace)
    mFaces <- (#peek aiMesh, mFaces) p >>= peekArray' mNumFaces
    num <- (#peek aiMesh, mNumBones) p :: IO CUInt
    -- mBones <- join $ liftM2 peekArray ((#peek aiMesh, mNumBones) p) ((#peek aiMesh, mBones) p)
    let mBones = []
    mMaterialIndex <- (#peek aiMesh, mMaterialIndex) p
    mName <- liftM aiStringToString $ (#peek aiMesh, mName) p
    return $ AiMesh mPrimitiveTypes mVertices mNormals mTangents mBitangents 
               mColors mTextureCoords 0 mFaces mBones mMaterialIndex mName
  poke = undefined

instance Storable AiMaterialProperty where
  sizeOf _ = #size aiMaterialProperty
  alignment _ = #alignment aiMaterialProperty
  peek p = do
    mKey <- liftM aiStringToString $ (#peek aiMaterialProperty, mKey) p
    mSemantic <- liftM (cToEnum :: CUInt -> AiTextureType) $ (#peek aiMaterialProperty, mSemantic) p
    mIndex <- (#peek aiMaterialProperty, mIndex) p
    -- mType <- liftM toEnum $ (#peek aiMaterialProperty, mType) p
    mData <- (#peek aiMaterialProperty, mData) p >>= peekCString
    -- return $ AiMaterialProperty mKey mSemantic mIndex mType mData
    return $ AiMaterialProperty mKey mSemantic mIndex mData
  poke p (AiMaterialProperty mKey mSemantic mIndex mData) = do
    (#poke aiMaterialProperty, mKey) p $ AiString mKey
    (#poke aiMaterialProperty, mSemantic) p $ fromEnum mSemantic
    (#poke aiMaterialProperty, mIndex) p mIndex
    -- (#poke aiMaterialProperty, mType) p $ fromEnum mType
    (#poke aiMaterialProperty, mData) p $ AiString mData

instance Storable AiNodeAnim where
  sizeOf _ = #size aiNodeAnim
  alignment _ = #alignment aiNodeAnim
  peek _ = return $ AiNodeAnim 0
  poke _ _ = return ()

instance Storable AiMeshAnim where
  sizeOf _ = #size aiMeshAnim
  alignment _ = #alignment aiMeshAnim
  peek _ = return $ AiMeshAnim 0
  poke _ _ = return ()

instance Storable AiMaterial where
  sizeOf _ = #size aiMaterial
  alignment _ = #alignment aiMaterial
  peek p = do
    -- mProperties <- join $ liftM2 peekArrayPtr ((#peek aiMaterial, mNumProperties) p) ((#peek aiMaterial, mProperties) p)
    mNumProperties' <- ((#peek aiMaterial, mNumProperties) p) :: IO CUInt
    let mNumProperties = fromIntegral mNumProperties' 
    mProperties' <- ((#peek aiMaterial, mProperties) p)
    mProperties <- peekArrayPtr mNumProperties mProperties'
    return $ AiMaterial mProperties
  poke p (AiMaterial mProperties) = do
    (#poke aiMaterial, mNumProperties) p $ length mProperties
    (#poke aiMaterial, mNumAllocated) p $ length mProperties
    newArray mProperties >>= (#poke aiMaterial, mProperties) p

instance Storable AiAnimation where
  sizeOf _ = #size aiAnimation
  alignment _ = #alignment aiAnimation
  peek p = do
    mName <- liftM aiStringToString $ (#peek aiAnimation, mName) p
    mDuration <- (#peek aiAnimation, mDuration) p
    mTicksPerSecond <- (#peek aiAnimation, mTicksPerSecond) p
    mNumChannels <- (#peek aiAnimation, mNumChannels) p
    mChannel <- (#peek aiAnimation, mChannels) p
    mChannels <- peekArray mNumChannels mChannel
    mNumMeshChannels <- (#peek aiAnimation, mNumMeshChannels) p
    mMeshChannel <- (#peek aiAnimation, mMeshChannels) p
    mMeshChannels <- peekArray mNumMeshChannels mMeshChannel
    return $ AiAnimation mName mDuration mTicksPerSecond mChannels mMeshChannels
  poke = undefined

instance Storable AiLight where
  sizeOf _ = #size aiLight
  alignment _ = #alignment aiLight
  peek p = do
    mName <- liftM aiStringToString $ (#peek aiLight, mName) p
    mType <- liftM toEnum $ (#peek aiLight, mType) p
    mPosition <- (#peek aiLight, mPosition) p
    mDirection <- (#peek aiLight, mDirection) p
    mAttenuationConstant <- (#peek aiLight, mAttenuationConstant) p
    mAttenuationLinear <- (#peek aiLight, mAttenuationLinear) p
    mAttenuationQuadratic <- (#peek aiLight, mAttenuationQuadratic) p
    mColorDiffuse <- (#peek aiLight, mColorDiffuse) p
    mColorSpecular <- (#peek aiLight, mColorSpecular) p
    mColorAmbient <- (#peek aiLight, mColorAmbient) p
    mAngleInnerCone <- (#peek aiLight, mAngleInnerCone) p
    mAngleOuterCone <- (#peek aiLight, mAngleOuterCone) p
    return $ AiLight mName mType mPosition mDirection mAttenuationConstant mAttenuationLinear mAttenuationQuadratic mColorDiffuse mColorSpecular mColorAmbient mAngleInnerCone mAngleOuterCone
  poke = undefined

instance Storable AiTexture where
  sizeOf _ = #size aiTexture
  alignment _ = #alignment aiTexture
  peek p = do
    mWidth <- (#peek aiTexture, mWidth) p
    mHeight <- (#peek aiTexture, mHeight) p
    achFormatHint <- (#peek aiTexture, achFormatHint) p >>= peekCString -- Should this be included?
    let pcData = []
    return $ AiTexture mWidth mHeight achFormatHint pcData
  poke = undefined

instance Storable AiCamera where
  sizeOf _ = #size aiCamera
  alignment _ = #alignment aiCamera
  peek p = do
    mName <- liftM aiStringToString $ (#peek aiCamera, mName) p
    mPosition <- (#peek aiCamera, mPosition) p
    mUp <- (#peek aiCamera, mUp) p
    mLookAt <- (#peek aiCamera, mLookAt) p
    mHorizontalFOV <- (#peek aiCamera, mHorizontalFOV) p
    mClipPlaneNear <- (#peek aiCamera, mClipPlaneNear) p
    mClipPlaneFar <- (#peek aiCamera, mClipPlaneFar) p
    mAspect <- (#peek aiCamera, mAspect) p
    return $ AiCamera mName mPosition mUp mLookAt mHorizontalFOV mClipPlaneNear mClipPlaneFar mAspect
  poke p (AiCamera mName mPosition mUp mLookAt mHorizontalFOV mClipPlaneNear mClipPlaneFar mAspect) = do
    (#poke aiCamera, mName) p $ AiString mName
    (#poke aiCamera, mPosition) p mPosition
    (#poke aiCamera, mUp) p mUp
    (#poke aiCamera, mLookAt) p mLookAt
    (#poke aiCamera, mHorizontalFOV) p mHorizontalFOV
    (#poke aiCamera, mClipPlaneNear) p mClipPlaneNear
    (#poke aiCamera, mClipPlaneFar) p mClipPlaneFar
    (#poke aiCamera, mAspect) p mAspect

instance Storable AiScene where
  sizeOf _ = #size aiScene
  alignment _ = #alignment aiScene
  peek p = do
    mFlags         <- liftM toEnum $ (#peek aiScene, mFlags) p
    mRootNodePtr   <- (#peek aiScene, mRootNode) p
    mRootNode      <- peek mRootNodePtr
    mNumMeshes     <- (#peek aiScene, mNumMeshes) p
    mMeshesP       <- (#peek aiScene, mMeshes) p :: IO (Ptr (Ptr AiMesh))
    mMeshes'       <- peekArray mNumMeshes mMeshesP :: IO [Ptr AiMesh]
    mMeshes        <- mapM peek mMeshes' :: IO [AiMesh]
    mNumMaterials  <- (#peek aiScene, mNumMaterials) p
    -- mMaterials     <- join $ liftM2 peekArray 
    --   ((#peek aiScene, mNumMaterials) p) ((#peek aiScene, mMaterials) p)
    mMaterialsP    <- (#peek aiScene, mMaterials) p :: IO (Ptr (Ptr AiMaterial))
    mMaterials'    <- peekArray mNumMaterials mMaterialsP :: IO [Ptr AiMaterial]
    mMaterials     <- mapM peek mMaterials' :: IO [AiMaterial]
    mNumAnimations <- (#peek aiScene, mNumAnimations) p
    mAnimations'   <- (#peek aiScene, mAnimations) p
    mAnimations    <- peekArrayPtr mNumAnimations mAnimations'
    mNumTextures   <- (#peek aiScene, mNumTextures) p
    mTextures'     <- (#peek aiScene, mTextures) p
    mTextures      <- peekArrayPtr mNumTextures mTextures'
    mNumLights     <- (#peek aiScene, mNumLights) p
    mLights'       <- (#peek aiScene, mLights) p
    mLights        <- peekArrayPtr mNumLights mLights'
    mNumCameras    <- (#peek aiScene, mNumCameras) p
    mCameras'      <- (#peek aiScene, mCameras) p
    mCameras       <- peekArrayPtr mNumCameras mCameras'
    return $ AiScene mFlags mRootNode mMeshes mMaterials mAnimations mTextures mLights mCameras
  poke p (AiScene flags mRootNode meshes materials animations textures lights cameras) = do
    (#poke aiScene, mFlags) p $ fromEnum flags 
    (#poke aiScene, mRootNode) p mRootNode
    (#poke aiScene, mNumMeshes) p $ length meshes
    newArray meshes >>= (#poke aiScene, mMeshes) p
    (#poke aiScene, mNumMaterials) p $ length materials
    newArray materials >>= (#poke aiScene, mMaterials) p
    (#poke aiScene, mNumAnimations) p $ length animations
    newArray animations >>= (#poke aiScene, mAnimations) p
    (#poke aiScene, mNumTextures) p $ length textures
    newArray textures >>= (#poke aiScene, mTextures) p
    (#poke aiScene, mNumLights) p $ length lights
    newArray lights >>= (#poke aiScene, mLights) p
    (#poke aiScene, mNumCameras) p $ length cameras
    newArray cameras >>= (#poke aiScene, mCameras) p
