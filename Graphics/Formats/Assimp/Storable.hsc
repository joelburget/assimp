{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.Formats.Assimp.Storable where

import Prelude hiding (replicate)
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Array
import Control.Monad (liftM)
import Data.Vector.Storable hiding (length)

import Graphics.Formats.Assimp.Types

#include "../../assimp/include/assimp.h"        // Plain-C interface
#include "../../assimp/include/aiScene.h"       // Output data structure
#include "../../assimp/include/aiPostProcess.h" // Post processing flags
#include "./typedefs.h"

instance Storable AiPlane where
  sizeOf _ = #{size aiPlane}
  alignment _ = 4
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
  sizeOf _ = (#size aiRay)
  alignment _ = 4
  peek p = do
    pos <- (#peek aiRay, pos) p
    dir <- (#peek aiRay, dir) p
    return $ AiRay pos dir
  poke p (AiRay pos dir) = do
    (#poke aiRay, pos) p pos
    (#poke aiRay, dir) p dir

instance Storable AiColor3D where
  sizeOf _ = (#size aiColor3D)
  alignment _ = 4
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
  sizeOf _ = (#size aiColor4D)
  alignment _ = 4
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
  sizeOf _ = (#size aiMemoryInfo)
  alignment _ = 4
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
  sizeOf _ = (#size aiQuaternion)
  alignment _ = 4
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
  sizeOf _ = (#size aiVector2D)
  alignment _ = 4
  peek p = do
    x <- (#peek aiVector2D, x) p
    y <- (#peek aiVector2D, y) p
    return $ AiVector2D x y
  poke p (AiVector2D x y) = do
    (#poke aiVector2D, x) p x
    (#poke aiVector2D, y) p y

instance Storable AiVector3D where
  sizeOf _ = (#size aiVector3D)
  alignment _ = 4
  peek p = do
    x <- (#peek aiVector3D, x) p
    y <- (#peek aiVector3D, y) p
    z <- (#peek aiVector3D, z) p
    return $ AiVector3D x y z
  poke p (AiVector3D x y z) = do
    (#poke aiVector3D, x) p x
    (#poke aiVector3D, y) p y
    (#poke aiVector3D, z) p z

-- I think this will have to change to support wide characters
instance Storable AiString where
  sizeOf _ = (#size aiString)
  alignment _ = 4
  -- peek p = do
  --   -- len <- (#peek aiString, length) p
  --   dat <- (#peek aiString, data) p
  --   return $ AiString dat
  peek = undefined
  poke p (AiString dat) = do
    (#poke aiString, length) p $ length dat
    str <- newCString $ dat
    (#poke aiString, data) p str

instance Storable AiMatrix3x3 where
  sizeOf _ = (#size aiMatrix3x3)
  alignment _ = 4
  peek p = return $ AiMatrix3x3 $ replicate 9 0
  poke = undefined

instance Storable AiMatrix4x4 where
  sizeOf _ = (#size aiMatrix4x4)
  alignment _ = 4
  peek p = return $ AiMatrix4x4 $ replicate 16 0
  poke = undefined

instance Storable AiNode where
  sizeOf _ = (#size aiNode)
  alignment _ = 4
  peek p = do
    mName <- (#peek aiNode, mName) p >>= peekCString 
    mTransformation <- (#peek aiNode, mTransformation) p
    mParent <- (#peek aiNode, mParent) p
    mNumChildren <- (#peek aiNode, mNumChildren) p
    mChildrenP <- (#peek aiNode, mChildren) p
    mChildren <- peekArray mNumChildren mChildrenP
    mMN <- (#peek aiNode, mNumMeshes) p 
    mMP <- (#peek aiNode, mMeshes) p
    mMeshes <- peekArray mMN mMP
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
  sizeOf _ = (#size aiFace)
  alignment _ = 4
  peek p = do
    mNumIndices <- (#peek aiFace, mNumIndices) p
    mIndices <- (#peek aiFace, mIndices) p
    lst <- peekArray mNumIndices mIndices
    return $ AiFace lst
  poke p (AiFace mIndices) = do
    (#poke aiFace, mNumIndices) p $ length mIndices
    newArray mIndices >>= ((#poke aiFace, mIndices) p)

instance Storable AiVertexWeight where
  sizeOf _ = (#size aiVertexWeight)
  alignment _ = 4
  peek p = do
    mV <- (#peek aiVertexWeight, mVertexId) p
    mW <- (#peek aiVertexWeight, mWeight) p
    return $ AiVertexWeight mV mW
  poke p (AiVertexWeight mV mW) = do
    (#poke aiVertexWeight, mVertexId) p mV
    (#poke aiVertexWeight, mWeight) p mW

instance Storable AiBone where
  sizeOf _ = (#size aiBone)
  alignment _ = 4
  peek p = do
    mN <- (#peek aiBone, mName) p >>= peekCString
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
  sizeOf _ = (#size aiMesh)
  alignment _ = 4
  peek p = do
    mPrimitiveTypes <- liftM toEnum $ (#peek aiMesh, mPrimitiveTypes) p
    return $ AiMesh mPrimitiveTypes [] [] [] [] [] [] 0 [] [] 0 "Nothing"
  poke = undefined

instance Storable AiMaterialProperty where
  sizeOf _ = (#size aiMaterialProperty)
  alignment _ = 4
  peek p = do
    mKey <- (#peek aiMaterialProperty, mKey) p >>= peekCString
    mSemantic <- liftM toEnum $ (#peek aiMaterialProperty, mSemantic) p
    mIndex <- (#peek aiMaterialProperty, mIndex) p
    mType <- liftM toEnum $ (#peek aiMaterialProperty, mType) p
    mData <- (#peek aiMaterialProperty, mData) p >>= peekCString
    return $ AiMaterialProperty mKey mSemantic mIndex mType mData
  poke p (AiMaterialProperty mKey mSemantic mIndex mType mData) = do
    (#poke aiMaterialProperty, mKey) p $ AiString mKey
    (#poke aiMaterialProperty, mSemantic) p $ fromEnum mSemantic
    (#poke aiMaterialProperty, mIndex) p mIndex
    (#poke aiMaterialProperty, mType) p $ fromEnum mType
    (#poke aiMaterialProperty, mData) p $ AiString mData

instance Storable AiNodeAnim where
  sizeOf _ = (#size aiNodeAnim)
  alignment _ = 4
  peek _ = return $ AiNodeAnim 0
  poke _ _ = return ()

instance Storable AiMeshAnim where
  sizeOf _ = (#size aiMeshAnim)
  alignment _ = 4
  peek _ = return $ AiMeshAnim 0
  poke _ _ = return ()

instance Storable AiMaterial where
  sizeOf _ = (#size aiMaterial)
  alignment _ = 4
  peek p = do
    mNumProperties <- (#peek aiMaterial, mNumProperties) p
    mProperty      <- (#peek aiMaterial, mProperties) p
    mProperties    <- peekArray mNumProperties mProperty
    return $ AiMaterial mProperties
  poke p (AiMaterial mProperties) = do
    (#poke aiMaterial, mNumProperties) p $ length mProperties
    (#poke aiMaterial, mNumAllocated) p $ length mProperties
    newArray mProperties >>= (#poke aiMaterial, mProperties) p

instance Storable AiAnimation where
  sizeOf _ = (#size aiAnimation)
  alignment _ = 4
  peek p = do
    mName <- (#peek aiAnimation, mName) p >>= peekCString
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
  sizeOf _ = (#size aiLight)
  alignment _ = 4
  peek p = do
    mName <- (#peek aiLight, mName) p >>= peekCString
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
  sizeOf _ = (#size aiTexture)
  alignment _ = 4
  peek p = do
    mWidth <- (#peek aiTexture, mWidth) p
    mHeight <- (#peek aiTexture, mHeight) p
    achFormatHint <- (#peek aiTexture, achFormatHint) p >>= peekCString -- Should this be included?
    let pcData = []
    return $ AiTexture mWidth mHeight achFormatHint pcData
  poke = undefined

instance Storable AiCamera where
  sizeOf _ = (#size aiCamera)
  alignment _ = 4
  peek p = do
    mName <- (#peek aiCamera, mName) p >>= peekCString 
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
  sizeOf _ = (#size aiScene)
  alignment _ = 4
  peek p = do
    mFlags         <- liftM toEnum $ (#peek aiScene, mFlags) p
    mNumMeshes     <- (#peek aiScene, mNumMeshes) p
    mMesh          <- (#peek aiScene, mMeshes) p
    mMeshes        <- peekArray mNumMeshes mMesh
    mNumMaterials  <- (#peek aiScene, mNumMaterials) p
    mMaterial      <- (#peek aiScene, mMaterials) p
    mMaterials     <- peekArray mNumMaterials mMaterial
    mNumAnimations <- (#peek aiScene, mNumAnimations) p
    mAnimation     <- (#peek aiScene, mAnimations) p
    mAnimations    <- peekArray mNumAnimations mAnimation
    mNumTextures   <- (#peek aiScene, mNumTextures) p
    mTexture       <- (#peek aiScene, mTextures) p
    mTextures      <- peekArray mNumTextures mTexture
    mNumLights     <- (#peek aiScene, mNumLights) p
    mLight         <- (#peek aiScene, mLights) p
    mLights        <- peekArray mNumLights mLight
    mNumCameras    <- (#peek aiScene, mNumCameras) p
    mCamera        <- (#peek aiScene, mCameras) p
    mCameras       <- peekArray mNumCameras mCamera
    return $ AiScene mFlags mMeshes mMaterials mAnimations mTextures mLights mCameras
  poke p (AiScene flags meshes materials animations textures lights cameras) = do
    (#poke aiScene, mFlags) p $ fromEnum flags 
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
