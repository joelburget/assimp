{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module : Graphics.Formats.Assimp.Storable
-- Copyright : (c) Joel Burget 2011
-- License BSD3
--
-- Maintainer : Joel Burget <joelburget@gmail.com>
-- Stability : experimental
-- Portability : non-portable
--
-- Storable instances for all data types, we leave @poke@ undefined because
-- it is not ever used

module Graphics.Formats.Assimp.Storable where

import C2HS
import Prelude hiding (replicate)
import Foreign.Storable
import Control.Monad (liftM, liftM2, join)
import Control.Applicative ((<$>), (<*>))
import Data.Vect.Float (Vec2(..), Vec3(..), Vec4(..), Mat3(..), Mat4(..))

import Graphics.Formats.Assimp.Types

#include "assimp.h"        // Plain-C interface
#include "aiScene.h"       // Output data structure
#include "aiPostProcess.h" // Post processing flags
#include "./typedefs.h"

#include <stddef.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- Same as peekArray but checks for a null pointer
peekArray' :: Storable a => Int -> Ptr a -> IO [a]
peekArray' n ptr = if ptr == nullPtr
                   then return []
                   else peekArray n ptr

peekArrayPtr :: (Storable a) => Int -> (Ptr (Ptr a)) -> IO [a]
peekArrayPtr n p = peekArray' (fromIntegral n) p >>= mapM peek

instance Storable Plane3d where
  sizeOf _ = #size aiPlane
  alignment _ = #alignment aiPlane
  peek p = Plane3d <$> (#peek aiPlane, a) p
                   <*> (#peek aiPlane, b) p
                   <*> (#peek aiPlane, c) p
                   <*> (#peek aiPlane, d) p
  poke = undefined

instance Storable Ray where
  sizeOf _ = #size aiRay
  alignment _ = #alignment aiRay
  peek p = do
    (Vec3F pos) <- (#peek aiRay, pos) p
    (Vec3F dir) <- (#peek aiRay, dir) p
    return $ Ray pos dir
  poke = undefined

instance Storable Vec2F where
  sizeOf _ = #size aiVector2D
  alignment _ = #alignment aiVector2D
  peek p = Vec2F <$> (Vec2 <$> (#peek aiVector2D, x) p
                           <*> (#peek aiVector2D, y) p)
  poke = undefined

instance Storable Vec3F where
  sizeOf _ = #size aiVector3D
  alignment _ = #alignment aiVector3D
  peek p = Vec3F <$> (Vec3 <$> (#peek aiVector3D, x) p
                           <*> (#peek aiVector3D, y) p
                           <*> (#peek aiVector3D, z) p)
  poke = undefined

instance Storable Color3F where
  sizeOf _ = #size aiColor3D
  alignment _ = #alignment aiColor3D
  peek p = Color3F <$> (Vec3 <$> (#peek aiColor3D, r) p
                             <*> (#peek aiColor3D, g) p
                             <*> (#peek aiColor3D, b) p)
  poke = undefined

instance Storable Color4F where
  sizeOf _ = #size aiColor4D
  alignment _ = #alignment aiColor4D
  peek p = Color4F <$> (Vec4 <$> (#peek aiColor4D, r) p
                             <*> (#peek aiColor4D, g) p
                             <*> (#peek aiColor4D, b) p
                             <*> (#peek aiColor4D, a) p)
  poke = undefined

instance Storable MemoryInfo where
  sizeOf _ = #size aiMemoryInfo
  alignment _ = #alignment aiMemoryInfo
  peek p = do
    text       <- (#peek aiMemoryInfo, textures) p
    materials  <- (#peek aiMemoryInfo, materials) p
    meshes     <- (#peek aiMemoryInfo, meshes) p
    nodes      <- (#peek aiMemoryInfo, nodes) p
    animations <- (#peek aiMemoryInfo, animations) p
    cameras    <- (#peek aiMemoryInfo, cameras) p
    lights     <- (#peek aiMemoryInfo, lights) p
    total      <- (#peek aiMemoryInfo, total) p
    return $ MemoryInfo text materials meshes nodes animations cameras lights 
                        total
  poke = undefined

instance Storable Quaternion where
  sizeOf _ = #size aiQuaternion
  alignment _ = #alignment aiQuaternion
  peek p = Quaternion <$> (#peek aiQuaternion, w) p
                      <*> (#peek aiQuaternion, w) p
                      <*> (#peek aiQuaternion, w) p
                      <*> (#peek aiQuaternion, w) p
  poke = undefined

instance Storable AiString where
  sizeOf _ = #size aiString
  alignment _ = #alignment aiString
  peek p = do
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
  poke = undefined

aiStringToString :: AiString -> String
aiStringToString (AiString s) = s

instance Storable Mat3F where
  sizeOf _ = #size aiMatrix3x3
  alignment _ = #alignment aiMatrix3x3
  peek p = do
    a1 <- (#peek aiMatrix3x3, a1) p
    a2 <- (#peek aiMatrix3x3, a2) p
    a3 <- (#peek aiMatrix3x3, a3) p
    b1 <- (#peek aiMatrix3x3, b1) p
    b2 <- (#peek aiMatrix3x3, b2) p
    b3 <- (#peek aiMatrix3x3, b3) p
    c1 <- (#peek aiMatrix3x3, c1) p
    c2 <- (#peek aiMatrix3x3, c2) p
    c3 <- (#peek aiMatrix3x3, c3) p
    return $ Mat3F $ Mat3
      (Vec3 a1 a2 a3)
      (Vec3 b1 b2 b3)
      (Vec3 c1 c2 c3)
  poke = undefined

instance Storable Mat4F where
  sizeOf _ = #size aiMatrix4x4
  alignment _ = #alignment aiMatrix4x4
  peek p = do
    a1 <- (#peek aiMatrix4x4, a1) p
    a2 <- (#peek aiMatrix4x4, a2) p
    a3 <- (#peek aiMatrix4x4, a3) p
    a4 <- (#peek aiMatrix4x4, a4) p
    b1 <- (#peek aiMatrix4x4, b1) p
    b2 <- (#peek aiMatrix4x4, b2) p
    b3 <- (#peek aiMatrix4x4, b3) p
    b4 <- (#peek aiMatrix4x4, b4) p
    c1 <- (#peek aiMatrix4x4, c1) p
    c2 <- (#peek aiMatrix4x4, c2) p
    c3 <- (#peek aiMatrix4x4, c3) p
    c4 <- (#peek aiMatrix4x4, c4) p
    d1 <- (#peek aiMatrix4x4, d1) p
    d2 <- (#peek aiMatrix4x4, d2) p
    d3 <- (#peek aiMatrix4x4, d3) p
    d4 <- (#peek aiMatrix4x4, d4) p
    return $ Mat4F $ Mat4
      (Vec4 a1 a2 a3 a4)
      (Vec4 b1 b2 b3 b4)
      (Vec4 c1 c2 c3 c4)
      (Vec4 d1 d2 d3 d4)
  poke = undefined

instance Storable Node where
  sizeOf _ = #size aiNode
  alignment _ = #alignment aiNode
  peek p = do
    mName           <- liftM aiStringToString $ (#peek aiNode, mName) p
    mTransformation <- (#peek aiNode, mTransformation) p
    -- mParent      <- if mParentPtr == nullPtr 
    --                 then return Nothing 
    --                 else (#peek aiNode, mParent) p
    -- Temporary workaround so we don't end up in an infinite loop
    let mParent = Nothing 
    mNumChildren    <- (#peek aiNode, mNumChildren) p :: IO CUInt
    mChildrenP''    <- (#peek aiNode ,mChildren) p >>=
                         peekArray' (fromIntegral mNumChildren)
    mChildren       <- mapM peek mChildrenP''
    mNumMeshes      <- (#peek aiNode, mNumMeshes) p :: IO CUInt
    mMeshes         <- (#peek aiNode, mMeshes) p >>=
                         peekArray (fromIntegral mNumMeshes)
    return $ Node mName mTransformation mParent mChildren mMeshes
  poke = undefined

instance Storable Face where
  sizeOf _ = #size aiFace
  alignment _ = #alignment aiFace
  peek p = do
    mNumIndices <- (#peek aiFace, mNumIndices) p :: IO CUInt
    mIndices <- (#peek aiFace, mIndices) p
    lst <- peekArray (fromIntegral mNumIndices) mIndices
    return $ Face lst
  poke = undefined

instance Storable VertexWeight where
  sizeOf _ = #size aiVertexWeight
  alignment _ = #alignment aiVertexWeight
  peek p = VertexWeight <$> (#peek aiVertexWeight, mVertexId) p
                        <*> (#peek aiVertexWeight, mWeight) p
  poke = undefined

instance Storable Bone where
  sizeOf _ = #size aiBone
  alignment _ = #alignment aiBone
  peek p = do
    mN <- liftM aiStringToString $ (#peek aiBone, mName) p
    mNW <- (#peek aiBone, mNumWeights) p
    mW <- (#peek aiBone, mWeights) p
    lst <- peekArray mNW mW
    mO <- (#peek aiBone, mOffsetMatrix) p
    return $ Bone mN lst mO
  poke = undefined

toEnumList :: Enum a => CUInt -> [a]
toEnumList ls =
  -- Find the places where the bits are set in ls, then convert them to b.
  let stage1 = map (ls .&.) $ map (2^) [0..(bitSize (undefined::CUInt))]
      stage2 = filter (>0) stage1
  in map (toEnum . fromIntegral) stage2

instance Storable Mesh where
  sizeOf _ = #size aiMesh
  alignment _ = #alignment aiMesh
  peek p = do
    -- Note for some reason I had to shift the bits right by 1 but I don't
    -- think that should have been necessary.
    mPrimitiveTypes  <- liftM (toEnumList . (flip shiftR 1))
                        ((#peek aiMesh, mPrimitiveTypes) p :: IO CUInt)
    mNumVs           <- liftM fromIntegral
                        $ ((#peek aiMesh, mNumVertices) p :: IO CUInt)
    mVertices        <- liftM (map (\(Vec3F x) -> x))   
                        $ (#peek aiMesh, mVertices) p      >>= peekArray  mNumVs
    mNormals         <- liftM (map (\(Vec3F x) -> x))   
                        $ (#peek aiMesh, mNormals) p       >>= peekArray' mNumVs
    mTangents        <- liftM (map (\(Vec3F x) -> x))   
                        $ (#peek aiMesh, mTangents) p      >>= peekArray' mNumVs
    mBitangents      <- liftM (map (\(Vec3F x) -> x))   
                        $ (#peek aiMesh, mBitangents) p    >>= peekArray' mNumVs
    mColors          <- liftM (map (\(Color4F x) -> x)) 
                        $ (#peek aiMesh, mColors) p        >>= peekArray' mNumVs
    mTextureCoords   <- liftM (map (\(Vec3F x) -> x))   
                        $ (#peek aiMesh, mTextureCoords) p >>= peekArray' mNumVs
    mNumUVComponents <- (#peek aiMesh, mNumUVComponents) p
    mNumFaces        <- liftM fromIntegral 
                        ((#peek aiMesh, mNumFaces) p :: IO CUInt)
    mFaces           <- (#peek aiMesh, mFaces) p >>= peekArray mNumFaces
    mBones           <- join $ peekArrayPtr <$> ((#peek aiMesh, mNumBones) p)
                                            <*> ((#peek aiMesh, mBones) p)
    mMaterialIndex   <- (#peek aiMesh, mMaterialIndex) p
    mName            <- liftM aiStringToString $ (#peek aiMesh, mName) p
    return $ Mesh
               mPrimitiveTypes mVertices mNormals mTangents mBitangents
               mColors mTextureCoords mNumUVComponents mFaces mBones
               mMaterialIndex mName
  poke = undefined

instance Storable MaterialProperty where
  sizeOf _ = #size aiMaterialProperty
  alignment _ = #alignment aiMaterialProperty
  peek p = do
    mKey <- liftM aiStringToString $ (#peek aiMaterialProperty, mKey) p
    mSemantic <- liftM (cToEnum :: CUInt -> TextureType) $ 
                   (#peek aiMaterialProperty, mSemantic) p
    mIndex <- (#peek aiMaterialProperty, mIndex) p
    -- mType <- liftM toEnum $ (#peek aiMaterialProperty, mType) p
    mData <- (#peek aiMaterialProperty, mData) p >>= peekCString
    -- return $ MaterialProperty mKey mSemantic mIndex mType mData
    return $ MaterialProperty mKey mSemantic mIndex mData
  poke = undefined

instance Storable NodeAnim where
  sizeOf _ = #size aiNodeAnim
  alignment _ = #alignment aiNodeAnim
  peek _ = return $ NodeAnim 0
  poke = undefined

instance Storable MeshAnim where
  sizeOf _ = #size aiMeshAnim
  alignment _ = #alignment aiMeshAnim
  peek _ = return $ MeshAnim 0
  poke = undefined

instance Storable Material where
  sizeOf _ = #size aiMaterial
  alignment _ = #alignment aiMaterial
  peek p = do
    mProperties <- join $ liftM2 peekArrayPtr
      (liftM fromIntegral ((#peek aiMaterial, mNumProperties) p :: IO CUInt))
      ((#peek aiMaterial, mProperties) p)
    return $ Material mProperties
  poke = undefined

instance Storable Animation where
  sizeOf _ = #size aiAnimation
  alignment _ = #alignment aiAnimation
  peek p = do
    mName            <- liftM aiStringToString $ (#peek aiAnimation, mName) p
    mDuration        <- (#peek aiAnimation, mDuration) p
    mTicksPerSecond  <- (#peek aiAnimation, mTicksPerSecond) p
    mNumChannels     <- (#peek aiAnimation, mNumChannels) p
    mChannels'       <- (#peek aiAnimation, mChannels) p
    mChannels        <- peekArray mNumChannels mChannels'
    mNumMeshChannels <- (#peek aiAnimation, mNumMeshChannels) p
    mMeshChannels'   <- (#peek aiAnimation, mMeshChannels) p
    mMeshChannels    <- peekArray mNumMeshChannels mMeshChannels'
    return $ Animation mName mDuration mTicksPerSecond mChannels mMeshChannels
  poke = undefined

instance Storable Light where
  sizeOf _ = #size aiLight
  alignment _ = #alignment aiLight
  peek p = do
    mName                    <- liftM aiStringToString $ (#peek aiLight, mName) p
    mType                    <- liftM toEnum $ (#peek aiLight, mType) p
    (Vec3F mPosition)        <- (#peek aiLight, mPosition) p
    (Vec3F mDirection)       <- (#peek aiLight, mDirection) p
    mAttenuationConstant     <- (#peek aiLight, mAttenuationConstant) p
    mAttenuationLinear       <- (#peek aiLight, mAttenuationLinear) p
    mAttenuationQuadratic    <- (#peek aiLight, mAttenuationQuadratic) p
    (Color3F mColorDiffuse)  <- (#peek aiLight, mColorDiffuse) p
    (Color3F mColorSpecular) <- (#peek aiLight, mColorSpecular) p
    (Color3F mColorAmbient)  <- (#peek aiLight, mColorAmbient) p
    mAngleInnerCone          <- (#peek aiLight, mAngleInnerCone) p
    mAngleOuterCone          <- (#peek aiLight, mAngleOuterCone) p
    return $ Light
               mName mType mPosition mDirection mAttenuationConstant
               mAttenuationLinear mAttenuationQuadratic mColorDiffuse
               mColorSpecular mColorAmbient mAngleInnerCone mAngleOuterCone
  poke = undefined

instance Storable Texture where
  sizeOf _ = #size aiTexture
  alignment _ = #alignment aiTexture
  peek p = do
    mWidth <- (#peek aiTexture, mWidth) p
    mHeight <- (#peek aiTexture, mHeight) p
    -- Should achFormatHint be included?
    achFormatHint <- (#peek aiTexture, achFormatHint) p >>= peekCString
    pcData' <- (#peek aiTexture, pcData) p
    pcData <- if mHeight == 0
              then peekArray (fromIntegral mWidth) pcData'
              else peekArray (fromIntegral (mWidth * mHeight)) pcData'
    return $ Texture mWidth mHeight achFormatHint pcData
  poke = undefined

instance Storable Texel where
  sizeOf _ = #size aiTexel
  alignment _ = #alignment aiTexel
  peek _ = return $ Texel 0
  poke = undefined

instance Storable Camera where
  sizeOf _ = #size aiCamera
  alignment _ = #alignment aiCamera
  peek p = do
    mName <- liftM aiStringToString $ (#peek aiCamera, mName) p
    (Vec3F mPosition) <- (#peek aiCamera, mPosition) p
    (Vec3F mUp) <- (#peek aiCamera, mUp) p
    (Vec3F mLookAt) <- (#peek aiCamera, mLookAt) p
    mHorizontalFOV <- (#peek aiCamera, mHorizontalFOV) p
    mClipPlaneNear <- (#peek aiCamera, mClipPlaneNear) p
    mClipPlaneFar <- (#peek aiCamera, mClipPlaneFar) p
    mAspect <- (#peek aiCamera, mAspect) p
    return $ Camera mName mPosition mUp mLookAt mHorizontalFOV mClipPlaneNear 
                    mClipPlaneFar mAspect
  poke = undefined

instance Storable Scene where
  sizeOf _ = #size aiScene
  alignment _ = #alignment aiScene
  peek p = do
    mFlags         <- liftM toEnumList $ ((#peek aiScene, mFlags) p :: IO CUInt)
    mRootNode      <- (#peek aiScene, mRootNode) p >>= peek
    mNumMeshes     <- (#peek aiScene, mNumMeshes) p :: IO CUInt
    mMeshes'       <- (#peek aiScene, mMeshes) p >>= 
                        peekArray (fromIntegral mNumMeshes)
    mMeshes        <- mapM peek mMeshes'
    mNumMaterials  <- (#peek aiScene, mNumMaterials) p :: IO CUInt
    mMaterials'    <- (#peek aiScene, mMaterials) p >>= 
                        peekArray (fromIntegral mNumMaterials)
    mMaterials     <- mapM peek mMaterials'
    mNumAnimations <- (#peek aiScene, mNumAnimations) p :: IO CUInt
    mAnimations    <- (#peek aiScene, mAnimations) p >>= 
                        peekArrayPtr (fromIntegral mNumAnimations)
    mNumTextures   <- (#peek aiScene, mNumTextures) p :: IO CUInt
    mTextures      <- (#peek aiScene, mTextures) p >>= 
                        peekArrayPtr (fromIntegral mNumTextures)
    mNumLights     <- (#peek aiScene, mNumLights) p :: IO CUInt
    mLights        <- (#peek aiScene, mLights) p >>= 
                        peekArrayPtr (fromIntegral mNumLights)
    mNumCameras    <- (#peek aiScene, mNumCameras) p :: IO CUInt
    mCameras       <- (#peek aiScene, mCameras) p >>= 
                        peekArrayPtr (fromIntegral mNumCameras)
    return $ Scene mFlags mRootNode mMeshes mMaterials mAnimations mTextures
                   mLights mCameras
  poke = undefined
