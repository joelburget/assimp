{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.Formats.Assimp.Storable where

import C2HS
import Prelude hiding (replicate)
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Array
import Control.Monad (liftM, liftM2, (>>=), join)
import Control.Applicative ((<$>), (<*>))
import Data.Vector (Vector, fromList)
import Data.Bits ((.&.))

import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Vec

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

instance Storable Plane where
  sizeOf _ = #size aiPlane
  alignment _ = #alignment aiPlane
  peek p = do
    a <- (#peek aiPlane, a) p
    b <- (#peek aiPlane, b) p
    c <- (#peek aiPlane, c) p
    d <- (#peek aiPlane, d) p
    return $ Plane a b c d
  poke p (Plane a b c d) = do
    (#poke aiPlane, a) p a
    (#poke aiPlane, b) p b
    (#poke aiPlane, c) p c
    (#poke aiPlane, d) p d

instance Storable Ray where
  sizeOf _ = #size aiRay
  alignment _ = #alignment aiRay
  peek p = do
    pos <- (#peek aiRay, pos) p
    dir <- (#peek aiRay, dir) p
    return $ Ray pos dir
  poke p (Ray pos dir) = do
    (#poke aiRay, pos) p pos
    (#poke aiRay, dir) p dir

instance Storable Vec2D where
  sizeOf _ = #size aiVector2D
  alignment _ = #alignment aiVector2D
  peek p = do
    x <- (#peek aiVector2D, x) p
    y <- (#peek aiVector2D, y) p
    return $ Vec2D x y
  poke p (Vec2D x y) = do
    (#poke aiVector2D, x) p x
    (#poke aiVector2D, y) p y

instance Storable Vec3D where
  sizeOf _ = #size aiVector3D
  alignment _ = #alignment aiVector3D
  peek p = do
    x <- (#peek aiVector3D, x) p
    y <- (#peek aiVector3D, y) p
    z <- (#peek aiVector3D, z) p
    return $ Vec3D x y z
  poke p (Vec3D x y z) = do
    (#poke aiVector3D, x) p x
    (#poke aiVector3D, y) p y
    (#poke aiVector3D, z) p z

instance Storable Color3D where
  sizeOf _ = #size aiColor3D
  alignment _ = #alignment aiColor3D
  peek p = do
    r <- (#peek aiColor3D, r) p
    g <- (#peek aiColor3D, g) p
    b <- (#peek aiColor3D, b) p
    return $ Vec3D r g b
  poke p (Vec3D r g b) = do
    (#poke aiColor3D, r) p r
    (#poke aiColor3D, g) p g
    (#poke aiColor3D, b) p b

instance Storable Color4D where
  sizeOf _ = #size aiColor4D
  alignment _ = #alignment aiColor4D
  peek p = do
    r <- (#peek aiColor4D, r) p
    g <- (#peek aiColor4D, g) p
    b <- (#peek aiColor4D, b) p
    a <- (#peek aiColor4D, a) p
    return $ Vec4D r g b a
  poke p (Vec4D r g b a) = do
    (#poke aiColor4D, r) p r
    (#poke aiColor4D, g) p g
    (#poke aiColor4D, b) p b
    (#poke aiColor4D, a) p a

instance Storable MemoryInfo where
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
    return $ MemoryInfo text materials meshes nodes animations cameras lights total
  poke p (MemoryInfo te ma me no an ca li to) = do
    (#poke aiMemoryInfo, textures) p te
    (#poke aiMemoryInfo, materials) p ma
    (#poke aiMemoryInfo, meshes) p me
    (#poke aiMemoryInfo, nodes) p no
    (#poke aiMemoryInfo, animations) p an
    (#poke aiMemoryInfo, cameras) p ca
    (#poke aiMemoryInfo, lights) p li
    (#poke aiMemoryInfo, total) p to

instance Storable Quaternion where
  sizeOf _ = #size aiQuaternion
  alignment _ = #alignment aiQuaternion
  peek p = do
    w <- (#peek aiQuaternion, w) p
    x <- (#peek aiQuaternion, x) p
    y <- (#peek aiQuaternion, y) p
    z <- (#peek aiQuaternion, z) p
    return $ Quaternion w x y z
  poke p (Quaternion w x y z) = do
    (#poke aiQuaternion, w) p w
    (#poke aiQuaternion, x) p x
    (#poke aiQuaternion, y) p y
    (#poke aiQuaternion, z) p z

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

instance Storable Matrix3x3 where
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
    return $ Matrix3x3 $ fromList $ map fromList 
      [
        [a1,a2,a3]
      , [b1,b2,b3]
      , [c1,c2,c3]
      ]
  poke = undefined

instance Storable Matrix4x4 where
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
    return $ Matrix4x4 $ fromList $ map fromList 
      [ [a1,a2,a3,a4]
      , [b1,b2,b3,b4]
      , [c1,c2,c3,c4]
      , [d1,d2,d3,d4]
      ]
  poke = undefined

instance Storable Node where
  sizeOf _ = #size aiNode
  alignment _ = #alignment aiNode
  peek p = do
    mName <- liftM aiStringToString $ (#peek aiNode, mName) p
    mTransformation <- (#peek aiNode, mTransformation) p
    -- mParentPtr <- (#peek aiNode, mParent) p
    -- mParent <- if mParentPtr == nullPtr then return Nothing else return peek mParentPtr
    let mParent = Nothing -- Temporary workaround so we don't end up in an infinite loop
    mNumChildren <- (#peek aiNode, mNumChildren) p :: IO CUInt
    mChildrenP' <- (#peek aiNode, mChildren) p :: IO (Ptr (Ptr Node))
    mChildrenP'' <- peekArray' (fromIntegral mNumChildren) mChildrenP' :: IO [Ptr Node]
    mChildren <- mapM peek mChildrenP''
    mNumMeshes <- (#peek aiNode, mNumMeshes) p :: IO CUInt
    mMeshes' <- (#peek aiNode, mMeshes) p
    mMeshes <- peekArray (fromIntegral mNumMeshes) mMeshes'
    return $ Node mName mTransformation mParent mChildren mMeshes
  poke p (Node name trans par chil mes) = do
    (#poke aiNode, mName) p $ AiString name
    (#poke aiNode, mTransformation) p trans
    (#poke aiNode, mParent) p par
    (#poke aiNode, mNumChildren) p $ length chil
    newArray chil >>= (#poke aiNode, mChildren) p
    (#poke aiNode, mNumMeshes) p $ length mes
    newArray mes >>= (#poke aiNode, mMeshes) p

instance Storable Face where
  sizeOf _ = #size aiFace
  alignment _ = #alignment aiFace
  peek p = do
    mNumIndices <- liftM fromIntegral ((#peek aiFace, mNumIndices) p :: IO CUInt) :: IO Int
    mIndices <- (#peek aiFace, mIndices) p
    lst <- peekArray mNumIndices mIndices
    return $ Face lst
  poke p (Face mIndices) = do
    (#poke aiFace, mNumIndices) p $ length mIndices
    newArray mIndices >>= ((#poke aiFace, mIndices) p)

instance Storable VertexWeight where
  sizeOf _ = #size aiVertexWeight
  alignment _ = #alignment aiVertexWeight
  peek p = do
    mV <- (#peek aiVertexWeight, mVertexId) p
    mW <- (#peek aiVertexWeight, mWeight) p
    return $ VertexWeight mV mW
  poke p (VertexWeight mV mW) = do
    (#poke aiVertexWeight, mVertexId) p mV
    (#poke aiVertexWeight, mWeight) p mW

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
  poke p (Bone mN mW mO) = do
    (#poke aiBone, mName) p $ AiString mN
    (#poke aiBone, mNumWeights) p $ length mW
    newArray mW >>= ((#poke aiBone, mWeights) p)
    (#poke aiBone, mOffsetMatrix) p mO

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
    mPrimitiveTypes <- liftM (toEnumList . (flip shiftR 1)) 
                              ((#peek aiMesh, mPrimitiveTypes) p :: IO CUInt)
    mNumVs <- liftM fromIntegral $ 
                ((#peek aiMesh, mNumVertices) p :: IO CUInt)
    mVertices    <- (#peek aiMesh, mVertices) p        >>= peekArray  mNumVs
    mNormals     <- (#peek aiMesh, mNormals) p         >>= peekArray' mNumVs
    mTangents    <- (#peek aiMesh, mTangents) p        >>= peekArray' mNumVs
    mBitangents  <- (#peek aiMesh, mBitangents) p      >>= peekArray' mNumVs
    mColors      <- (#peek aiMesh, mColors) p          >>= peekArray' mNumVs
    mTextureCoords <- (#peek aiMesh, mTextureCoords) p >>= peekArray' mNumVs
    mNumUVComponents <- (#peek aiMesh, mNumUVComponents) p
    mNumFaces <- liftM fromIntegral ((#peek aiMesh, mNumFaces) p :: IO CUInt)
    mFaces <- (#peek aiMesh, mFaces) p >>= peekArray' mNumFaces
    mBones <- join $ liftM2 peekArrayPtr ((#peek aiMesh, mNumBones) p) 
                                         ((#peek aiMesh, mBones) p)
    mMaterialIndex <- (#peek aiMesh, mMaterialIndex) p
    mName <- liftM aiStringToString $ (#peek aiMesh, mName) p
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
    mSemantic <- liftM (cToEnum :: CUInt -> TextureType) $ (#peek aiMaterialProperty, mSemantic) p
    mIndex <- (#peek aiMaterialProperty, mIndex) p
    -- mType <- liftM toEnum $ (#peek aiMaterialProperty, mType) p
    mData <- (#peek aiMaterialProperty, mData) p >>= peekCString
    -- return $ MaterialProperty mKey mSemantic mIndex mType mData
    return $ MaterialProperty mKey mSemantic mIndex mData
  poke p (MaterialProperty mKey mSemantic mIndex mData) = do
    (#poke aiMaterialProperty, mKey) p $ AiString mKey
    (#poke aiMaterialProperty, mSemantic) p $ fromEnum mSemantic
    (#poke aiMaterialProperty, mIndex) p mIndex
    -- (#poke aiMaterialProperty, mType) p $ fromEnum mType
    (#poke aiMaterialProperty, mData) p $ AiString mData

instance Storable NodeAnim where
  sizeOf _ = #size aiNodeAnim
  alignment _ = #alignment aiNodeAnim
  peek _ = return $ NodeAnim 0
  poke _ _ = return ()

instance Storable MeshAnim where
  sizeOf _ = #size aiMeshAnim
  alignment _ = #alignment aiMeshAnim
  peek _ = return $ MeshAnim 0
  poke _ _ = return ()

instance Storable Material where
  sizeOf _ = #size aiMaterial
  alignment _ = #alignment aiMaterial
  peek p = do
    mProperties <- join $ liftM2 peekArrayPtr 
      (liftM fromIntegral ((#peek aiMaterial, mNumProperties) p :: IO CUInt)) 
      ((#peek aiMaterial, mProperties) p)
    return $ Material mProperties
  poke p (Material mProperties) = do
    (#poke aiMaterial, mNumProperties) p $ length mProperties
    (#poke aiMaterial, mNumAllocated) p $ length mProperties
    newArray mProperties >>= (#poke aiMaterial, mProperties) p

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
    mName                 <- liftM aiStringToString $ (#peek aiLight, mName) p
    mType                 <- liftM toEnum $ (#peek aiLight, mType) p
    mPosition             <- (#peek aiLight, mPosition) p
    mDirection            <- (#peek aiLight, mDirection) p
    mAttenuationConstant  <- (#peek aiLight, mAttenuationConstant) p
    mAttenuationLinear    <- (#peek aiLight, mAttenuationLinear) p
    mAttenuationQuadratic <- (#peek aiLight, mAttenuationQuadratic) p
    mColorDiffuse         <- (#peek aiLight, mColorDiffuse) p
    mColorSpecular        <- (#peek aiLight, mColorSpecular) p
    mColorAmbient         <- (#peek aiLight, mColorAmbient) p
    mAngleInnerCone       <- (#peek aiLight, mAngleInnerCone) p
    mAngleOuterCone       <- (#peek aiLight, mAngleOuterCone) p
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
  peek p = return $ Texel 0
  poke = undefined

instance Storable Camera where
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
    return $ Camera mName mPosition mUp mLookAt mHorizontalFOV mClipPlaneNear mClipPlaneFar mAspect
  poke p (Camera mName mPosition mUp mLookAt mHorizontalFOV mClipPlaneNear mClipPlaneFar mAspect) = do
    (#poke aiCamera, mName) p $ AiString mName
    (#poke aiCamera, mPosition) p mPosition
    (#poke aiCamera, mUp) p mUp
    (#poke aiCamera, mLookAt) p mLookAt
    (#poke aiCamera, mHorizontalFOV) p mHorizontalFOV
    (#poke aiCamera, mClipPlaneNear) p mClipPlaneNear
    (#poke aiCamera, mClipPlaneFar) p mClipPlaneFar
    (#poke aiCamera, mAspect) p mAspect

instance Storable Scene where
  sizeOf _ = #size aiScene
  alignment _ = #alignment aiScene
  peek p = do
    mFlags         <- liftM toEnumList $ ((#peek aiScene, mFlags) p :: IO CUInt)
    mRootNode      <- (#peek aiScene, mRootNode) p >>= peek
    mNumMeshes     <- (#peek aiScene, mNumMeshes) p :: IO CUInt
    mMeshes'       <- (#peek aiScene, mMeshes) p >>= peekArray (fromIntegral mNumMeshes)
    mMeshes        <- mapM peek mMeshes'
    mNumMaterials  <- (#peek aiScene, mNumMaterials) p :: IO CUInt
    mMaterials'    <- (#peek aiScene, mMaterials) p >>= peekArray (fromIntegral mNumMaterials)
    mMaterials     <- mapM peek mMaterials'
    mNumAnimations <- (#peek aiScene, mNumAnimations) p :: IO CUInt
    mAnimations    <- (#peek aiScene, mAnimations) p >>= peekArrayPtr (fromIntegral mNumAnimations)
    mNumTextures   <- (#peek aiScene, mNumTextures) p :: IO CUInt
    mTextures      <- (#peek aiScene, mTextures) p >>= peekArrayPtr (fromIntegral mNumTextures)
    mNumLights     <- (#peek aiScene, mNumLights) p :: IO CUInt
    mLights        <- (#peek aiScene, mLights) p >>= peekArrayPtr (fromIntegral mNumLights)
    mNumCameras    <- (#peek aiScene, mNumCameras) p :: IO CUInt
    mCameras       <- (#peek aiScene, mCameras) p >>= peekArrayPtr (fromIntegral mNumCameras)
    return $ Scene mFlags mRootNode mMeshes mMaterials mAnimations mTextures 
                   mLights mCameras
  poke p (Scene flags mRootNode meshes materials 
                animations textures lights cameras) = do
    -- (#poke aiScene, mFlags) p $ fromEnum flags 
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
