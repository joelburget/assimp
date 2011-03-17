{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Formats.Assimp.Types (
    SceneFlags(..)
  , AiPostProcessSteps(..)
  , AiReturn(..)
  , AiOrigin(..)
  , AiDefaultLogStream(..)
  , AiPrimitiveType(..)
  , AiLightSourceType(..)
  , AiTextureOp(..)
  , AiTextureMapMode(..)
  , AiTextureMapping(..)
  , AiTextureType(..)
  , AiShadingMode(..)
  , AiTextureFlags(..)
  , AiBlendMode(..)
  , AiPropertyTypeInfo(..)
  , AiPlane(..)
  , AiRay(..)
  , AiColor3D(..)
  , AiColor4D(..)
  , AiMemoryInfo(..)
  , AiQuaternion(..)
  , AiVector2D(..)
  , AiVector3D(..)
  , AiString(..)
  , AiMatrix3x3(..)
  , AiMatrix4x4(..)
  , AiNode(..)
  , AiFace(..)
  , AiVertexWeight(..)
  , AiBone(..)
  , AiMesh(..)
  , AiMaterialProperty(..)
  , AiMaterial(..)
  , AiNodeAnim(..)
  , AiMeshAnim(..)
  , AiAnimation(..)
  , AiLight(..)
  , AiCamera(..)
  , AiScene(..)
  , AiTexture(..)
  , (.|.)
  ) where

import C2HS
import Data.Vector.Storable hiding ((++))
import Data.Bits ((.|.))

#include "../../assimp/include/assimp.h"        // Plain-C interface
#include "../../assimp/include/aiScene.h"       // Output data structure
#include "../../assimp/include/aiPostProcess.h" // Post processing flags
#include "typedefs.h"

{#context lib="assimp"#}

{#enum define SceneFlags {AI_SCENE_FLAGS_INCOMPLETE         as FlagsIncomplete
                        , AI_SCENE_FLAGS_VALIDATED          as FlagsValidated
                        , AI_SCENE_FLAGS_VALIDATION_WARNING as FlagsValidationWarning
                        , AI_SCENE_FLAGS_NON_VERBOSE_FORMAT as FlagsNonVerboseFormat
                        , AI_SCENE_FLAGS_TERRAIN            as FlagsTerrain
                        }#}
instance Show SceneFlags where
  show _ = "Scene Flag"

{#enum aiPostProcessSteps as AiPostProcessSteps {underscoreToCase} deriving (Show, Eq)#}
{#enum aiReturn as AiReturn                     {underscoreToCase} deriving (Show, Eq)#}
{#enum aiOrigin as AiOrigin                     {underscoreToCase} deriving (Show, Eq)#}
{#enum aiDefaultLogStream as AiDefaultLogStream {underscoreToCase} deriving (Show, Eq)#}
{#enum aiPrimitiveType as AiPrimitiveType       {underscoreToCase} deriving (Show, Eq)#}
{#enum aiLightSourceType as AiLightSourceType   {underscoreToCase} deriving (Show, Eq)#}

-- Texture enums
{#enum aiTextureOp as AiTextureOp               {underscoreToCase} deriving (Show, Eq)#}
{#enum aiTextureMapMode as AiTextureMapMode     {underscoreToCase} deriving (Show, Eq)#}
{#enum aiTextureMapping as AiTextureMapping     {underscoreToCase} deriving (Show, Eq)#}
{#enum aiTextureType as AiTextureType           {underscoreToCase} deriving (Show, Eq)#}
{#enum aiShadingMode as AiShadingMode           {underscoreToCase} deriving (Show, Eq)#}
{#enum aiTextureFlags as AiTextureFlags         {underscoreToCase} deriving (Show, Eq)#}
{#enum aiBlendMode as AiBlendMode               {underscoreToCase} deriving (Show, Eq)#}
{#enum aiPropertyTypeInfo as AiPropertyTypeInfo {underscoreToCase} deriving (Show, Eq)#}

data AiPlane = AiPlane {
    a'AiPlane :: Float
  , b'AiPlane :: Float
  , c'AiPlane :: Float
  , d'AiPlane :: Float
  } deriving (Show)
{#pointer *aiPlane as AiPlanePtr -> AiPlane#}

data AiRay = AiRay {
    pos'AiRay :: AiVector3D
  , dir'AiRay :: AiVector3D
  } deriving (Show)
{#pointer *aiRay as AiRayPtr -> AiRay#}

data AiColor3D = AiColor3D {
    r'AiColor3D :: Float
  , g'AiColor3D :: Float
  , b'AiColor3D :: Float
  } deriving (Show)
{#pointer *aiColor3D as AiColor3DPtr -> AiColor3D#}

data AiColor4D = AiColor4D {
    r'AiColor4D :: Float
  , g'AiColor4D :: Float
  , b'AiColor4D :: Float
  , a'AiColor4D :: Float
  } deriving (Show)
{#pointer *aiColor4D as AiColor4DPtr -> AiColor4D#}

data AiMemoryInfo = AiMemoryInfo {
    textures'AiMemoryInfo   :: CUInt
  , materials'AiMemoryInfo  :: CUInt
  , meshes'AiMemoryInfo     :: CUInt
  , nodes'AiMemoryInfo      :: CUInt
  , animations'AiMemoryInfo :: CUInt
  , cameras'AiMemoryInfo    :: CUInt
  , lights'AiMemoryInfo     :: CUInt
  , total'AiMemoryInfo      :: CUInt
  } deriving (Show)
{#pointer *aiMemoryInfo as AiMemoryInfoPtr -> AiMemoryInfo#}

data AiLogStream
{#pointer *aiLogStream as AiLogStreamPtr -> AiLogStream#}

data AiQuaternion = AiQuaternion {
    w'AiQuaternion :: Float
  , x'AiQuaternion :: Float
  , y'AiQuaternion :: Float
  , z'AiQuaternion :: Float
  } deriving (Show)
{#pointer *aiQuaternion as AiQuaternionPtr -> AiQuaternion#}

data AiVector2D = AiVector2D {
    x'AiVector2D :: Float
  , y'AiVector2D :: Float
  } deriving (Show)
{#pointer *aiVector2D as AiVector2DPtr -> AiVector2D#}

data AiVector3D = AiVector3D {
    x'AiVector3D :: Float
  , y'AiVector3D :: Float
  , z'AiVector3D :: Float
  } deriving (Show)
{#pointer *aiVector3D as AiVector3DPtr -> AiVector3D#}

newtype AiString = AiString String deriving (Show)
{#pointer *aiString as AiStringPtr -> AiString#}

data AiMatrix3x3 = AiMatrix3x3 {
  matrix'AiMatrix3x3 :: Vector Float
  } deriving (Show)
{#pointer *aiMatrix3x3 as AiMatrix3x3Ptr -> AiMatrix3x3#}

data AiMatrix4x4 = AiMatrix4x4 {
  matrix'AiMatrix4x4 :: Vector Float
  } deriving (Show)
{#pointer *aiMatrix4x4 as AiMatrix4x4Ptr -> AiMatrix4x4#}

{- From the Assimp source:
 -
 - Nodes are little named entities in the scene that have a place and
 - orientation relative to their parents. Starting from the scene's root node
 - all nodes can have 0 to x child nodes, thus forming a hierarchy. They form
 - the base on which the scene is built on: a node can refer to 0..x meshes,
 - can be referred to by a bone of a mesh or can be animated by a key sequence
 - of an animation. DirectX calls them "frames", others call them "objects", we
 - call them aiNode.
 -
 - A node can potentially refer to single or multiple meshes. The meshes are
 - not stored inside the node, but instead in an array of aiMesh inside the
 - aiScene. A node only refers to them by their array index. This also means
 - that multiple nodes can refer to the same mesh, which provides a simple form
 - of instancing. A mesh referred to by this way lives in the node's local
 - coordinate system. If you want the mesh's orientation in global space, you'd
 - have to concatenate the transformations from the referring node and all of
 - its parents.
-}
data AiNode = AiNode
  { mName'AiNode           :: String
  , mTransformation'AiNode :: AiMatrix4x4
  , mParent'AiNode         :: Maybe AiNode
  , mChildren'AiNode       :: [AiNode]
  , mMeshes'AiNode         :: [CUInt] -- Holds indices defining the node
  } deriving (Show)
{#pointer *aiNode as AiNodePtr -> AiNode#}

data AiFace = AiFace
  {
    mIndices'AiFace    :: [CUInt] -- Holds indices defining the face
  } deriving (Show)
{#pointer *aiFace as AiFacePtr -> AiFace#}

data AiVertexWeight = AiVertexWeight
  { mVertexId'AiVertexWeight :: CUInt
  , mWeight'AiVertexWeight   :: CFloat
  } deriving (Show)
{#pointer *aiVertexWeight as AiVertexWeightPtr -> AiVertexWeight#}

data AiBone = AiBone
  { mName'AiBone          :: String
  , mWeights'AiBone       :: [AiVertexWeight]
  , mOffpokeMatrix'AiBone :: AiMatrix4x4
  } deriving (Show)
{#pointer *aiBone as AiBonePtr -> AiBone#}

data AiMesh = AiMesh
  { mPrimitiveTypes'AiMesh  :: [AiPrimitiveType]
  , mVertices'AiMesh        :: [AiVector3D]
  , mNormals'AiMesh         :: [AiVector3D]
  , mTangents'AiMesh        :: [AiVector3D]
  , mBitangents'AiMesh      :: [AiVector3D]
  , mColors'AiMesh          :: [AiColor4D]
  , mTextureCoords'AiMesh   :: [AiVector3D]
  , mNumUVComponents'AiMesh :: CUInt
  , mFaces'AiMesh           :: [AiFace]
  , mBones'AiMesh           :: [AiBone]
  , mMaterialIndex'AiMesh   :: CUInt
  , mName'AiMesh            :: String
  } deriving (Show)
{#pointer *aiMesh as AiMeshPtr -> AiMesh#}

data AiMaterialProperty = AiMaterialProperty {
    mKey      :: String
  , mSemantic :: AiTextureType
  , mIndex    :: CUInt
  , mData     :: String
  } deriving (Show)
{#pointer *aiMaterialProperty as AiMaterialPropertyPtr -> AiMaterialProperty#}

data AiMaterial = AiMaterial {
    mProperties'AiMaterial :: [AiMaterialProperty]
  } deriving (Show)
{#pointer *aiMaterial as AiMaterialPtr -> AiMaterial#}

data AiNodeAnim = AiNodeAnim {
    dummy'AiNodeAnim :: Int
  } deriving (Show)

data AiMeshAnim = AiMeshAnim {
    dummy'AiMeshAnim :: Int
  } deriving (Show)

data AiAnimation = AiAnimation {
    mName'AiAnimation           :: String
  , mDuration'AiAnimation       :: Double
  , mTicksPerSecond'AiAnimation :: Double
  , mChannels'AiAnimation       :: [AiNodeAnim]
  , mMeshChannels'AiAnimation   :: [AiMeshAnim]
  } deriving (Show)
{#pointer *aiAnimation as AiAnimationPtr -> AiAnimation#}

data AiTexel = AiTexel {
    dummy'AiTexel :: Int
  } deriving (Show)

data AiTexture = AiTexture {
    mWidth'AiTexture :: CUInt
  , mHeight'AiTexture :: CUInt
  , achFormatHint'AiTexture :: String
  , pcData'AiTexture :: [AiTexel]
  } deriving (Show)
{#pointer *aiTexture as AiTexturePtr -> AiTexture#}

data AiUVTransform = AiUVTransform {
    mTranslation'AiUVTransform :: AiVector2D
  , mScaling'AiUVTransform     :: AiVector2D
  , mRotation'AiUVTransform    :: Float
  } deriving (Show)

data AiLight = AiLight {
    mName'AiLight                 :: String
  , mType'AiLight                 :: AiLightSourceType
  , mPosition'AiLight             :: AiVector3D
  , mDirection'AiLight            :: AiVector3D
  , mAttenuationConstant'AiLight  :: Float
  , mAttenuationLinear'AiLight    :: Float
  , mAttenuationQuadratic'AiLight :: Float
  , mColorDiffuse'AiLight         :: AiColor3D
  , mColorSpecular'AiLight        :: AiColor3D
  , mColorAmbient'AiLight         :: AiColor3D
  , mAngleInnerCone'AiLight       :: Float
  , mAngleOuterCone'AiLight       :: Float
  } deriving (Show)
{#pointer *aiLight as AiLightPtr -> AiLight#}

data AiCamera = AiCamera {
    mName'AiCamera          :: String
  , mPosition'AiCamera      :: AiVector3D
  , mUp'AiCamera            :: AiVector3D
  , mLookAt'AiCamera        :: AiVector3D
  , mHorizontalFOV'AiCamera :: Float
  , mClipPlaneNear'AiCamera :: Float
  , mClipPlaneFar'AiCamera  :: Float
  , mAspect'AiCamera        :: Float
  } deriving (Show)
{#pointer *aiCamera as AiCameraPtr -> AiCamera#}

data AiScene = AiScene
  { mFlags'AiScene         :: SceneFlags
  , mRootNode'AiScene      :: AiNode
  , mMeshes'AiScene        :: [AiMesh]
  , mMaterials'AiScene     :: [AiMaterial]
  , mAnimations'AiScene    :: [AiAnimation]
  , mTextures'AiScene      :: [AiTexture]
  , mLights'AiScene        :: [AiLight]
  , mCameras'AiScene       :: [AiCamera]
  } deriving (Show)
{#pointer *aiScene as AiScenePtr -> AiScene#}
