{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Formats.Assimp.Types (
    SceneFlags(..)
  , PostProcessSteps(..)
  , Return(..)
  , Origin(..)
  , DefaultLogStream(..)
  , PrimitiveType(..)
  , LightSourceType(..)
  , TextureOp(..)
  , TextureMapMode(..)
  , TextureMapping(..)
  , TextureType(..)
  , ShadingMode(..)
  , TextureFlags(..)
  , BlendMode(..)
  , PropertyTypeInfo(..)
  , Plane(..)
  , Ray(..)
  , Color3D(..)
  , Color4D(..)
  , MemoryInfo(..)
  , Quaternion(..)
  , Vector2D(..)
  , Vector3D(..)
  , AiString(..)
  , Matrix3x3(..)
  , Matrix4x4(..)
  , Node(..)
  , Face(..)
  , VertexWeight(..)
  , Bone(..)
  , Mesh(..)
  , MaterialProperty(..)
  , Material(..)
  , NodeAnim(..)
  , MeshAnim(..)
  , Animation(..)
  , Light(..)
  , Camera(..)
  , Scene(..)
  , Texture(..)
  , Texel(..)
  , (.|.)
  ) where

import C2HS
import Data.Vector hiding ((++))
import Data.Bits ((.|.))

#include "../../assimp/include/assimp.h"        // Plain-C interface
#include "../../assimp/include/aiScene.h"       // Output data structure
#include "../../assimp/include/aiPostProcess.h" // Post processing flags
#include "typedefs.h"

{#context lib="assimp"#}
{#context prefix="ai"#}

{#enum define SceneFlags {AI_SCENE_FLAGS_INCOMPLETE         as FlagsIncomplete
                        , AI_SCENE_FLAGS_VALIDATED          as FlagsValidated
                        , AI_SCENE_FLAGS_VALIDATION_WARNING as FlagsValidationWarning
                        , AI_SCENE_FLAGS_NON_VERBOSE_FORMAT as FlagsNonVerboseFormat
                        , AI_SCENE_FLAGS_TERRAIN            as FlagsTerrain
                        }#}
instance Show SceneFlags where
  show FlagsIncomplete        = "FlagsIncomplete"
  show FlagsValidated         = "FlagsValidated"
  show FlagsValidationWarning = "FlagsValidationWarning"
  show FlagsNonVerboseFormat  = "FlagsNonVerboseFormat"
  show FlagsTerrain           = "FlagsTerrain"

{#enum aiPostProcessSteps as PostProcessSteps {underscoreToCase} deriving (Show, Eq)#}
{#enum aiReturn as Return                     {underscoreToCase} deriving (Show, Eq)#}
{#enum aiOrigin as Origin                     {underscoreToCase} deriving (Show, Eq)#}
{#enum aiDefaultLogStream as DefaultLogStream {underscoreToCase} deriving (Show, Eq)#}
{#enum aiPrimitiveType as PrimitiveType       {underscoreToCase} deriving (Show, Eq)#}
{#enum aiLightSourceType as LightSourceType   {underscoreToCase} deriving (Show, Eq)#}

-- Texture enums
{#enum aiTextureOp as TextureOp               {underscoreToCase} deriving (Show, Eq)#}
{#enum aiTextureMapMode as TextureMapMode     {underscoreToCase} deriving (Show, Eq)#}
{#enum aiTextureMapping as TextureMapping     {underscoreToCase} deriving (Show, Eq)#}
{#enum aiTextureType as TextureType           {underscoreToCase} deriving (Show, Eq)#}
{#enum aiShadingMode as ShadingMode           {underscoreToCase} deriving (Show, Eq)#}
{#enum aiTextureFlags as TextureFlags         {underscoreToCase} deriving (Show, Eq)#}
{#enum aiBlendMode as BlendMode               {underscoreToCase} deriving (Show, Eq)#}
{#enum aiPropertyTypeInfo as PropertyTypeInfo {underscoreToCase} deriving (Show, Eq)#}

data Plane = Plane {
    a'Plane :: Float
  , b'Plane :: Float
  , c'Plane :: Float
  , d'Plane :: Float
  } deriving (Show)
{#pointer *aiPlane as PlanePtr -> Plane#}

data Ray = Ray {
    pos'Ray :: Vector3D
  , dir'Ray :: Vector3D
  } deriving (Show)
{#pointer *aiRay as RayPtr -> Ray#}

data Color3D = Color3D {
    r'Color3D :: Float
  , g'Color3D :: Float
  , b'Color3D :: Float
  } deriving (Show)
{#pointer *aiColor3D as Color3DPtr -> Color3D#}

data Color4D = Color4D {
    r'Color4D :: Float
  , g'Color4D :: Float
  , b'Color4D :: Float
  , a'Color4D :: Float
  } deriving (Show)
{#pointer *aiColor4D as Color4DPtr -> Color4D#}

data MemoryInfo = MemoryInfo {
    textures'MemoryInfo   :: CUInt
  , materials'MemoryInfo  :: CUInt
  , meshes'MemoryInfo     :: CUInt
  , nodes'MemoryInfo      :: CUInt
  , animations'MemoryInfo :: CUInt
  , cameras'MemoryInfo    :: CUInt
  , lights'MemoryInfo     :: CUInt
  , total'MemoryInfo      :: CUInt
  } deriving (Show)
{#pointer *aiMemoryInfo as MemoryInfoPtr -> MemoryInfo#}

data LogStream
{#pointer *aiLogStream as LogStreamPtr -> LogStream#}

data Quaternion = Quaternion {
    w'Quaternion :: Float
  , x'Quaternion :: Float
  , y'Quaternion :: Float
  , z'Quaternion :: Float
  } deriving (Show)
{#pointer *aiQuaternion as QuaternionPtr -> Quaternion#}

data Vector2D = Vector2D {
    x'Vector2D :: Float
  , y'Vector2D :: Float
  } deriving (Show)
{#pointer *aiVector2D as Vector2DPtr -> Vector2D#}

data Vector3D = Vector3D {
    x'Vector3D :: Float
  , y'Vector3D :: Float
  , z'Vector3D :: Float
  } deriving (Show)
{#pointer *aiVector3D as Vector3DPtr -> Vector3D#}

newtype AiString = AiString String deriving (Show)
{#pointer *aiString as StringPtr -> AiString#}

data Matrix3x3 = Matrix3x3 {
  matrix'Matrix3x3 :: Vector (Vector Float)
  } deriving (Show)
{#pointer *aiMatrix3x3 as Matrix3x3Ptr -> Matrix3x3#}

data Matrix4x4 = Matrix4x4 {
  matrix'Matrix4x4 :: Vector (Vector Float)
  } deriving (Show)
{#pointer *aiMatrix4x4 as Matrix4x4Ptr -> Matrix4x4#}

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
data Node = Node
  { mName'Node           :: String
  , mTransformation'Node :: Matrix4x4
  , mParent'Node         :: Maybe Node
  , mChildren'Node       :: [Node]
  , mMeshes'Node         :: [CUInt] -- Holds indices defining the node
  } deriving (Show)
{#pointer *aiNode as NodePtr -> Node#}

data Face = Face
  {
    mIndices'Face    :: [CUInt] -- Holds indices defining the face
  } deriving (Show)
{#pointer *aiFace as FacePtr -> Face#}

data VertexWeight = VertexWeight
  { mVertexId'VertexWeight :: CUInt
  , mWeight'VertexWeight   :: CFloat
  } deriving (Show)
{#pointer *aiVertexWeight as VertexWeightPtr -> VertexWeight#}

data Bone = Bone
  { mName'Bone          :: String
  , mWeights'Bone       :: [VertexWeight]
  , mOffpokeMatrix'Bone :: Matrix4x4
  } deriving (Show)
{#pointer *aiBone as BonePtr -> Bone#}

data Mesh = Mesh
  { mPrimitiveTypes'Mesh  :: [PrimitiveType]
  , mVertices'Mesh        :: [Vector3D]
  , mNormals'Mesh         :: [Vector3D]
  , mTangents'Mesh        :: [Vector3D]
  , mBitangents'Mesh      :: [Vector3D]
  , mColors'Mesh          :: [Color4D]
  , mTextureCoords'Mesh   :: [Vector3D]
  , mNumUVComponents'Mesh :: CUInt
  , mFaces'Mesh           :: [Face]
  , mBones'Mesh           :: [Bone]
  , mMaterialIndex'Mesh   :: CUInt
  , mName'Mesh            :: String
  } deriving (Show)
{#pointer *aiMesh as MeshPtr -> Mesh#}

data MaterialProperty = MaterialProperty {
    mKey      :: String
  , mSemantic :: TextureType
  , mIndex    :: CUInt
  , mData     :: String
  } deriving (Show)
{#pointer *aiMaterialProperty as MaterialPropertyPtr -> MaterialProperty#}

data Material = Material {
    mProperties'Material :: [MaterialProperty]
  } deriving (Show)
{#pointer *aiMaterial as MaterialPtr -> Material#}

data NodeAnim = NodeAnim {
    dummy'NodeAnim :: Int
  } deriving (Show)

data MeshAnim = MeshAnim {
    dummy'MeshAnim :: Int
  } deriving (Show)

data Animation = Animation {
    mName'Animation           :: String
  , mDuration'Animation       :: Double
  , mTicksPerSecond'Animation :: Double
  , mChannels'Animation       :: [NodeAnim]
  , mMeshChannels'Animation   :: [MeshAnim]
  } deriving (Show)
{#pointer *aiAnimation as AnimationPtr -> Animation#}

data Texel = Texel {
    dummy'Texel :: Int
  } deriving (Show)

data Texture = Texture {
    mWidth'Texture :: CUInt
  , mHeight'Texture :: CUInt
  , achFormatHint'Texture :: String
  , pcData'Texture :: [Texel]
  } deriving (Show)
{#pointer *aiTexture as TexturePtr -> Texture#}

data UVTransform = UVTransform {
    mTranslation'UVTransform :: Vector2D
  , mScaling'UVTransform     :: Vector2D
  , mRotation'UVTransform    :: Float
  } deriving (Show)

data Light = Light {
    mName'Light                 :: String
  , mType'Light                 :: LightSourceType
  , mPosition'Light             :: Vector3D
  , mDirection'Light            :: Vector3D
  , mAttenuationConstant'Light  :: Float
  , mAttenuationLinear'Light    :: Float
  , mAttenuationQuadratic'Light :: Float
  , mColorDiffuse'Light         :: Color3D
  , mColorSpecular'Light        :: Color3D
  , mColorAmbient'Light         :: Color3D
  , mAngleInnerCone'Light       :: Float
  , mAngleOuterCone'Light       :: Float
  } deriving (Show)
{#pointer *aiLight as LightPtr -> Light#}

data Camera = Camera {
    mName'Camera          :: String
  , mPosition'Camera      :: Vector3D
  , mUp'Camera            :: Vector3D
  , mLookAt'Camera        :: Vector3D
  , mHorizontalFOV'Camera :: Float
  , mClipPlaneNear'Camera :: Float
  , mClipPlaneFar'Camera  :: Float
  , mAspect'Camera        :: Float
  } deriving (Show)
{#pointer *aiCamera as CameraPtr -> Camera#}

data Scene = Scene
  { mFlags'Scene         :: [SceneFlags]
  , mRootNode'Scene      :: Node
  , mMeshes'Scene        :: [Mesh]
  , mMaterials'Scene     :: [Material]
  , mAnimations'Scene    :: [Animation]
  , mTextures'Scene      :: [Texture]
  , mLights'Scene        :: [Light]
  , mCameras'Scene       :: [Camera]
  } deriving (Show)
{#pointer *aiScene as ScenePtr -> Scene#}
