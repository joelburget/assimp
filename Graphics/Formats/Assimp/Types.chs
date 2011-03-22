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
    planeA :: Float
  , planeB :: Float
  , planeC :: Float
  , planeD :: Float
  } deriving (Show)
{#pointer *aiPlane as PlanePtr -> Plane#}

data Ray = Ray {
    rayPos :: Vector3D
  , rayDir :: Vector3D
  } deriving (Show)
{#pointer *aiRay as RayPtr -> Ray#}

data Color3D = Color3D {
    color3dR :: Float
  , color3dG :: Float
  , color3dB :: Float
  } deriving (Show)
{#pointer *aiColor3D as Color3DPtr -> Color3D#}

data Color4D = Color4D {
    color4dR :: Float
  , color4dG :: Float
  , color4dB :: Float
  , color4dA :: Float
  } deriving (Show)
{#pointer *aiColor4D as Color4DPtr -> Color4D#}

data MemoryInfo = MemoryInfo {
    textures   :: CUInt
  , materials  :: CUInt
  , meshes     :: CUInt
  , nodes      :: CUInt
  , animations :: CUInt
  , cameras    :: CUInt
  , lights     :: CUInt
  , total      :: CUInt
  } deriving (Show)
{#pointer *aiMemoryInfo as MemoryInfoPtr -> MemoryInfo#}

data LogStream
{#pointer *aiLogStream as LogStreamPtr -> LogStream#}

data Quaternion = Quaternion {
    quaternionW :: Float
  , quaternionX :: Float
  , quaternionY :: Float
  , quaternionZ :: Float
  } deriving (Show)
{#pointer *aiQuaternion as QuaternionPtr -> Quaternion#}

data Vector2D = Vector2D {
    vector2dX :: Float
  , vector2dY :: Float
  } deriving (Show)
{#pointer *aiVector2D as Vector2DPtr -> Vector2D#}

data Vector3D = Vector3D {
    vector3dX :: Float
  , vector3dY :: Float
  , vector3dZ :: Float
  } deriving (Show)
{#pointer *aiVector3D as Vector3DPtr -> Vector3D#}

newtype AiString = AiString String deriving (Show)
{#pointer *aiString as StringPtr -> AiString#}

data Matrix3x3 = Matrix3x3 {
  matrix3x3 :: Vector (Vector Float)
  } deriving (Show)
{#pointer *aiMatrix3x3 as Matrix3x3Ptr -> Matrix3x3#}

data Matrix4x4 = Matrix4x4 {
  matrix4x4 :: Vector (Vector Float)
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
  { nodeMName       :: String
  , mTransformation :: Matrix4x4
  , mParent         :: Maybe Node
  , mChildren       :: [Node]
  , nodeMMeshes     :: [CUInt] -- Holds indices defining the node
  } deriving (Show)
{#pointer *aiNode as NodePtr -> Node#}

data Face = Face
  {
    mIndices :: [CUInt] -- Holds indices defining the face
  } deriving (Show)
{#pointer *aiFace as FacePtr -> Face#}

data VertexWeight = VertexWeight
  { mVertexId :: CUInt
  , mWeight   :: CFloat
  } deriving (Show)
{#pointer *aiVertexWeight as VertexWeightPtr -> VertexWeight#}

data Bone = Bone
  { boneMName          :: String
  , mWeights       :: [VertexWeight]
  , mOffpokeMatrix :: Matrix4x4
  } deriving (Show)
{#pointer *aiBone as BonePtr -> Bone#}

data Mesh = Mesh
  { mPrimitiveTypes  :: [PrimitiveType]
  , mVertices        :: [Vector3D]
  , mNormals         :: [Vector3D]
  , mTangents        :: [Vector3D]
  , mBitangents      :: [Vector3D]
  , mColors          :: [Color4D]
  , mTextureCoords   :: [Vector3D]
  , mNumUVComponents :: CUInt
  , mFaces           :: [Face]
  , mBones           :: [Bone]
  , mMaterialIndex   :: CUInt
  , meshMName        :: String
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
    mProperties :: [MaterialProperty]
  } deriving (Show)
{#pointer *aiMaterial as MaterialPtr -> Material#}

data NodeAnim = NodeAnim {
    dummy'NodeAnim :: Int
  } deriving (Show)

data MeshAnim = MeshAnim {
    dummy'MeshAnim :: Int
  } deriving (Show)

data Animation = Animation {
    animationMName           :: String
  , mDuration       :: Double
  , mTicksPerSecond :: Double
  , mChannels       :: [NodeAnim]
  , mMeshChannels   :: [MeshAnim]
  } deriving (Show)
{#pointer *aiAnimation as AnimationPtr -> Animation#}

data Texel = Texel {
    dummy'Texel :: Int
  } deriving (Show)

data Texture = Texture {
    mWidth        :: CUInt
  , mHeight       :: CUInt
  , achFormatHint :: String
  , pcData        :: [Texel]
  } deriving (Show)
{#pointer *aiTexture as TexturePtr -> Texture#}

data UVTransform = UVTransform {
    mTranslation :: Vector2D
  , mScaling     :: Vector2D
  , mRotation    :: Float
  } deriving (Show)

data Light = Light {
    lightMName                 :: String
  , mType                 :: LightSourceType
  , lightMPosition             :: Vector3D
  , mDirection            :: Vector3D
  , mAttenuationConstant  :: Float
  , mAttenuationLinear    :: Float
  , mAttenuationQuadratic :: Float
  , mColorDiffuse         :: Color3D
  , mColorSpecular        :: Color3D
  , mColorAmbient         :: Color3D
  , mAngleInnerCone       :: Float
  , mAngleOuterCone       :: Float
  } deriving (Show)
{#pointer *aiLight as LightPtr -> Light#}

data Camera = Camera {
    cameraMName    :: String
  , mPosition      :: Vector3D
  , mUp            :: Vector3D
  , mLookAt        :: Vector3D
  , mHorizontalFOV :: Float
  , mClipPlaneNear :: Float
  , mClipPlaneFar  :: Float
  , mAspect        :: Float
  } deriving (Show)
{#pointer *aiCamera as CameraPtr -> Camera#}

data Scene = Scene
  { mFlags         :: [SceneFlags]
  , mRootNode      :: Node
  , mMeshes        :: [Mesh]
  , mMaterials     :: [Material]
  , mAnimations    :: [Animation]
  , mTextures      :: [Texture]
  , mLights        :: [Light]
  , mCameras       :: [Camera]
  } deriving (Show)
{#pointer *aiScene as ScenePtr -> Scene#}
