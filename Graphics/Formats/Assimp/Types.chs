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
  , MemoryInfo(..)
  , Quaternion(..)
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

import Graphics.Formats.Assimp.Vec

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
    rayPos :: Vec3D
  , rayDir :: Vec3D
  } deriving (Show)
{#pointer *aiRay as RayPtr -> Ray#}

{#pointer *aiColor3D as Color3DPtr -> Color3D#}
{#pointer *aiColor4D as Color4DPtr -> Color4D#}
{#pointer *aiVector2D as Vec2DPtr -> Vec2D#}
{#pointer *aiVector3D as Vec3DPtr -> Vec3D#}

data MemoryInfo = MemoryInfo {
    memoryInfoTextures   :: CUInt
  , memoryInfoMaterials  :: CUInt
  , memoryInfoMeshes     :: CUInt
  , memoryInfoNodes      :: CUInt
  , memoryInfoAnimations :: CUInt
  , memoryInfoCameras    :: CUInt
  , memoryInfoLights     :: CUInt
  , memoryInfoTotal      :: CUInt
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
  { nodeName       :: String
  , transformation :: Matrix4x4
  , parent         :: Maybe Node
  , children       :: [Node]
  , nodeMeshes     :: [CUInt] -- Holds indices defining the node
  } deriving (Show)
{#pointer *aiNode as NodePtr -> Node#}

data Face = Face
  {
    indices :: [CUInt] -- Holds indices defining the face
  } deriving (Show)
{#pointer *aiFace as FacePtr -> Face#}

data VertexWeight = VertexWeight
  { vertexId :: CUInt
  , weight   :: CFloat
  } deriving (Show)
{#pointer *aiVertexWeight as VertexWeightPtr -> VertexWeight#}

data Bone = Bone
  { boneName      :: String
  , weights       :: [VertexWeight]
  , offpokeMatrix :: Matrix4x4
  } deriving (Show)
{#pointer *aiBone as BonePtr -> Bone#}

data Mesh = Mesh
  { primitiveTypes  :: [PrimitiveType]
  , vertices        :: [Vec3D]
  , normals         :: [Vec3D]
  , tangents        :: [Vec3D]
  , bitangents      :: [Vec3D]
  , colors          :: [Color4D]
  , textureCoords   :: [Vec3D]
  , numUVComponents :: CUInt
  , faces           :: [Face]
  , bones           :: [Bone]
  , materialIndex   :: CUInt
  , meshName        :: String
  } deriving (Show)
{#pointer *aiMesh as MeshPtr -> Mesh#}

data MaterialProperty = MaterialProperty {
    key      :: String
  , semantic :: TextureType
  , index    :: CUInt
  , mData    :: String
  } deriving (Show)
{#pointer *aiMaterialProperty as MaterialPropertyPtr -> MaterialProperty#}

data Material = Material {
    properties :: [MaterialProperty]
  } deriving (Show)
{#pointer *aiMaterial as MaterialPtr -> Material#}

data NodeAnim = NodeAnim {
    dummy'NodeAnim :: Int
  } deriving (Show)

data MeshAnim = MeshAnim {
    dummy'MeshAnim :: Int
  } deriving (Show)

data Animation = Animation {
    animationName  :: String
  , duration       :: Double
  , ticksPerSecond :: Double
  , channels       :: [NodeAnim]
  , meshChannels   :: [MeshAnim]
  } deriving (Show)
{#pointer *aiAnimation as AnimationPtr -> Animation#}

data Texel = Texel {
    dummy'Texel :: Int
  } deriving (Show)

data Texture = Texture {
    width        :: CUInt
  , height       :: CUInt
  , achFormatHint :: String
  , pcData        :: [Texel]
  } deriving (Show)
{#pointer *aiTexture as TexturePtr -> Texture#}

data UVTransform = UVTransform {
    translation :: Vec2D
  , scaling     :: Vec2D
  , rotation    :: Float
  } deriving (Show)

data Light = Light {
    lightName            :: String
  , mType                :: LightSourceType
  , lightPosition        :: Vec3D
  , direction            :: Vec3D
  , attenuationConstant  :: Float
  , attenuationLinear    :: Float
  , attenuationQuadratic :: Float
  , colorDiffuse         :: Color3D
  , colorSpecular        :: Color3D
  , colorAmbient         :: Color3D
  , angleInnerCone       :: Float
  , angleOuterCone       :: Float
  } deriving (Show)
{#pointer *aiLight as LightPtr -> Light#}

data Camera = Camera {
    cameraName     :: String
  , cameraPosition :: Vec3D
  , up             :: Vec3D
  , lookAt         :: Vec3D
  , horizontalFOV  :: Float
  , clipPlaneNear  :: Float
  , clipPlaneFar   :: Float
  , aspect         :: Float
  } deriving (Show)
{#pointer *aiCamera as CameraPtr -> Camera#}

data Scene = Scene
  { flags      :: [SceneFlags]
  , rootNode   :: Node
  , meshes     :: [Mesh]
  , materials  :: [Material]
  , animations :: [Animation]
  , textures   :: [Texture]
  , lights     :: [Light]
  , cameras    :: [Camera]
  } deriving (Show)
{#pointer *aiScene as ScenePtr -> Scene#}
