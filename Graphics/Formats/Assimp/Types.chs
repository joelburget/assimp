{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Formats.Assimp.Types (
    SceneFlags(..)
  , CompileFlags(..)
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
  , position
  ) where

import C2HS
import Data.Bits ((.|.))

import Graphics.Formats.Assimp.Vec

#include "assimp.h"        // Plain-C interface
#include "aiScene.h"       // Output data structure
#include "aiPostProcess.h" // Post processing flags
#include "aiVersion.h"     // Version information
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

{#enum define CompileFlags {ASSIMP_CFLAGS_SHARED         as Shared
                          , ASSIMP_CFLAGS_STLPORT        as StlPort
                          , ASSIMP_CFLAGS_DEBUG          as Debug
                          , ASSIMP_CFLAGS_NOBOOST        as NoBoost
                          , ASSIMP_CFLAGS_SINGLETHREADED as SingleThreaded
                          }#}

{#enum aiPostProcessSteps as PostProcessSteps {} with prefix="aiProcess_" deriving (Show, Eq)#}
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

data Plane = Plane 
  { planeA :: Float
  , planeB :: Float
  , planeC :: Float
  , planeD :: Float
  } deriving (Show)
{#pointer *aiPlane as PlanePtr -> Plane#}

data Ray = Ray 
  { rayPos :: Vec3F
  , rayDir :: Vec3F
  } deriving (Show)
{#pointer *aiRay as RayPtr -> Ray#}

{#pointer *aiColor3D as Color3FPtr -> Color3F#}
{#pointer *aiColor4D as Color4FPtr -> Color4F#}
{#pointer *aiVector2D as Vec2FPtr -> Vec2F#}
{#pointer *aiVector3D as Vec3FPtr -> Vec3F#}
{#pointer *aiMatrix3x3 as Matrix3FPtr -> Matrix3F#}
{#pointer *aiMatrix4x4 as Matrix4FPtr -> Matrix4F#}

data MemoryInfo = MemoryInfo 
  { memoryInfoTextures   :: CUInt
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

data Quaternion = Quaternion 
  { quaternionW :: Float
  , quaternionX :: Float
  , quaternionY :: Float
  , quaternionZ :: Float
  } deriving (Show)
{#pointer *aiQuaternion as QuaternionPtr -> Quaternion#}

newtype AiString = AiString String deriving (Show)
{#pointer *aiString as StringPtr -> AiString#}

data Node = Node
  { nodeName       :: String
  , transformation :: Matrix4F
  , parent         :: Maybe Node
  , children       :: [Node]
  , nodeMeshes     :: [CUInt] -- Holds indices defining the node
  } deriving (Show)
{#pointer *aiNode as NodePtr -> Node#}

data Face = Face
  { indices :: [CUInt] -- Holds indices defining the face
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
  , offpokeMatrix :: Matrix4F
  } deriving (Show)
{#pointer *aiBone as BonePtr -> Bone#}

data Mesh = Mesh
  { primitiveTypes  :: [PrimitiveType]
  , vertices        :: [Vec3F]
  , normals         :: [Vec3F]
  , tangents        :: [Vec3F]
  , bitangents      :: [Vec3F]
  , colors          :: [Color4F]
  , textureCoords   :: [Vec3F]
  , numUVComponents :: CUInt
  , faces           :: [Face]
  , bones           :: [Bone]
  , materialIndex   :: CUInt
  , meshName        :: String
  } deriving (Show)
{#pointer *aiMesh as MeshPtr -> Mesh#}

data MaterialProperty = MaterialProperty 
  { key      :: String
  , semantic :: TextureType
  , index    :: CUInt
  , mData    :: String
  } deriving (Show)
{#pointer *aiMaterialProperty as MaterialPropertyPtr -> MaterialProperty#}

data Material = Material 
  { properties :: [MaterialProperty]
  } deriving (Show)
{#pointer *aiMaterial as MaterialPtr -> Material#}

data NodeAnim = NodeAnim 
  { dummy'NodeAnim :: Int
  } deriving (Show)

data MeshAnim = MeshAnim 
  { dummy'MeshAnim :: Int
  } deriving (Show)

data Animation = Animation 
  { animationName  :: String
  , duration       :: Double
  , ticksPerSecond :: Double
  , channels       :: [NodeAnim]
  , meshChannels   :: [MeshAnim]
  } deriving (Show)
{#pointer *aiAnimation as AnimationPtr -> Animation#}

data Texel = Texel 
  { dummy'Texel :: Int
  } deriving (Show)

data Texture = Texture 
  { width         :: CUInt
  , height        :: CUInt
  , achFormatHint :: String
  , pcData        :: [Texel]
  } deriving (Show)
{#pointer *aiTexture as TexturePtr -> Texture#}

data UVTransform = UVTransform 
  { translation :: Vec2F
  , scaling     :: Vec2F
  , rotation    :: Float
  } deriving (Show)

data Light = Light 
  { lightName            :: String
  , mType                :: LightSourceType
  , lightPosition        :: Vec3F
  , direction            :: Vec3F
  , attenuationConstant  :: Float
  , attenuationLinear    :: Float
  , attenuationQuadratic :: Float
  , colorDiffuse         :: Color3F
  , colorSpecular        :: Color3F
  , colorAmbient         :: Color3F
  , angleInnerCone       :: Float
  , angleOuterCone       :: Float
  } deriving (Show)
{#pointer *aiLight as LightPtr -> Light#}

data Camera = Camera 
  { cameraName     :: String
  , cameraPosition :: Vec3F
  , up             :: Vec3F
  , lookAt         :: Vec3F
  , horizontalFOV  :: Float
  , clipPlaneNear  :: Float
  , clipPlaneFar   :: Float
  , aspect         :: Float
  } deriving (Show)
{#pointer *aiCamera as CameraPtr -> Camera#}

class Position a where
  position :: a -> Vec3F

instance Position Camera where
  position = cameraPosition

instance Position Light where
  position = lightPosition

class Name a where
  name :: a -> String

instance Name Node where
  name = nodeName

instance Name Bone where
  name = boneName

instance Name Mesh where
  name = meshName

instance Name Animation where
  name = animationName

instance Name Light where
  name = lightName

instance Name Camera where
  name = cameraName

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
