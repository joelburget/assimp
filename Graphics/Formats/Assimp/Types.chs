{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Formats.Assimp.Types (
    SceneFlags(..)
  , CompileFlags(..)
  , PostProcessSteps(..)
--   , Return(..)
--   , Origin(..)
--  , DefaultLogStream(..)
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
  , Plane3d(..)
  , Ray(..)
  , Vec2F(Vec2F)
  , Vec3F(Vec3F)
  , Vec4F(Vec4F)
  , Mat3F(Mat3F)
  , Mat4F(Mat4F)
  , Color3F(Color3F)
  , Color4F(Color4F)
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

import Data.Vect.Float

-- Remove the force32bit enums
#define SWIG

#include "assimp.h"        // Plain-C interface
#include "aiScene.h"       // Output data structure
#include "aiPostProcess.h" // Post processing flags
#include "aiVersion.h"     // Version information
#include "typedefs.h"

{#context lib="assimp"#}
{#context prefix="ai"#}

-- PostProcessSteps

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
instance Show CompileFlags where
  show Shared         = "Shared"
  show StlPort        = "StlPort"
  show Debug          = "Debug"
  show NoBoost        = "NoBoost"
  show SingleThreaded = "SingleThreaded"

{#enum aiPostProcessSteps as PostProcessSteps {} with prefix="aiProcess_" deriving (Show, Eq)#}

data Return = ReturnSuccess
            | ReturnFailure
            | ReturnOutofmemory
            deriving (Show,Eq)
instance Enum Return where
  fromEnum ReturnSuccess = 0
  fromEnum ReturnFailure = (-1)
  fromEnum ReturnOutofmemory = (-3)

  toEnum 0 = ReturnSuccess
  toEnum (-1) = ReturnFailure
  toEnum (-3) = ReturnOutofmemory
  toEnum unmatched = error ("Return.toEnum: Cannot match " ++ show unmatched)

--{#enum aiOrigin as Origin                     {} with prefix="aiOrigin_" deriving (Show, Eq)#}
--{#enum aiDefaultLogStream as DefaultLogStream {underscoreToCase} with prefix="aiDefaultLogStream" deriving (Show, Eq)#}

data PrimitiveType = PrimitiveTypePoint
                   | PrimitiveTypeLine
                   | PrimitiveTypeTriangle
                   | PrimitiveTypePolygon
                   deriving (Show,Eq)
instance Enum PrimitiveType where
  fromEnum PrimitiveTypePoint = 1
  fromEnum PrimitiveTypeLine = 2
  fromEnum PrimitiveTypeTriangle = 4
  fromEnum PrimitiveTypePolygon = 8

  toEnum 1 = PrimitiveTypePoint
  toEnum 2 = PrimitiveTypeLine
  toEnum 4 = PrimitiveTypeTriangle
  toEnum 8 = PrimitiveTypePolygon
  toEnum unmatched = error ("PrimitiveType.toEnum: Cannot match " ++ show unmatched)

data LightSourceType = LightSourceUndefined
                     | LightSourceDirectional
                     | LightSourcePoint
                     | LightSourceSpot
                     deriving (Show,Eq)
instance Enum LightSourceType where
  fromEnum LightSourceUndefined = 0
  fromEnum LightSourceDirectional = 1
  fromEnum LightSourcePoint = 2
  fromEnum LightSourceSpot = 3

  toEnum 0 = LightSourceUndefined
  toEnum 1 = LightSourceDirectional
  toEnum 2 = LightSourcePoint
  toEnum 3 = LightSourceSpot
  toEnum unmatched = error ("LightSourceType.toEnum: Cannot match " ++ show unmatched)

-- Texture enums
{#enum aiTextureOp as TextureOp               {} with prefix="aiTextureOp_" deriving (Show, Eq)#}
{#enum aiTextureMapMode as TextureMapMode     {} with prefix="aiTextureMapMode_" deriving (Show, Eq)#}
{#enum aiTextureMapping as TextureMapping     {underscoreToCase} with prefix="aiTextureMapping_" deriving (Show, Eq)#}
{#enum aiTextureType as TextureType           {underscoreToCase} with prefix="aiTextureType_" deriving (Show, Eq)#}
{#enum aiShadingMode as ShadingMode           {} with prefix="aiShadingMode_" deriving (Show, Eq)#}
{#enum aiTextureFlags as TextureFlags         {} with prefix="aiTextureFlags_" deriving (Show, Eq)#}
{#enum aiBlendMode as BlendMode               {} with prefix="aiBlendMode_" deriving (Show, Eq)#}
{#enum aiPropertyTypeInfo as PropertyTypeInfo {underscoreToCase} deriving (Show, Eq)#}

data Plane3d = Plane3d
  { planeA :: Float
  , planeB :: Float
  , planeC :: Float
  , planeD :: Float
  } deriving (Show)
{#pointer *aiPlane as PlanePtr -> Plane3d#}

data Ray = Ray 
  { rayPos :: Vec3
  , rayDir :: Vec3
  } deriving (Show)
{#pointer *aiRay as RayPtr -> Ray#}

newtype Vec2F = Vec2F Vec2
newtype Vec3F = Vec3F Vec3
newtype Vec4F = Vec4F Vec4
newtype Mat3F = Mat3F Mat3
newtype Mat4F = Mat4F Mat4
newtype Color3F = Color3F Vec3
newtype Color4F = Color4F Vec4

{#pointer *aiColor3D as Color3Ptr -> Color3F#}
{#pointer *aiColor4D as Color4Ptr -> Color4F#}
{#pointer *aiVector2D as Vec2Ptr -> Vec2#}
{#pointer *aiVector3D as Vec3Ptr -> Vec3#}
{#pointer *aiMatrix3x3 as Mat3Ptr -> Mat3#}
{#pointer *aiMatrix4x4 as Mat4Ptr -> Mat4#}

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
  , transformation :: Mat4
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
  , offpokeMatrix :: Mat4
  } deriving (Show)
{#pointer *aiBone as BonePtr -> Bone#}

data Mesh = Mesh
  { primitiveTypes  :: [PrimitiveType]
  , vertices        :: [Vec3]
  , normals         :: [Vec3]
  , tangents        :: [Vec3]
  , bitangents      :: [Vec3]
  , colors          :: [Vec4]
  , textureCoords   :: [Vec3]
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
  { translation :: Vec2
  , scaling     :: Vec2
  , rotation    :: Float
  } deriving (Show)

data Light = Light
  { lightName            :: String
  , mType                :: LightSourceType
  , lightPosition        :: Vec3
  , direction            :: Vec3
  , attenuationConstant  :: Float
  , attenuationLinear    :: Float
  , attenuationQuadratic :: Float
  , colorDiffuse         :: Vec3
  , colorSpecular        :: Vec3
  , colorAmbient         :: Vec3
  , angleInnerCone       :: Float
  , angleOuterCone       :: Float
  } deriving (Show)
{#pointer *aiLight as LightPtr -> Light#}

data Camera = Camera 
  { cameraName     :: String
  , cameraPosition :: Vec3
  , up             :: Vec3
  , lookAt         :: Vec3
  , horizontalFOV  :: Float
  , clipPlaneNear  :: Float
  , clipPlaneFar   :: Float
  , aspect         :: Float
  } deriving (Show)
{#pointer *aiCamera as CameraPtr -> Camera#}

class Position a where
  position :: a -> Vec3

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
