{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Formats.Assimp where

import C2HS
--import Foreign.Ptr
--import System.IO.Unsafe
import Control.Monad
import Control.Applicative ((<$>), (<*>))

#include "../../assimp/include/assimp.h"        // Plain-C interface
#include "../../assimp/include/aiScene.h"       // Output data structure
#include "../../assimp/include/aiPostProcess.h" // Post processing flags

{#context lib="assimp"#}

{#enum define SceneFlags {AI_SCENE_FLAGS_INCOMPLETE         as FlagsIncomplete
                        , AI_SCENE_FLAGS_VALIDATED          as FlagsValidated
                        , AI_SCENE_FLAGS_VALIDATION_WARNING as FlagsValidationWarning
                        , AI_SCENE_FLAGS_NON_VERBOSE_FORMAT as FlagsNonVerboseFormat
                        , AI_SCENE_FLAGS_TERRAIN            as FlagsTerrain
                        }#}

{#enum aiPostProcessSteps as AiPostProcessSteps {underscoreToCase} deriving (Show, Eq)#}
{#enum aiReturn as AiReturn                     {underscoreToCase} deriving (Show, Eq)#}
{#enum aiOrigin as AiOrigin                     {underscoreToCase} deriving (Show, Eq)#}
{#enum aiDefaultLogStream as AiDefaultLogStream {underscoreToCase} deriving (Show, Eq)#}
{#enum aiPrimitiveType as AiPrimitiveType       {underscoreToCase} deriving (Show, Eq)#}

-- This seems to be necessary for #sizeof to work
#c
typedef struct aiScene aiScene;
typedef struct aiString aiString;
typedef struct aiNode aiNode;
typedef struct aiFace aiFace;
typedef struct aiVertexWeight aiVertexWeight;
typedef struct aiBone aiBone;
typedef struct aiMesh aiMesh;
typedef struct aiMatrix4x4 aiMatrix4x4;
#endc

--{#pointer *aiNode         as AiNode         #}
--{#pointer *aiScene        as AiScene        #}
{#pointer *aiPlane        as AiPlane        #}
{#pointer *aiRay          as AiRay          #}
{#pointer *aiColor3D      as AiColor3D      #}
{#pointer *aiColor4D      as AiColor4D      #}
--{#pointer *aiString       as AiString       #}
{#pointer *aiMemoryInfo   as AiMemoryInfo   #}
{#pointer *aiLogStream    as AiLogStream    #}
{#pointer *aiQuaternion   as AiQuaternion   #}
{#pointer *aiMatrix3x3    as AiMatrix3x3    #}
{#pointer *aiMatrix4x4    as AiMatrix4x4    #}
{#pointer *aiVector3D     as AiVector3D     #}
--{#pointer *aiFace         as AiFace         #}
--{#pointer *aiVertexWeight as AiVertexWeight #}
--{#pointer *aiBone         as AiBone         #}
--{#pointer *aiMesh         as AiMesh         #}

data AiString = AiString
  { length'AiString :: Int
  , data'AiString   :: String -- Maximum length MAXLEN
  }
aiStringToString = data'AiString
stringToAiString x = AiString (length x) x
instance Storable AiString where
  sizeOf _ = {#sizeof aiString#}
  alignment _ = 4 -- http://web.archiveorange.com/archive/v/V6skIGuT4JclKSXDM7Xj
  peek p = liftM2 AiString
    (liftM cIntConv ({#get aiString.length#} p))
    ({#get aiString.data#} p >>= peekCString)
  poke p x = do
    {#set aiString.length#} p (cIntConv $ length'AiString x)
    str <- newCString $ data'AiString x
    {#set aiString.data#} p str

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
  , mParent'AiNode         :: AiNode
  --, mNumChildren'AiNode    :: CUInt
  , mChildren'AiNode       :: [AiNode]
  --, mNumMeshes'AiNode      :: CUInt
  , mMeshes                :: [CUInt]
  }
instance Storable AiNode where
  sizeOf _ = {#sizeof aiNode#}
  alignment _ = 4
  peek = undefined
  poke = undefined
  --peek p = do
  --  mName <- liftM aiStringToString $ {#get aiNode.mName#} p
  --  mTransformation <- {#get aiNode.mTransformation#} p
  --  mParent <- {#get aiNode.mParent#} p
  --  mNumChildren <- {#get aiNode.mNumChildren#} p
  --  mChildren <- peekArray mNumChildren ({#get aiNode.mChildren#} p) -- TODO
  --  mMeshes <- peekArray ({#get aiNode.mNumMeshes#} p) ({#get aiNode.mMeshes#})
  --  return $ AiNode mName mTransformation mParent mChildren mMeshes
  --poke p (AiNode name trans par chil mes) = do
  --  {#set aiNode.mName#} $ stringToAiString $ mName'AiNode p
  --  {#set aiNode.mTransformation#} $ mTransformation'AiNode p
  --  {#set aiNode.mParent#} $ mParent'AiNode p 
  --  {#set aiNode.mChildren#} $ mChildren'AiNode p
  --  {#set aiNode.mMeshes#} $ mMeshes'AiNode p

data AiFace = AiFace
  { --mNumIndices :: CUInt
    mIndices'AiFace    :: [CUInt]
  }
instance Storable AiFace where
  sizeOf _ = {#sizeof aiFace#}
  alignment _ = 4
  peek p = do
    mNumIndices <- {#get aiFace.mNumIndices#}
    mIndices <- {#get aiFace.mIndices#}
    lst <- peekArray mNumIndices mIndices
    return $ AiFace lst
  poke p (AiFace mIndices) = do
    {#set aiFace.mNumIndices#} p $ cIntConv $ length mIndices
    newArray mIndices >>= ({#set aiFace.mIndices#} p)

data AiVertexWeight = AiVertexWeight
  { mVertexId'AiVertexWeight :: CUInt
  , mWeight'AiVertexWeight   :: CFloat
  }
instance Storable AiVertexWeight where
  sizeOf _ = {#sizeof aiVertexWeight#}
  alignment _ = 4
  peek p = do
    mV <- {#get aiVertexWeight.mVertexId#}
    mW <- {#get aiVertexWeight.mWeight#}
    return $ AiVertexWeight mV mW
  poke p (AiVertexWeight mV mW) = do
    {#set aiVertexWeight.mVertexId#} p mV
    {#set aiVertexWeight.mWeight#} p mW

data AiBone = AiBone
  { mName'AiBone         :: String
  --, mNumWeights   :: CUInt
  , mWeights'AiBone      :: [AiVertexWeight]
  , mOffsetMatrix'AiBone :: AiMatrix4x4
  }
instance Storable AiBone where
  sizeOf _ = {#sizeof aiBone#}
  alignment _ = 4
  peek = undefined
  poke = undefined
  --peek p = do
  --  mN <- liftM peek $ {#get aiBone.mName#} p
  --  mW <- {#get aiBone.mWeights#} p
  --  mO <- {#get aiBone.mOffsetMatrix#} p >>= peek
  --  return $ AiBone mN mW mO
  --poke p (AiBone mN mW mO) = do
  --  {#set aiBone.mName#} p mN
  --  {#set aiBone.mWeights#} p mW
  --  {#set aiBone.mOffsetMatrix#} p mO

data AiMesh = AiMesh
  { mPrimitiveTypes'AiMesh  :: AiPrimitiveType
  --, mNumVertices     :: CUInt
  , mVertices'AiMesh        :: [AiVector3D]
  , mNormals'AiMesh         :: [AiVector3D]
  , mTangents'AiMesh        :: [AiVector3D]
  , mBitangents'AiMesh      :: [AiVector3D]
  , mColors'AiMesh          :: [AiColor4D]
  , mTextureCoords'AiMesh   :: [AiVector3D]
  , mNumUVComponents'AiMesh :: CUInt
  --, mNumFaces        :: CUInt
  , mFaces'AiMesh           :: [AiFace]
  --, mNumBones        :: CUInt
  , mBones'AiMesh           :: [AiBone]
  , mMaterialIndex'AiMesh   :: CUInt
  , mName'AiMesh            :: String
  --, mNumAnimMeshes :: CUInt
  --, mAnimMeshes    :: [AiAnimMesh]
  }

{#pointer *aiMaterial  as AiMaterial#}
{#pointer *aiAnimation as AiAnimation#}
{#pointer *aiTexture   as AiTexture#}
{#pointer *aiLight     as AiLight#}
{#pointer *aiCamera    as AiCamera#}

data AiScene = AiScene
  { mFlags'AiScene         :: SceneFlags

  --, mNumMeshes     :: CUInt
  , mMeshes'AiScene        :: [AiMesh]

  --, mNumMaterials  :: CUInt
  , mMaterials'AiScene     :: [AiMaterial]

  --, mNumAnimations :: CUInt
  , mAnimations'AiScene    :: [AiAnimation]

  --, mNumTextures   :: CUInt
  , mTextures'AiScene      :: [AiTexture]

  --, mNumLights     :: CUInt
  , mLights'AiScene        :: [AiLight]

  --, mNumCameras    :: CUInt
  , mCameras'AiScene       :: [AiCamera]
  }
instance Storable AiScene where
  sizeOf _ = {#sizeof aiScene#}
  alignment _ = 4
  peek = undefined
  --peek p = do
  --  let mFlags = 
  poke p x = undefined--do
    -- {#set aiString.length#} p (cIntConv $ length'AiString x)
    -- str <- newCString $ data'AiString x
    -- {#set aiString.data#} p str

{#fun aiImportFile as ^
  {`String', cFromEnum `SceneFlags'} -> `AiScene' peek#}

-- aiImportFileEx
-- aiImportFileFromMemory

{#fun aiApplyPostProcessing as ^
  {id `AiScene', cFromEnum `AiPostProcessSteps'} -> `AiScene' id#}

--{#fun aiGetPredefinedLogStream as ^
--  {cFromEnum `AiDefaultLogStream', `String'} -> `AiLogStream' id#}

-- aiAttachLogStream
-- aiEnableVerboseLogging
-- aiDetachLogStream
-- aiDetachAllLogStreams

{#fun aiReleaseImport as ^
  {id `AiScene'} -> `()'#}

{#fun aiGetErrorString as ^
  {} -> `String'#}

{#fun aiIsExtensionSupported as ^
  {`String'} -> `Bool'#}

-- aiGetExtensionList
-- aiGetMemoryRequirements

{# fun aiSetImportPropertyInteger as ^
  {`String', `Int'} -> `()'#}

{# fun aiSetImportPropertyFloat as ^
  {`String', `Float'} -> `()'#}

--{# fun aiSetImportPropertyString as ^
--  {`String', `AiString'} -> `()'#}

-- aiCreateQuaternionFromMatrix
-- aiDecomposeMatrix
-- aiTransposematrix4
-- aiTransposematrix3
-- aiTransformVecByMatrix3
-- aiTransformVecByMatrix4
-- aiMultiplyMatrix3
-- aiMultiplyMatrix4
-- aiIdentityMatrix3
-- aiIdentityMatrix4
