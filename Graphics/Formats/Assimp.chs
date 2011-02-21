{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Formats.Assimp where

import C2HS
--import Foreign.Ptr
--import System.IO.Unsafe
--import Foreign.C

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
{#enum aiReturn as AiReturn {underscoreToCase} deriving (Show, Eq)#}
{#enum aiOrigin as AiOrigin {underscoreToCase} deriving (Show, Eq)#}
{#enum aiDefaultLogStream as AiDefaultLogStream {underscoreToCase} deriving (Show, Eq)#}

{#pointer *aiNode  as AiNode #}
{#pointer *aiScene as AiScene#}
{#pointer *aiPlane as AiPlane#}
{#pointer *aiRay   as AiRay  #}
{#pointer *aiColor3D as AiColor3D#}
{#pointer *aiString as AiString#}
{#pointer *aiMemoryInfo as AiMemoryInfo#}
{#pointer *aiLogStream as AiLogStream#}
{#pointer *aiQuaternion as AiQuaternion#}
{#pointer *aiMatrix3x3 as AiMatrix3x3#}
{#pointer *aiMatrix4x4 as AiMatrix4x4#}
{#pointer *aiVector3D as AiVector3D#}

{#fun aiImportFile as ^
  {`String', cFromEnum `SceneFlags'} -> `AiScene' id#}

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
