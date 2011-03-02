{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Formats.Assimp.Fun where

import C2HS
import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Storable

#include "assimp.h"        // Plain-C interface
#include "aiScene.h"       // Output data structure
#include "aiPostProcess.h" // Post processing flags
#include "typedefs.h"

--withT = with -- http://blog.ezyang.com/2010/06/call-and-fun-marshalling-redux/
with' :: (Storable a) => a -> (Ptr b -> IO c) -> IO c
with' x y = with x (y . castPtr)

peek' :: (Storable b) => Ptr a -> IO b
peek' = peek . castPtr

--peekAiScene = liftM (AiScene . castPtr) . peek
{#fun aiImportFile as ^
  {`String', cFromEnum `SceneFlags'} -> `AiScene' peek'*#}

-- aiImportFileEx
-- aiImportFileFromMemory

{#fun aiApplyPostProcessing as ^
  {with'* `AiScene', cFromEnum `AiPostProcessSteps'} -> `AiScene' peek'*#}

--{#fun aiGetPredefinedLogStream as ^
--  {cFromEnum `AiDefaultLogStream', `String'} -> `AiLogStream' id#}

-- aiAttachLogStream
-- aiEnableVerboseLogging
-- aiDetachLogStream
-- aiDetachAllLogStreams

{#fun aiReleaseImport as ^
  {with'* `AiScene'} -> `()'#}

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
-- aiGetLegalString
-- aiGetVersionMinor
-- aiGetVersionmajor
-- aiGetVersionRevision
-- aiGetCompileFlags
-- aiGetMaterialProperty
-- aiGetMaterialFloatArray
-- aiGetMaterialIntegerArray
-- aiGetMaterialColor
-- aiGetMaterialString
