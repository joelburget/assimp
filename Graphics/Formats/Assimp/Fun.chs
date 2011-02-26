{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Formats.Assimp.Fun where

import C2HS
import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Storable

#include "../../assimp/include/assimp.h"        // Plain-C interface
#include "../../assimp/include/aiScene.h"       // Output data structure
#include "../../assimp/include/aiPostProcess.h" // Post processing flags
#include "./typedefs.h"

--peekAiScene = peek :: Ptr AiScene -> IO AiScene
{#fun aiImportFile as ^
  {`String', cFromEnum `SceneFlags'} -> `AiScenePtr' id#}

-- aiImportFileEx
-- aiImportFileFromMemory

{#fun aiApplyPostProcessing as ^
  {withT* `AiScene', cFromEnum `AiPostProcessSteps'} -> `AiScenePtr' id#}

--{#fun aiGetPredefinedLogStream as ^
--  {cFromEnum `AiDefaultLogStream', `String'} -> `AiLogStream' id#}

-- aiAttachLogStream
-- aiEnableVerboseLogging
-- aiDetachLogStream
-- aiDetachAllLogStreams

{#fun aiReleaseImport as ^
  {withT* `AiScene'} -> `()'#}

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
