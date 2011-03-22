{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Formats.Assimp.Fun (
    importFile
  , applyPostProcessing
  , releaseImport
  , getErrorString
  , isExtensionSupported
  , setImportPropertyInteger
  , setImportPropertyFloat
  ) where

import C2HS
import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Storable

#include "assimp.h"
#include "typedefs.h"

--withT = with -- http://blog.ezyang.com/2010/06/call-and-fun-marshalling-redux/
with' :: (Storable a) => a -> (Ptr b -> IO c) -> IO c
with' x y = with x (y . castPtr)

peek' :: (Storable b) => Ptr a -> IO b
peek' = peek . castPtr

{#fun aiImportFile as importFile
  {`String', cFromEnum `PostProcessSteps'} -> `Scene' peek'*#}

-- aiImportFileEx
-- aiImportFileFromMemory

{#fun aiApplyPostProcessing as applyPostProcessing 
  {with'* `Scene', cFromEnum `PostProcessSteps'} -> `Scene' peek'*#}

--{#fun aiGetPredefinedLogStream as ^
--  {cFromEnum `DefaultLogStream', `String'} -> `LogStream' id#}

-- aiAttachLogStream
-- aiEnableVerboseLogging
-- aiDetachLogStream
-- aiDetachAllLogStreams

{#fun aiReleaseImport as releaseImport
  {with'* `Scene'} -> `()'#}

{#fun aiGetErrorString as getErrorString
  {} -> `String'#}

{#fun aiIsExtensionSupported as isExtensionSupported
  {`String'} -> `Bool'#}

-- aiGetExtensionList
-- aiGetMemoryRequirements

{# fun aiSetImportPropertyInteger as setImportPropertyInteger
  {`String', `Int'} -> `()'#}

{# fun aiSetImportPropertyFloat as setImportPropertyFloat 
  {`String', `Float'} -> `()'#}

--{# fun aiSetImportPropertyString as ^
--  {`String', `String'} -> `()'#}

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
