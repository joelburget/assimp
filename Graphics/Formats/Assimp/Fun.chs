{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Formats.Assimp.Fun (
    importFile
  , applyPostProcessing
--  , releaseImport
  , getErrorString
  , isExtensionSupported
  , setImportPropertyInteger
  , setImportPropertyFloat
  , getVersionMinor
  , getVersionMajor
  , getVersionRevision
  ) where

import C2HS
import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Storable

#include "assimp.h"
#include "aiVersion.h"
#include "typedefs.h"

--withT = with -- http://blog.ezyang.com/2010/06/call-and-fun-marshalling-redux/
with' :: (Storable a) => a -> (Ptr b -> IO c) -> IO c
with' x y = with x (y . castPtr)

peek' :: (Storable b) => Ptr a -> IO b
peek' = peek . castPtr

-- Removing these for now. I don't see any reason to use them. You can't
-- release resources after using this importFile because you don't get the
-- pointer.
-- {#fun unsafe aiImportFile as importFile
--   {`String', cFromEnum `PostProcessSteps'} -> `Scene' peek'*#}
-- 
-- {#fun unsafe aiReleaseImport as releaseImport
--   {with'* `Scene'} -> `()'#}

-- aiImportFileEx
-- aiImportFileFromMemory

{#fun unsafe aiApplyPostProcessing as applyPostProcessing 
  {with'* `Scene', cFromEnum `PostProcessSteps'} -> `Scene' peek'*#}

--{#fun aiGetPredefinedLogStream as ^
--  {cFromEnum `DefaultLogStream', `String'} -> `LogStream' id#}

-- aiAttachLogStream
-- aiEnableVerboseLogging
-- aiDetachLogStream
-- aiDetachAllLogStreams

importFile :: String -> PostProcessSteps -> IO Scene
importFile str psteps = do 
  let psteps' = cFromEnum psteps
  sceneptr <- withCString str $ \x -> {#call unsafe aiImportFile#} x psteps'
  scene <- peek' sceneptr
  {#call unsafe aiReleaseImport#} sceneptr
  return scene

{#fun unsafe aiGetErrorString as getErrorString
  {} -> `String'#}

{#fun unsafe aiIsExtensionSupported as isExtensionSupported
  {`String'} -> `Bool'#}

-- aiGetExtensionList
-- aiGetMemoryRequirements

{# fun unsafe aiSetImportPropertyInteger as setImportPropertyInteger
  {`String', `Int'} -> `()'#}

{# fun unsafe aiSetImportPropertyFloat as setImportPropertyFloat 
  {`String', `Float'} -> `()'#}

--{# fun aiSetImportPropertyString as ^
--  {`String', `String'} -> `()'#}

-- aiGetLegalString
{# fun unsafe aiGetVersionMinor as getVersionMinor
  {} -> `Int'#}
{# fun unsafe aiGetVersionMajor as getVersionMajor
  {} -> `Int'#}
{# fun unsafe aiGetVersionRevision as getVersionRevision
  {} -> `Int'#}
-- aiGetCompileFlags
-- aiGetMaterialProperty
-- aiGetMaterialFloatArray
-- aiGetMaterialIntegerArray
-- aiGetMaterialColor
-- aiGetMaterialString

-- I'm not sure whether or not I will implement these or not. I guess I'll
-- check the source to see if they're pure. They could easily be implemented in
-- pure haskell.

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
