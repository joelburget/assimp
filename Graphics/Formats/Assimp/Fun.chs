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
import Data.Word (Word)
import Data.Either (Either(Left,Right))
import Foreign.Storable ()
import Foreign.Marshal.Array (peekArray)
import Control.Monad (liftM)
import Unsafe.Coerce (unsafeCoerce)

import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Storable

#include "assimp.h"
#include "aiVersion.h"
#include "aiMaterial.h"
#include "typedefs.h"

--withT = with -- http://blog.ezyang.com/2010/06/call-and-fun-marshalling-redux/
with' :: (Storable a) => a -> (Ptr b -> IO c) -> IO c
with' x y = with x (y . castPtr)

peek' :: (Storable b) => Ptr a -> IO b
peek' = peek . castPtr

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

-- {#fun unsafe aiGetExtensionList as getExtensionList
--   {alloca- `Ptr CChar' peek'*} -> `()'#}

-- aiGetMemoryRequirements

{#fun unsafe aiSetImportPropertyInteger as setImportPropertyInteger
  {`String', `Int'} -> `()'#}

{#fun unsafe aiSetImportPropertyFloat as setImportPropertyFloat 
  {`String', `Float'} -> `()'#}

--{# fun aiSetImportPropertyString as ^
--  {`String', `String'} -> `()'#}

-- aiGetLegalString
{#fun unsafe aiGetVersionMinor as getVersionMinor
  {} -> `Int'#}
{#fun unsafe aiGetVersionMajor as getVersionMajor
  {} -> `Int'#}
{#fun unsafe aiGetVersionRevision as getVersionRevision
  {} -> `Int'#}
-- aiGetCompileFlags

mWord = fromInteger . toInteger
mRet  = toEnum . fromInteger . toInteger

-------------------------------------------------------------------------------

getMaterialProperty :: Material -> MatKey -> IO (Either String MaterialProperty)
getMaterialProperty mat key = do
  let (mKey, mType, mIndex) = matKeyToTuple key
  (ret, prop) <- getMaterialProperty' mat mKey mType mIndex
  case ret of
    ReturnSuccess     -> return prop
    ReturnFailure     -> return $ Left "Failed"
    ReturnOutOfMemory -> return $ Left "Out of memory."

{#fun unsafe aiGetMaterialProperty as getMaterialProperty'
  {with'*  `Material',
           `String'  ,
   mWord   `Word'    ,
   mWord   `Word'    ,
   alloca- `Either String MaterialProperty' peekM*} -> `Return' mRet#}
  where
    peekM :: Ptr (Ptr ()) -> IO (Either String MaterialProperty)
    peekM p = 
      if p == nullPtr 
      then return $ Left "No property found"
      else liftM Right $ peek p >>= peek'

-------------------------------------------------------------------------------
    
getMaterialFloatArray :: Material -> MatKey -> Word -> IO (Either String [Float])
getMaterialFloatArray mat key max = do
  let (mKey, mType, mIndex) = matKeyToTuple key
  (ret, arr, max) <- getMaterialFloatArray' mat mKey mType mIndex max
  case ret of
    ReturnSuccess     -> liftM (Right . (map unsafeCoerce)) 
                           $ peekArray (fromIntegral max) arr 
    ReturnFailure     -> return $ Left "Failed"
    ReturnOutOfMemory -> return $ Left "Out of memory."

{#fun unsafe aiGetMaterialFloatArray as getMaterialFloatArray'
  {with'*  `Material'     ,
           `String'       ,
   mWord   `Word'         ,
   mWord   `Word'         ,
   alloca- `Ptr CFloat' id,
   with'*  `Word'  peek'*} -> `Return' mRet#}

-------------------------------------------------------------------------------

getMaterialFloat :: Material -> MatKey -> IO (Either String Float)
getMaterialFloat mat key = do
  arr <- getMaterialFloatArray mat key 1
  case arr of
    Left err  -> return . Left $ err
    Right ans -> return . Right . head $ ans

-- aiGetMaterialIntegerArray
-- aiGetMaterialInteger
-- aiGetMaterialColor
-- aiGetMaterialString
-- aiGetMaterialTextureCount
-- aiGetMaterialTexture

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
