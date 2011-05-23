{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}

module Graphics.Formats.Assimp.Fun (
    importFile
  , applyPostProcessing
  , getErrorString
  , isExtensionSupported
  , setImportPropertyInteger
  , setImportPropertyFloat
  , getVersionMinor
  , getVersionMajor
  , getVersionRevision
  , get
  , getArray
  ) where

import C2HS
import Data.Either (Either(Left,Right))
import Foreign.Storable ()
import Foreign.Marshal.Array (peekArray)
import Control.Monad (liftM)
import Unsafe.Coerce (unsafeCoerce)
import Data.Vect (Vec3(Vec3), Vec4(Vec4))
import Control.Applicative ((<$>), (<*>))
import Data.Bits ((.|.))
import Data.List (foldl1')

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

importFile :: String -> [PostProcessSteps] -> IO (Either String Scene)
importFile str psteps = do 
  let psteps' = foldl1' (.|.) $ map cFromEnum psteps
  sceneptr <- withCString str $ \x -> {#call unsafe aiImportFile#} x psteps'
  if sceneptr == nullPtr
    then liftM Left getErrorString
    else do
      scene <- peek' sceneptr
      {#call unsafe aiReleaseImport#} sceneptr
      return $ Right scene

{#fun unsafe aiGetErrorString as getErrorString
  {} -> `String'#}

{#fun unsafe aiIsExtensionSupported as isExtensionSupported
  {`String'} -> `Bool'#}

-- {#fun unsafe aiGetExtensionList as getExtensionList
--   {alloca- `AiString' peek'*} -> `()'#}

-- aiGetMemoryRequirements

{#fun unsafe aiSetImportPropertyInteger as setImportPropertyInteger
  {`String', `Int'} -> `()'#}

{#fun unsafe aiSetImportPropertyFloat as setImportPropertyFloat 
  {`String', `Float'} -> `()'#}

--{# fun aiSetImportPropertyString as ^
--  {`String', `String'} -> `()'#}

{#fun unsafe aiGetLegalString as getLegalString
  {} -> `String'#}

{#fun unsafe aiGetVersionMinor as getVersionMinor
  {} -> `CUInt' unsafeCoerce#}

{#fun unsafe aiGetVersionMajor as getVersionMajor
  {} -> `CUInt' unsafeCoerce#}

{#fun unsafe aiGetVersionRevision as getVersionRevision
  {} -> `CUInt' unsafeCoerce#}

{#fun unsafe aiGetCompileFlags as getCompileFlags
  {} -> `CompileFlags' convert#}
  where convert = toEnum . cIntConv

class ArrayGetter a where
  getArray :: Material -> MatKey -> CUInt -> IO (Either String [a])

class SingleGetter a where
  get :: Material -> MatKey -> IO (Either String a)

instance ArrayGetter Float where
  getArray = getMaterialFloatArray

instance ArrayGetter Int where
  getArray = getMaterialIntArray

instance SingleGetter Float where
  get = getMaterialFloat

instance SingleGetter Int where
  get = getMaterialInt

instance SingleGetter Vec3 where
-- Also another case?
  get mat key = (liftM . liftM) (\(Vec4 r g b a) -> Vec3 r g b) 
    (getMaterialColor mat key)

instance SingleGetter Vec4 where
  get = getMaterialColor

instance SingleGetter String where
  get = getMaterialString

mCUInt = fromInteger . toInteger
mRet  = toEnum . fromInteger . toInteger

-------------------------------------------------------------------------------

{- |
 - Retrieve a material property with a specific key from the material
 -}
getMaterialProperty :: Material -- ^ The material
                    -> MatKey   -- ^ A key
                    -> IO (Either String MaterialProperty)
getMaterialProperty mat key = do
  let (mKey, mType, mIndex) = matKeyToTuple key
  (ret, prop) <- getMaterialProperty' mat mKey mType mIndex
  case ret of
    ReturnSuccess     -> peekM prop
    ReturnFailure     -> return $ Left "Failed"
    ReturnOutOfMemory -> return $ Left "Out of memory."
  where
    peekM :: Ptr MaterialProperty -> IO (Either String MaterialProperty)
    peekM p = 
      if p == nullPtr
      then return $ Left "No property found"
      else liftM Right $ peek p

{#fun unsafe aiGetMaterialProperty as getMaterialProperty'
  {with'*  `Material',
           `String'  ,
   mCUInt   `CUInt'    ,
   mCUInt   `CUInt'    ,
   alloca- `Ptr MaterialProperty' peek'*} -> `Return' mRet#}

-------------------------------------------------------------------------------
{- |
 - Retrieve an array of float values with a specific key from the material
 - Example:
 - > getMaterialFloatArray mat (KeyUvTransform Diffuse 0) 4
 -}
getMaterialFloatArray :: Material -- ^ The material
                      -> MatKey   -- ^ A key
                      -> CUInt    -- ^ Max number of values to retrieve
                      -> IO (Either String [Float])
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
   mCUInt   `CUInt'         ,
   mCUInt   `CUInt'         ,
   alloca- `Ptr CFloat' id,
   with'*  `CUInt'  peek'*} -> `Return' mRet#}

-------------------------------------------------------------------------------

getMaterialFloat :: Material -> MatKey -> IO (Either String Float)
getMaterialFloat mat key = do
  arr <- getMaterialFloatArray mat key 1
  return $ case arr of
    Left err  -> Left err
    Right ans -> Right $ head ans

-------------------------------------------------------------------------------

getMaterialIntArray :: Material -> MatKey -> CUInt -> IO (Either String [Int])
getMaterialIntArray mat key max = do
  let (mKey, mType, mIndex) = matKeyToTuple key
  (ret, arr, max) <- getMaterialIntArray' mat mKey mType mIndex max
  case ret of
    ReturnSuccess     -> liftM (Right . (map unsafeCoerce)) 
                           $ peekArray (fromIntegral max) arr 
    ReturnFailure     -> return $ Left "Failed"
    ReturnOutOfMemory -> return $ Left "Out of memory."

{#fun unsafe aiGetMaterialIntegerArray as getMaterialIntArray'
  {with'*  `Material'   ,
           `String'     ,
   mCUInt   `CUInt'       ,
   mCUInt   `CUInt'       ,
   alloca- `Ptr CInt' id,
   with'*  `CUInt'  peek'*} -> `Return' mRet#}

-------------------------------------------------------------------------------

getMaterialInt :: Material -> MatKey -> IO (Either String Int)
getMaterialInt mat key = do
  arr <- getMaterialIntArray mat key 1
  return $ case arr of
    Left err  -> Left err
    Right ans -> Right $ head ans

-------------------------------------------------------------------------------

getMaterialColor :: Material -> MatKey -> IO (Either String Vec4)
getMaterialColor mat key = do
  let (mKey, mType, mIndex) = matKeyToTuple key
  (ret, vec) <- getMaterialColor' mat mKey mType mIndex
  return $ case ret of
    ReturnSuccess     -> Right vec
    ReturnFailure     -> Left "Failed"
    ReturnOutOfMemory -> Left "Out of memory."

-- Can't get this to work :(
-- {#fun unsafe aiGetMaterialColor as getMaterialColor'
--   {with'* `Material',
--           `String'  ,
--    mCUInt  `CUInt'    ,
--    mCUInt  `CUInt'    ,
--    alloca- `Vec4' peek'*} -> `Return' mRet#}

getMaterialColor' :: Material -> String -> CUInt -> CUInt -> IO (Return, Vec4)
getMaterialColor' a1 a2 a3 a4 =
  with a1 $ \a1' -> 
  withCString a2 $ \a2' -> 
  alloca $ \a5' -> 
  getMaterialColor''_ a1' a2' (mCUInt a3) (mCUInt a4) a5' >>= \res ->
  peek a5' >>= \a5'' -> 
  return (mRet res, a5'')

foreign import ccall unsafe "Graphics/Formats/Assimp/Fun.chs.h aiGetMaterialColor"
  getMaterialColor''_ :: Ptr Material -> Ptr CChar -> CUInt -> CUInt -> Ptr Vec4 -> IO CInt

-------------------------------------------------------------------------------

getMaterialString :: Material -> MatKey -> IO (Either String String)
getMaterialString mat key = do
  let (mKey, mType, mIndex) = matKeyToTuple key
  (ret, (AiString str)) <- getMaterialString' mat mKey mType mIndex
  return $ case ret of
    ReturnSuccess     -> Right str
    ReturnFailure     -> Left "Failed"
    ReturnOutOfMemory -> Left "Out of memory."
    
-- {#fun unsafe aiGetMaterialString as getMaterialString
--   {with'*  `Material',
--            `String'  ,
--    mCUInt   `CUInt'    ,
--    mCUInt   `CUInt'    ,
--    alloca- `AiString' peek'*} -> `Return' mRet#}

getMaterialString' :: Material -> String -> CUInt -> CUInt -> IO (Return, AiString)
getMaterialString' a1 a2 a3 a4 =
  with a1 $ \a1' -> 
  withCString a2 $ \a2' -> 
  alloca $ \a5' -> 
  getMaterialString''_ a1' a2' (mCUInt a3) (mCUInt a4) a5' >>= \res ->
  peek a5' >>= \a5'' -> 
  return (mRet res, a5'')

foreign import ccall unsafe "Graphics/Formats/Assimp/Fun.chs.h aiGetMaterialString"
  getMaterialString''_ :: Ptr Material -> Ptr CChar -> CUInt -> CUInt -> Ptr AiString -> IO CInt

-------------------------------------------------------------------------------

{#fun unsafe aiGetMaterialTextureCount as getTextureCount
  {with'*  `Material',
   f `TextureType'} -> `CUInt' unsafeCoerce#}
  where
    f :: TextureType -> CInt
    f = cIntConv . fromEnum

-------------------------------------------------------------------------------

-- getTexture :: 

-- {#fun unsafe aiGetMaterialTexture as getTexture'
--   {with'*  `Material'   ,
--    f `TextureType',
--    mCUInt `CUInt'
--            `String'     ,
--    alloca-  `CUInt'  peek'*} -> `Return' mRet#}

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
