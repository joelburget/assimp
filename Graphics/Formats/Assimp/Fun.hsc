{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module : Graphics.Formats.Assimp.Fun
-- Copyright : (c) Joel Burget 2011
-- License BSD3
--
-- Maintainer : Joel Burget <joelburget@gmail.com>
-- Stability : experimental
-- Portability : non-portable
--
-- Defines functions for interacting with assimp

module Graphics.Formats.Assimp.Fun (
  -- * Basics
    importFile
  , applyPostProcessing
  -- * Accessing materials
  , get
  , getArray
  -- * Currently unused
  , getErrorString
  , setImportPropertyInteger
  , setImportPropertyFloat
  , setImportPropertyString
  , getExtensionList
  , getMemoryRequirements
  , getMaterialProperty
  , getTextureCount
  ) where

import Prelude hiding (max)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable
import Data.Bits ((.|.))
import Control.Monad (liftM)
import Unsafe.Coerce (unsafeCoerce)
import Data.Vect (Vec3(Vec3), Vec4(Vec4))
import Data.List (foldl1')

import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Scene
import Graphics.Formats.Assimp.Material hiding (key)
import Graphics.Formats.Assimp.PostProcess

#include "assimp.h"
#include "aiMaterial.h"
#include "typedefs.h"

-- aiImportFileEx
-- aiImportFileFromMemory

applyPostProcessing :: Scene -> PostProcessSteps -> IO Scene
applyPostProcessing scene steps =
  with scene $ \pscene -> 
    aiApplyPostProcessing pscene (fromIntegral . fromEnum $ steps) >>= peek

foreign import ccall unsafe "aiApplyPostProcessing"
  aiApplyPostProcessing :: Ptr Scene -> CUInt -> IO (Ptr Scene)

-- aiGetPredefinedLogStream
-- aiAttachLogStream
-- aiEnableVerboseLogging
-- aiDetachLogStream
-- aiDetachAllLogStreams

importFile :: String -> [PostProcessSteps] -> IO (Either String Scene)
importFile str psteps = do 
  let psteps' = foldl1' (.|.) $ map (fromIntegral . fromEnum) psteps
  sceneptr <- withCString str $ \x -> aiImportFile x psteps'
  if sceneptr == nullPtr
    then liftM Left getErrorString
    else do
      scene <- peek sceneptr
      aiReleaseImport sceneptr
      return $ Right scene

foreign import ccall unsafe "aiImportFile"
  aiImportFile :: Ptr CChar -> CUInt -> IO (Ptr Scene)

foreign import ccall unsafe "aiReleaseImport"
  aiReleaseImport :: Ptr Scene -> IO ()

getErrorString :: IO String
getErrorString = aiGetErrorString >>= peekCString

foreign import ccall unsafe "aiGetErrorString"
  aiGetErrorString :: IO (Ptr CChar)

getExtensionList :: IO String
getExtensionList = alloca $ \pstr -> do
  aiGetExtensionList pstr
  liftM aiStringToString $ peek pstr

foreign import ccall unsafe "aiGetExtensionList"
  aiGetExtensionList :: Ptr AiString -> IO ()

getMemoryRequirements :: Scene -> IO MemoryInfo
getMemoryRequirements scene = 
  with scene $ \pscene ->
    alloca $ \pmeminfo -> do
      aiGetMemoryRequirements pscene pmeminfo
      peek pmeminfo
  
foreign import ccall unsafe "aiGetMemoryRequirements" 
  aiGetMemoryRequirements :: Ptr Scene -> Ptr MemoryInfo -> IO ()

setImportPropertyInteger :: String -> Int -> IO ()
setImportPropertyInteger prop n =
  withCString prop $ \prop' -> aiSetImportPropertyInteger prop' (fromIntegral n)

foreign import ccall unsafe "aiSetImportPropertyInteger"
  aiSetImportPropertyInteger :: Ptr CChar -> CInt -> IO ()

setImportPropertyFloat :: String -> Float -> IO ()
setImportPropertyFloat prop f = withCString prop $ \prop' -> 
  aiSetImportPropertyFloat prop' (fromRational . toRational $ f)

foreign import ccall unsafe "aiSetImportPropertyFloat"
  aiSetImportPropertyFloat :: Ptr CChar -> CFloat -> IO ()

setImportPropertyString :: String -> String -> IO ()
setImportPropertyString prop s =
  withCString prop $ \prop' ->
    with (AiString s) $ \s' -> aiSetImportPropertyString prop' s'

foreign import ccall unsafe "aiSetImportPropertyString"
  aiSetImportPropertyString :: CString -> Ptr AiString -> IO ()

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
  get mat key = (liftM . liftM) (\(Vec4 r g b _) -> Vec3 r g b) 
    (getMaterialColor mat key)

instance SingleGetter Vec4 where
  get = getMaterialColor

instance SingleGetter String where
  get = getMaterialString

mRet :: CInt -> Return
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

getMaterialProperty' :: Material 
                     -> String 
                     -> CUInt 
                     -> CUInt 
                     -> IO (Return, Ptr MaterialProperty)
getMaterialProperty' mat prop n1 n2 =
  with mat $ \pmat ->
    withCString prop $ \pprop ->
      alloca $ \ppmatprop -> do
        ret <- aiGetMaterialProperty pmat pprop n1 n2 ppmatprop
        pmatprop <- peek ppmatprop
        return (toEnum . fromIntegral $ ret, pmatprop)

foreign import ccall unsafe "aiGetMaterialProperty"
  aiGetMaterialProperty :: Ptr Material 
                        -> Ptr CChar 
                        -> CUInt 
                        -> CUInt 
                        -> Ptr (Ptr MaterialProperty) 
                        -> IO CInt

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
  (ret, arr, max') <- getMaterialFloatArray' mat mKey mType mIndex max
  ret' <- case ret of
    ReturnSuccess     -> liftM (Right . (map unsafeCoerce)) 
                           $ peekArray (fromIntegral max') arr 
    ReturnFailure     -> return $ Left "Failed"
    ReturnOutOfMemory -> return $ Left "Out of memory."
  free arr
  return ret'

getMaterialFloatArray' :: Material
                       -> String
                       -> CUInt
                       -> CUInt
                       -> CUInt
                       -> IO (Return, Ptr CFloat, CUInt)
getMaterialFloatArray' mat key typ idx max =
  with mat $ \pmat ->
    withCString key $ \ckey ->
      with max $ \pmax -> do
        buf <- mallocBytes ((sizeOf (undefined :: CFloat)) * (fromIntegral max))
        ret <- aiGetMaterialFloatArray pmat ckey typ idx buf pmax
        max' <- peek pmax
        return (toEnum . fromIntegral $ ret, buf, max')

foreign import ccall unsafe "aiGetMaterialFloatArray"
  aiGetMaterialFloatArray :: Ptr Material 
                          -> Ptr CChar 
                          -> CUInt 
                          -> CUInt 
                          -> Ptr CFloat 
                          -> Ptr CUInt 
                          -> IO CInt

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
  (ret, arr, max') <- getMaterialIntArray' mat mKey mType mIndex max
  ret' <- case ret of
    ReturnSuccess     -> liftM (Right . (map unsafeCoerce)) 
                           $ peekArray (fromIntegral max') arr 
    ReturnFailure     -> return $ Left "Failed"
    ReturnOutOfMemory -> return $ Left "Out of memory."
  free arr
  return ret'

getMaterialIntArray' :: Material
                       -> String
                       -> CUInt
                       -> CUInt
                       -> CUInt
                       -> IO (Return, Ptr CInt, CUInt)
getMaterialIntArray' mat key typ idx max =
  with mat $ \pmat ->
    withCString key $ \ckey ->
      with max $ \pmax -> do
        buf <- mallocBytes ((sizeOf (undefined :: CInt)) * (fromIntegral max))
        ret <- aiGetMaterialIntegerArray pmat ckey typ idx buf pmax
        max' <- peek pmax
        return (toEnum . fromIntegral $ ret, buf, max')

foreign import ccall unsafe "aiGetMaterialIntegerArray"
  aiGetMaterialIntegerArray :: Ptr Material 
                            -> Ptr CChar 
                            -> CUInt 
                            -> CUInt 
                            -> Ptr CInt 
                            -> Ptr CUInt 
                            -> IO CInt

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
  aiGetMaterialColor a1' a2' a3 a4 a5' >>= \res ->
  peek a5' >>= \a5'' -> 
  return (mRet res, a5'')

foreign import ccall unsafe "Graphics/Formats/Assimp/Fun.chs.h aiGetMaterialColor"
  aiGetMaterialColor :: Ptr Material 
                     -> Ptr CChar 
                     -> CUInt 
                     -> CUInt 
                     -> Ptr Vec4 
                     -> IO CInt

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
  aiGetMaterialString a1' a2' a3 a4 a5' >>= \res ->
  peek a5' >>= \a5'' -> 
  return (mRet res, a5'')

foreign import ccall unsafe "Graphics/Formats/Assimp/Fun.chs.h aiGetMaterialString"
  aiGetMaterialString :: Ptr Material 
                      -> Ptr CChar 
                      -> CUInt 
                      -> CUInt 
                      -> Ptr AiString 
                      -> IO CInt

-------------------------------------------------------------------------------

getTextureCount :: Material -> TextureType -> IO CUInt
getTextureCount mat ttype =
  with mat $ \pmat -> aiGetMaterialTextureCount pmat (fromIntegral . fromEnum $ ttype)

foreign import ccall unsafe "aiGetMaterialTextureCount"
  aiGetMaterialTextureCount :: Ptr Material -> CInt -> IO CUInt

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
