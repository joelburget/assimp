{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Graphics.Formats.Assimp.Fun
-- Copyright   : (c) Joel Burget 2011-2012
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Defines functions for interacting with assimp

module Graphics.Formats.Assimp.Fun (
  -- * Basic functions
    importFile
  , applyPostProcessing
  -- * Accessing materials
  , get
  , getArray
  , getMaterialProperty
  , getTexture
  , GetTextureConf(..)
  , GetTextureRet(..)
  -- * Other functions
  , getErrorString
  , setImportProperty
  , getExtensionList
  , getMemoryRequirements
  , getTextureCount
  ) where

import Prelude hiding (max, catch)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable
import Data.Bits ((.|.))
import Control.Monad (liftM)
import Control.Applicative (liftA, (<$>), (<*>))
import Control.Exception (catch)
import Unsafe.Coerce (unsafeCoerce)
import Data.Vect (Vec3(Vec3), Vec4(Vec4))
import Data.List (foldl1')

import Graphics.Formats.Assimp.Config
import Graphics.Formats.Assimp.Color4D
import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Scene
import Graphics.Formats.Assimp.Material hiding (key)
import Graphics.Formats.Assimp.PostProcess
import Graphics.Formats.Assimp.Utils 

#include "material.h"
#include "typedefs.h"

-- aiImportFileEx
-- aiImportFileFromMemory

-- | Apply post-processing to an already-imported scene.
--
-- This is strictly equivalent to calling 'importFile' with the same flags.
-- However, you can use this separate function to inspect the imported scene
-- first to fine-tune your post-processing setup.
--
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

-- | Reads the given file and returns its content.
importFile :: String -> [PostProcessSteps] -> IO (Either String Scene)
importFile str psteps = do
  let psteps' = foldl1' (.|.) $ map (fromIntegral . fromEnum) psteps
  logLn "withCString"
  sceneptr <- withCString str $ flip aiImportFile psteps'
  logLn $ show sceneptr
  if sceneptr == nullPtr
    then liftM Left getErrorString
    else do
      logLn "peeking"
      scene <- peek sceneptr
      logLn "releasing"
      aiReleaseImport sceneptr
      return $ Right scene

foreign import ccall unsafe "aiImportFile"
  aiImportFile :: Ptr CChar -> CUInt -> IO (Ptr Scene)

foreign import ccall unsafe "aiReleaseImport"
  aiReleaseImport :: Ptr Scene -> IO ()

-- | Returns the error text of the last failed import process.
--
-- You probably won't ever need this.
getErrorString :: IO String
getErrorString = aiGetErrorString >>= peekCString

foreign import ccall unsafe "aiGetErrorString"
  aiGetErrorString :: IO (Ptr CChar)

-- | Get a list of all file extensions supported by ASSIMP.
--
-- If a file extension is contained in the list this does, of course, not mean
-- that ASSIMP is able to load all files with this extension.
getExtensionList :: IO String
getExtensionList = alloca $ \pstr -> do
  aiGetExtensionList pstr
  liftM aiStringToString $ peek pstr

foreign import ccall unsafe "aiGetExtensionList"
  aiGetExtensionList :: Ptr AiString -> IO ()

-- | Get the storage required by an imported asset.
getMemoryRequirements :: Scene -> IO MemoryInfo
getMemoryRequirements scene =
  with scene $ \pscene ->
    alloca $ \pmeminfo -> do
      aiGetMemoryRequirements pscene pmeminfo
      peek pmeminfo

foreign import ccall unsafe "aiGetMemoryRequirements"
  aiGetMemoryRequirements :: Ptr Scene -> Ptr MemoryInfo -> IO ()

-- | Set a global import property
--
-- This corresponds to 'aiSetImportPropertyInteger',
-- 'aiSetImportPropertyFloat', and 'aiSetImportPropertyString' from the C++
-- api. See 'Config' for the options.
setImportProperty :: Config -> IO ()
setImportProperty conf = let name = configName conf in case conf of
  GlobMeasureTime b                -> setImportPropertyInteger name $ fromEnum b
  PpCtMaxSmoothingAngle f          -> setImportPropertyFloat   name f
  PpGsnMaxSmoothingAngle f         -> setImportPropertyFloat   name f
  ImportMdlColormap str            -> setImportPropertyString  name str
  PpRrmExcludeList str             -> setImportPropertyString  name str
  PpPtvKeepHierarchy b             -> setImportPropertyInteger name $ fromEnum b
  PpPtvNormalize b                 -> setImportPropertyInteger name $ fromEnum b
  PpFdRemove b                     -> setImportPropertyInteger name $ fromEnum b
  PpOgExcludeList str              -> setImportPropertyString  name str
  PpSlmTriangleLimit i             -> setImportPropertyInteger name i
  PpSlmVertexLimit i               -> setImportPropertyInteger name i
  PpLbwMaxWeights i                -> setImportPropertyInteger name i
  PpIclPtcacheSize i               -> setImportPropertyInteger name i
  PpRvcFlags i                     -> setImportPropertyInteger name i
  PpSbpRemove i                    -> setImportPropertyInteger name i
  PpFidAnimAccuracy f              -> setImportPropertyFloat   name f
  PpTuvEvaluate i                  -> setImportPropertyInteger name i
  FavorSpeed b                     -> setImportPropertyInteger name $ fromEnum b
  ImportGlobalKeyframe i           -> setImportPropertyInteger name i
  ImportMd3Keyframe i              -> setImportPropertyInteger name i
  ImportMd2Keyframe i              -> setImportPropertyInteger name i
  ImportMdlKeyframe i              -> setImportPropertyInteger name i
  ImportMdcKeyframe i              -> setImportPropertyInteger name i
  ImportSmdKeyframe i              -> setImportPropertyInteger name i
  ImportUnrealKeyframe i           -> setImportPropertyInteger name i
  ImportAcSeparateBfCull b         -> setImportPropertyInteger name $ fromEnum b
  ImportAcEvalSubdivision b        -> setImportPropertyInteger name $ fromEnum b
  ImportUnrealHandleFlags b        -> setImportPropertyInteger name $ fromEnum b
  ImportTerMakeUvs b               -> setImportPropertyInteger name $ fromEnum b
  ImportAseReconstructNormals b    -> setImportPropertyInteger name $ fromEnum b
  ImportMd3HandleMultipart b       -> setImportPropertyInteger name $ fromEnum b
  ImportMd3SkinName str            -> setImportPropertyString  name str
  ImportMd3ShaderSrc str           -> setImportPropertyString  name str
  ImportLwoOneLayerOnly (Left str) -> setImportPropertyString  name str
  ImportLwoOneLayerOnly (Right i)  -> setImportPropertyInteger name i
  ImportMd5NoAnimAutoload b        -> setImportPropertyInteger name $ fromEnum b
  ImportLwsAnimStart i             -> setImportPropertyInteger name i
  ImportLwsAnimEnd i               -> setImportPropertyInteger name i
  ImportIrrAnimFps i               -> setImportPropertyInteger name i
  ImportOgreMaterialFile str       -> setImportPropertyString  name str

-- Set an integer property.
setImportPropertyInteger :: String -> Int -> IO ()
setImportPropertyInteger prop n =
  withCString prop $ \cprop -> aiSetImportPropertyInteger cprop (fromIntegral n)

foreign import ccall unsafe "aiSetImportPropertyInteger"
  aiSetImportPropertyInteger :: Ptr CChar -> CInt -> IO ()

-- Set a floating-point property.
setImportPropertyFloat :: String -> Float -> IO ()
setImportPropertyFloat prop f = withCString prop $ \prop' ->
  aiSetImportPropertyFloat prop' (fromRational . toRational $ f)

foreign import ccall unsafe "aiSetImportPropertyFloat"
  aiSetImportPropertyFloat :: Ptr CChar -> CFloat -> IO ()

-- Set a string property.
setImportPropertyString :: String -> String -> IO ()
setImportPropertyString prop s =
  withCString prop $ \prop' ->
    with (AiString s) $ \s' -> aiSetImportPropertyString prop' s'

foreign import ccall unsafe "aiSetImportPropertyString"
  aiSetImportPropertyString :: CString -> Ptr AiString -> IO ()

class ArrayGetter a where
  -- | Retrieve a list of values from a material with a specific key
  --
  -- Corresponds to '.Get(5 params)' from the C++ api and the
  -- following functions from the C api:
  --
  -- * aiGetMaterialFloatArray
  --
  -- * aiGetMaterialIntegerArray
  --
  -- Example:
  --
  -- > getArray mat (KeyUvTransform Diffuse 0) 4
  getArray :: Material -- ^ Material to query
           -> MatKey   -- ^ Key of the property to retrieve
           -> CUInt    -- ^ Maximum number of values to retrieve
           -> IO (Either String [a])

-- TODO(joel) - make type safe (must use correct MatKey)
class SingleGetter a where
  -- | Retrieve a value from a material with a specific key
  --
  -- Corresponds to '.Get(4 params)' from the C++ api and the following
  -- functions from the C api:
  --
  -- * aiGetMaterialFloat
  --
  -- * aiGetMaterialInteger
  --
  -- * aiGetMaterialColor
  --
  -- * aiGetMaterialString
  --
  -- Example:
  --
  -- > get mat KeyColorDiffuse
  get :: Material -- ^ Material to query
      -> MatKey   -- ^ Key of the property to retrieve
      -> IO (Either String a)

instance ArrayGetter Float where
  getArray = wrapArray getMaterialFloatArray

instance ArrayGetter Int where
  getArray = getMaterialIntArray

instance SingleGetter Float where
  get = getMaterialFloat

instance SingleGetter Int where
  get = getMaterialInt

instance SingleGetter Vec3 where
  get mat key = (liftM . liftM) (\(Vec4 r g b _) -> Vec3 r g b)
    (getMaterialColor mat key)

instance SingleGetter Color4F where
  get mat key = (liftM . liftM) Color4F (getMaterialColor mat key)

instance SingleGetter Vec4 where
  get = getMaterialColor

instance SingleGetter String where
  get = getMaterialString

mRet :: CInt -> Return
mRet  = toEnum . fromInteger . toInteger

wrap :: (Material -> MatKey -> IO (Either String a))
     -> (Material -> MatKey -> IO (Either String a))
wrap fun mat key = catch (fun mat key) $ \e ->
    return $ Left $ show (e :: IOError)

wrapArray :: (Material -> MatKey -> CUInt -> IO (Either String a))
         -> (Material -> MatKey -> CUInt -> IO (Either String a))
wrapArray fun mat key num = catch (fun mat key num) $ \e ->
    return $ Left $ show (e :: IOError)

-------------------------------------------------------------------------------

-- | Retrieve a material property with a specific key
getMaterialProperty :: Material -- ^ The material
                    -> MatKey   -- ^ A key
                    -> IO (Either String MaterialProperty)
getMaterialProperty mat key = do
  let (mKey, mType, mIndex) = matKeyToTuple key
  (ret, prop) <- getMaterialProperty' mat mKey mType mIndex
  case ret of
    ReturnSuccess     -> peekM prop
    ReturnFailure     -> return $ Left "Failed."
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
      alloca $ \ppmatprop -> (,)
        <$> liftA (toEnum . fromIntegral) 
              (aiGetMaterialProperty pmat pprop n1 n2 ppmatprop)
        <*> peek ppmatprop

foreign import ccall unsafe "aiGetMaterialProperty"
  aiGetMaterialProperty :: Ptr Material
                        -> Ptr CChar
                        -> CUInt
                        -> CUInt
                        -> Ptr (Ptr MaterialProperty)
                        -> IO CInt

-------------------------------------------------------------------------------

getMaterialFloatArray :: Material -- ^ The material
                      -> MatKey   -- ^ A key
                      -> CUInt    -- ^ Max number of values to retrieve
                      -> IO (Either String [Float])
getMaterialFloatArray mat key max = do
  logLn "gmfa 1"
  let (mKey, mType, mIndex) = matKeyToTuple key
  logLn "gmfa 6"
  (ret, arr, max') <- getMaterialFloatArray' mat mKey mType mIndex max
  logLn "gmfa 7"
  ret' <- case ret of
    ReturnSuccess     -> liftM (Right . (map unsafeCoerce))
                           $ peekArray (fromIntegral max') arr
    ReturnFailure     -> return $ Left "Failed."
    ReturnOutOfMemory -> return $ Left "Out of memory."
  logLn "gmfa 2"
  free arr
  logLn "gmfa 3"
  return ret'

getMaterialFloatArray' :: Material
                       -> String
                       -> CUInt
                       -> CUInt
                       -> CUInt
                       -> IO (Return, Ptr CFloat, CUInt)
getMaterialFloatArray' mat key typ idx max = do
  logLn "gmfa' infinity"
  logPrint mat
  with mat $ \pmat -> do
    logLn "gmfa' 5"
    return (ReturnSuccess, nullPtr, 0)
--    withCString key $ \ckey -> do
--      logLn "gmfa' 6"
--      with max $ \pmax -> do
--        logLn "gmfa' 1"
--        buf <- mallocBytes $ (sizeOf (undefined :: CFloat)) * (fromIntegral max)
--        logLn "gmfa' 2"
--        ret <- aiGetMaterialFloatArray pmat ckey typ idx buf pmax
--        logLn "gmfa' 3"
--        max' <- peek pmax
--        logLn "gmfa' 4"
--        return (toEnum . fromIntegral $ ret, buf, max')

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
    ReturnSuccess     -> liftM (Right . (map unsafeCoerce)) $
                           peekArray (fromIntegral max') arr
    ReturnFailure     -> return $ Left "Failed."
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
    ReturnFailure     -> Left "Failed."
    ReturnOutOfMemory -> Left "Out of memory."

getMaterialColor' :: Material -> String -> CUInt -> CUInt -> IO (Return, Vec4)
getMaterialColor' mat key typ idx =
  with mat $ \pmat ->
    withCString key $ \ckey ->
      alloca $ \pcolor -> (,)
        <$> liftA mRet (aiGetMaterialColor pmat ckey typ idx pcolor)
        <*> peek pcolor

foreign import ccall unsafe "aiGetMaterialColor"
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
    ReturnFailure     -> Left "Failed."
    ReturnOutOfMemory -> Left "Out of memory."

getMaterialString' :: Material 
                   -> String 
                   -> CUInt 
                   -> CUInt 
                   -> IO (Return, AiString)
getMaterialString' mat key typ idx =
  with mat $ \pmat ->
    withCString key $ \ckey ->
      alloca $ \pAiStr -> (,)
        <$> liftA mRet (aiGetMaterialString pmat ckey typ idx pAiStr)
        <*> peek pAiStr

foreign import ccall unsafe "aiGetMaterialString"
  aiGetMaterialString :: Ptr Material
                      -> Ptr CChar
                      -> CUInt
                      -> CUInt
                      -> Ptr AiString
                      -> IO CInt

-------------------------------------------------------------------------------

-- | Get the number of textures on a texture stack
getTextureCount :: Material -> TextureType -> IO CUInt
getTextureCount mat ttype = with mat $ \pmat -> 
  aiGetMaterialTextureCount pmat (fromIntegral . fromEnum $ ttype)

foreign import ccall unsafe "aiGetMaterialTextureCount"
  aiGetMaterialTextureCount :: Ptr Material -> CInt -> IO CUInt

-------------------------------------------------------------------------------

-- | Retrieve values pertaining to a particular texture slot from a material
-- structure
--
-- This function is provided just for convenience. You could also read the
-- texture by parsing all of its properties manually. This function bundles all
-- of them in a huge function monster.
--
getTexture :: Material       -- ^ Material to query
           -> TextureType    -- ^ Texture stack to read from (diffuse, specular, etc)
           -> CUInt          -- ^ Index of the texture
           -> GetTextureConf -- ^ Which values to retrieve
           -> IO (Either String GetTextureRet)
getTexture mat typ idx conf = do
  (ret, gtRet) <- getTexture' mat typ idx conf
  return $ case ret of
    ReturnSuccess     -> Right gtRet
    ReturnFailure     -> Left "Failed."
    ReturnOutOfMemory -> Left "Out of memory."

data GetTextureConf = GetTextureConf
  { confMapping :: Bool -- ^ Retrieve the texture mapping mode?
  , confUvindex :: Bool -- ^ Retrieve the index of the UV source channel?
  , confBlend   :: Bool -- ^ Retrieve the blend factor?
  , confOp      :: Bool -- ^ Retrieve the texture blend operation to perform between this texture and the previous texture?
  , confMapmode :: Bool -- ^ Retrieve the mapping modes to be used for the texture?
  , confFlags   :: Bool -- ^ Retrieve the texture flags?
  }

allGetTextureConf = GetTextureConf True True True True True True
noneGetTextureConf = GetTextureConf False False False False False False

data GetTextureRet = GetTextureRet
  { retPath    :: String               -- ^ Path to the texture
  , retMapping :: Maybe TextureMapping -- ^ Texture mapping mode
  , retUvindex :: Maybe CUInt          -- ^ Index of the UV source channel
  , retBlend   :: Maybe CFloat         -- ^ Blend factor
  , retOp      :: Maybe TextureOp      -- ^ Texture operation
  , retMapmode :: Maybe TextureMapMode -- ^ Texture mapping modes
  , retFlags   :: Maybe [TextureFlag]  -- ^ Texture flags
  }

getTexture' :: Material 
            -> TextureType 
            -> CUInt 
            -> GetTextureConf
            -> IO (Return, GetTextureRet)
getTexture' mat typ idx conf =
  with mat $ \pMat ->
    alloca $ \pPath ->
      (if confMapping conf then alloca else ($ nullPtr)) $ \pMapping ->
        (if confUvindex conf then alloca else ($ nullPtr)) $ \pUv ->
          (if confBlend conf then alloca else ($ nullPtr)) $ \pBlend ->
            (if confOp conf then alloca else ($ nullPtr)) $ \pOp ->
              (if confMapmode conf then alloca else ($ nullPtr)) $ \pMapMode ->
                (if confFlags conf then alloca else ($ nullPtr)) $ \pFlags -> do
                  ret <- mRet `liftM` aiGetMaterialTexture pMat 
                                        (fromIntegral . fromEnum $ typ) idx pPath 
                                        pMapping pUv pBlend pOp pMapMode pFlags
                  let helper ptr = if ptr /= nullPtr 
                                   then Just `liftM` peek ptr 
                                   else return Nothing
                  path'    <- aiStringToString `liftM` peek pPath
                  mapping' <- (liftM . liftM) (toEnum . fromIntegral) (helper pMapping)
                  uv'      <- helper pUv
                  blend'   <- helper pBlend
                  op'      <- (liftM . liftM) (toEnum . fromIntegral) (helper pOp)
                  mapmode' <- (liftM . liftM) (toEnum . fromIntegral) (helper pMapMode)
                  -- flags'   <- (liftM . liftM) (toEnum . fromIntegral) (helper pFlags)
                  -- TODO
                  let flags' = Nothing
                  return (ret, GetTextureRet path' mapping' uv' blend' op' mapmode' flags')

foreign import ccall unsafe "aiGetMaterialTexture"
  aiGetMaterialTexture :: Ptr Material       -- mat     [in]
                       -> CUInt              -- type    [in]
                       -> CUInt              -- index   [in]
                       -> Ptr AiString       -- path    [out]
                       -> Ptr CInt           -- mapping [out]
                       -> Ptr CUInt          -- uvindex [out]
                       -> Ptr CFloat         -- blend   [out]
                       -> Ptr CInt           -- op      [out]
                       -> Ptr CInt           -- mapmode [out]
                       -> Ptr CUInt          -- flags   [out]
                       -> IO CInt

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
