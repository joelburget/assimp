{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module : Graphics.Formats.Assimp.Scene
-- Copyright : (c) Joel Burget 2011
-- License BSD3
--
-- Maintainer : Joel Burget <joelburget@gmail.com>
-- Stability : experimental
-- Portability : non-portable

module Graphics.Formats.Assimp.Scene (
    SceneFlags(..)
  , Node(..)
  , Scene(..)
  ) where

#include "typedefs.h"
#include "aiScene.h"       // Output data structure
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Foreign.Storable
import Foreign.C
import Foreign.Marshal.Array
import Control.Monad (liftM)
import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Vector
import Graphics.Formats.Assimp.Matrix
import Graphics.Formats.Assimp.Light
import Graphics.Formats.Assimp.Camera
import Graphics.Formats.Assimp.Mesh
import Graphics.Formats.Assimp.Anim
import Graphics.Formats.Assimp.Material
import Graphics.Formats.Assimp.Texture

data SceneFlags = FlagsIncomplete
                | FlagsValidated
                | FlagsValidationWarning
                | FlagsNonVerboseFormat
                | FlagsTerrain
                deriving Eq

instance Enum SceneFlags where
  fromEnum FlagsIncomplete        = #const AI_SCENE_FLAGS_INCOMPLETE
  fromEnum FlagsValidated         = #const AI_SCENE_FLAGS_VALIDATED
  fromEnum FlagsValidationWarning = #const AI_SCENE_FLAGS_VALIDATION_WARNING
  fromEnum FlagsNonVerboseFormat  = #const AI_SCENE_FLAGS_NON_VERBOSE_FORMAT
  fromEnum FlagsTerrain           = #const AI_SCENE_FLAGS_TERRAIN

  toEnum (#const AI_SCENE_FLAGS_INCOMPLETE)         = FlagsIncomplete
  toEnum (#const AI_SCENE_FLAGS_VALIDATED)          = FlagsValidated
  toEnum (#const AI_SCENE_FLAGS_VALIDATION_WARNING) = FlagsValidationWarning
  toEnum (#const AI_SCENE_FLAGS_NON_VERBOSE_FORMAT) = FlagsNonVerboseFormat
  toEnum (#const AI_SCENE_FLAGS_TERRAIN)            = FlagsTerrain
  toEnum unmatched                                  = error $ 
    "Return.toEnum: Cannot match " ++ show unmatched

instance Show SceneFlags where
  show FlagsIncomplete        = "FlagsIncomplete"
  show FlagsValidated         = "FlagsValidated"
  show FlagsValidationWarning = "FlagsValidationWarning"
  show FlagsNonVerboseFormat  = "FlagsNonVerboseFormat"
  show FlagsTerrain           = "FlagsTerrain"

data Node = Node
  { nodeName       :: String
  , transformation :: Mat4F
  , parent         :: Maybe Node
  , children       :: [Node]
  , nodeMeshes     :: [CUInt] -- Holds indices defining the node
  } deriving (Show)

instance Name Node where
  name = nodeName

instance Storable Node where
  sizeOf _ = #size aiNode
  alignment _ = #alignment aiNode
  peek p = do
    mName           <- liftM aiStringToString $ (#peek aiNode, mName) p
    mTransformation <- (#peek aiNode, mTransformation) p
    -- mParent      <- if mParentPtr == nullPtr 
    --                 then return Nothing 
    --                 else (#peek aiNode, mParent) p
    -- Temporary workaround so we don't end up in an infinite loop
    let mParent = Nothing 
    mNumChildren    <- (#peek aiNode, mNumChildren) p :: IO CUInt
    mChildrenP''    <- (#peek aiNode ,mChildren) p >>=
                         peekArray' (fromIntegral mNumChildren)
    mChildren       <- mapM peek mChildrenP''
    mNumMeshes      <- (#peek aiNode, mNumMeshes) p :: IO CUInt
    mMeshes         <- (#peek aiNode, mMeshes) p >>=
                         peekArray (fromIntegral mNumMeshes)
    return $ Node mName mTransformation mParent mChildren mMeshes
  poke = undefined

data Scene = Scene
  { flags      :: [SceneFlags]
  , rootNode   :: Node
  , meshes     :: [Mesh]
  , materials  :: [Material]
  , animations :: [Animation]
  , textures   :: [Texture]
  , lights     :: [Light]
  , cameras    :: [Camera]
  } deriving (Show)

instance Storable Scene where
  sizeOf _ = #size aiScene
  alignment _ = #alignment aiScene
  peek p = do
    mFlags         <- liftM toEnumList $ ((#peek aiScene, mFlags) p :: IO CUInt)
    mRootNode      <- (#peek aiScene, mRootNode) p >>= peek
    mNumMeshes     <- (#peek aiScene, mNumMeshes) p :: IO CUInt
    mMeshes'       <- (#peek aiScene, mMeshes) p >>= 
                        peekArray (fromIntegral mNumMeshes)
    mMeshes        <- mapM peek mMeshes'
    mNumMaterials  <- (#peek aiScene, mNumMaterials) p :: IO CUInt
    mMaterials'    <- (#peek aiScene, mMaterials) p >>= 
                        peekArray (fromIntegral mNumMaterials)
    mMaterials     <- mapM peek mMaterials'
    mNumAnimations <- (#peek aiScene, mNumAnimations) p :: IO CUInt
    mAnimations    <- (#peek aiScene, mAnimations) p >>= 
                        peekArrayPtr (fromIntegral mNumAnimations)
    mNumTextures   <- (#peek aiScene, mNumTextures) p :: IO CUInt
    mTextures      <- (#peek aiScene, mTextures) p >>= 
                        peekArrayPtr (fromIntegral mNumTextures)
    mNumLights     <- (#peek aiScene, mNumLights) p :: IO CUInt
    mLights        <- (#peek aiScene, mLights) p >>= 
                        peekArrayPtr (fromIntegral mNumLights)
    mNumCameras    <- (#peek aiScene, mNumCameras) p :: IO CUInt
    mCameras       <- (#peek aiScene, mCameras) p >>= 
                        peekArrayPtr (fromIntegral mNumCameras)
    return $ Scene mFlags mRootNode mMeshes mMaterials mAnimations mTextures
                   mLights mCameras
  poke = undefined
