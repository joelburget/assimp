
module Graphics.Formats.Assimp.Scene (
    SceneFlags(..)
  , Node(..)
  , Scene(..)
  ) where

#include "aiScene.h"       // Output data structure

{#enum define SceneFlags {AI_SCENE_FLAGS_INCOMPLETE         as FlagsIncomplete
                        , AI_SCENE_FLAGS_VALIDATED          as FlagsValidated
                        , AI_SCENE_FLAGS_VALIDATION_WARNING as FlagsValidationWarning
                        , AI_SCENE_FLAGS_NON_VERBOSE_FORMAT as FlagsNonVerboseFormat
                        , AI_SCENE_FLAGS_TERRAIN            as FlagsTerrain
                        }#}
instance Show SceneFlags where
  show FlagsIncomplete        = "FlagsIncomplete"
  show FlagsValidated         = "FlagsValidated"
  show FlagsValidationWarning = "FlagsValidationWarning"
  show FlagsNonVerboseFormat  = "FlagsNonVerboseFormat"
  show FlagsTerrain           = "FlagsTerrain"

data Node = Node
  { nodeName       :: String
  , transformation :: Mat4
  , parent         :: Maybe Node
  , children       :: [Node]
  , nodeMeshes     :: [CUInt] -- Holds indices defining the node
  } deriving (Show)
{#pointer *aiNode as NodePtr -> Node#}

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
{#pointer *aiScene as ScenePtr -> Scene#}

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
