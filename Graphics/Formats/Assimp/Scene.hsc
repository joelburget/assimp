{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.Formats.Assimp.Scene
-- Copyright   : (c) Joel Burget 2011-2012
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
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
import Graphics.Formats.Assimp.Matrix
import Graphics.Formats.Assimp.Light
import Graphics.Formats.Assimp.Camera
import Graphics.Formats.Assimp.Mesh
import Graphics.Formats.Assimp.Anim
import Graphics.Formats.Assimp.Material
import Graphics.Formats.Assimp.Texture

data SceneFlags = 
  -- |
  --
  -- Specifies that the scene data structure that was imported is not complete.
  -- This flag bypasses some internal validations and allows the import of
  -- animation skeletons, material libraries or camera animation paths using
  -- Assimp. Most applications won't support such data.
    FlagsIncomplete
  -- |
  --
  -- This flag is set by the validation postprocess-step
  -- (aiPostProcess_ValidateDS) if the validation is successful. In a validated
  -- scene you can be sure that any cross references in the data structure
  -- (e.g. vertex indices) are valid.
  | FlagsValidated
  -- |
  --
  -- This flag is set by the validation postprocess-step
  -- (aiPostProcess_ValidateDS) if the validation is successful but some issues
  -- have been found.  This can for example mean that a texture that does not
  -- exist is referenced by a material or that the bone weights for a vertex
  -- don't sum to 1.0 ... .  In most cases you should still be able to use the
  -- import. This flag could be useful for applications which don't capture
  -- Assimp's log output.
  | FlagsValidationWarning
  -- |
  --
  -- This flag is currently only set by the aiProcess_JoinIdenticalVertices
  -- step.  It indicates that the vertices of the output meshes aren't in the
  -- internal verbose format anymore. In the verbose format all vertices are
  -- unique, no vertex is ever referenced by more than one face.
  | FlagsNonVerboseFormat
  -- |
  --
  -- Denotes pure height-map terrain data. Pure terrains usually consist of
  -- quads, sometimes triangles, in a regular grid. The x,y coordinates of all
  -- vertex positions refer to the x,y coordinates on the terrain height map,
  -- the z-axis stores the elevation at a specific point.
  --
  -- TER (Terragen) and HMP (3D Game Studio) are height map formats.  @note
  -- Assimp is probably not the best choice for loading *huge* terrains - fully
  -- triangulated data takes extremely much free store and should be avoided as
  -- long as possible (typically you'll do the triangulation when you actually
  -- need to render it).
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

data Node = Node {
  -- | The name of the node. Use 'name' to access.
  --
  -- The name might be empty (length of zero) but all nodes which need to be
  -- accessed afterwards by bones or anims are usually named.  Multiple nodes
  -- may have the same name, but nodes which are accessed by bones (see
  -- 'aiBone' and 'aiMesh'::'mBones') *must* be unique.
  -- 
  -- Cameras and lights are assigned to a specific node name - if there are
  -- multiple nodes with this name, they're assigned to each of them.
  --
  -- There are no limitations regarding the characters contained in this text.
  -- You should be able to handle stuff like whitespace, tabs, linefeeds,
  -- quotation marks, ampersands, ... .
    nodeName       :: String
  -- | The transformation relative to the node's parent.
  , transformation :: Mat4F
  -- | Parent node. 'Nothing' if this node is the root node.
  , parent         :: Maybe Node
  -- | The child nodes of this node.
  , children       :: [Node]
  -- | The meshes of this node. Each entry is an index into the mesh
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

data Scene = Scene {
  -- | Any combination of the AI_SCENE_FLAGS_XXX flags.
  --
  -- Most applications will want to reject all scenes with 'FlagsIncomplete'.
    flags      :: [SceneFlags]
  -- | The root node of the hierarchy. 
  --
  -- There will always be at least the root node if the import was successful
  -- (and no special flags have been set).  Presence of further nodes depends
  -- on the format and content of the imported file.
  , rootNode   :: Node
  -- | The list of meshes. 
  --
  -- Use the indices given in your 'Node' to access this list. If the
  -- 'FlagsIncomplete' flag is not set there will always be at least ONE
  -- material.
  , meshes     :: [Mesh]
  -- | The list of materials. 
  --
  -- Use the index given in each 'Mesh' to access this list. If the
  -- 'FlagsIncomplete' flag is not set there will always be at least ONE
  -- material.
  , materials  :: [Material]
  -- | The list of animations. 
  --
  -- All animations imported from the given file are listed here.
  , animations :: [Animation]
  -- | The list of embedded textures.
  --
  -- Not many file formats embed their textures into the file. An example is
  -- Quake's MDL format (which is also used by some GameStudio versions)
  , textures   :: [Texture]
  -- | The list of light sources.
  --
  -- All light sources imported from the given file are listed here.
  , lights     :: [Light]
  -- | The list of cameras.
  --
  -- All cameras imported from the given file are listed here. The first
  -- camera (if existing) is the default camera view into the scene.
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
