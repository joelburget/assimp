{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.Formats.Assimp.Types
-- Copyright   : (c) Joel Burget 2011-2012
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Corresponds to aiTypes.h

module Graphics.Formats.Assimp.Types (
    Return(..)
  , Origin(..)
  , Plane(..)
  , Ray(..)
  , Color3F(Color3F)
  , MemoryInfo(..)
  , AiString(..)
  , position
  , peekArray'
  , peekArrayPtr
  , toEnumList
  , aiStringToString
  , Position(position)
  , Name(name)
  ) where

import Data.Bits
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String (peekCStringLen)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable
import Control.Applicative((<$>), (<*>))
import Data.Vect.Float (Vec3(Vec3))

#include "assimp.h"        // Plain-C interface
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | Standard return type for some library functions.
data Return 
  -- | Indicates that a function was successful
  = ReturnSuccess
  -- | Indicates that a function failed
  | ReturnFailure
  -- | Indicates that not enough memory was available to perform the requested
  -- operation
  | ReturnOutOfMemory
  deriving (Show,Eq)

instance Enum Return where
  fromEnum ReturnSuccess = 0
  fromEnum ReturnFailure = (-1)
  fromEnum ReturnOutOfMemory = (-3)

  toEnum 0 = ReturnSuccess
  toEnum (-1) = ReturnFailure
  toEnum (-3) = ReturnOutOfMemory
  toEnum unmatched = error $ "Return.toEnum: Cannot match " ++ show unmatched

-- | Seek origins (for the virtual file system API).
data Origin -- TODO(joel) used?
  -- | Beginning of the file
  = Set
  -- | Current position of the file pointer
  | Cur
  -- | End of the file, offsets must be negative
  | End
  deriving (Show, Eq)

data Plane = Plane
  { planeA :: Float
  , planeB :: Float
  , planeC :: Float
  , planeD :: Float
  } deriving (Show)

instance Storable Plane where
  sizeOf _ = #size aiPlane
  alignment _ = #alignment aiPlane
  peek p = Plane <$> (#peek aiPlane, a) p
                   <*> (#peek aiPlane, b) p
                   <*> (#peek aiPlane, c) p
                   <*> (#peek aiPlane, d) p
  poke = undefined

data Ray = Ray 
  { rayPos :: Vec3
  , rayDir :: Vec3
  } deriving (Show)

instance Storable Ray where
  sizeOf _ = #size aiRay
  alignment _ = #alignment aiRay
  peek p = Ray <$> (#peek aiRay, pos) p
               <*> (#peek aiRay, dir) p
  poke = undefined

newtype Color3F = Color3F Vec3 deriving Show

instance Storable Color3F where
  sizeOf _ = #size aiColor3D
  alignment _ = #alignment aiColor3D
  peek p = Color3F <$> (Vec3 <$> (#peek aiColor3D, r) p
                             <*> (#peek aiColor3D, g) p
                             <*> (#peek aiColor3D, b) p)
  poke = undefined

-- | Stores the memory requirements for different components (e.g. meshes,
-- materials, animations) of an import. All sizes are in bytes.
--
-- see 'getMemoryRequirements'
data MemoryInfo = MemoryInfo {
    memoryInfoTextures   :: CUInt -- ^ Storage allocated for texture data
  , memoryInfoMaterials  :: CUInt -- ^ Storage allocated for material data
  , memoryInfoMeshes     :: CUInt -- ^ Storage allocated for mesh data
  , memoryInfoNodes      :: CUInt -- ^ Storage allocated for node data
  , memoryInfoAnimations :: CUInt -- ^ Storage allocated for animation data
  , memoryInfoCameras    :: CUInt -- ^ Storage allocated for camera data
  , memoryInfoLights     :: CUInt -- ^ Storage allocated for light data
  , memoryInfoTotal      :: CUInt -- ^ Storage allocated for the full import
  } deriving (Show)

instance Storable MemoryInfo where
  sizeOf _ = #size aiMemoryInfo
  alignment _ = #alignment aiMemoryInfo
  peek p = do
    text       <- (#peek aiMemoryInfo, textures) p
    materials  <- (#peek aiMemoryInfo, materials) p
    meshes     <- (#peek aiMemoryInfo, meshes) p
    nodes      <- (#peek aiMemoryInfo, nodes) p
    animations <- (#peek aiMemoryInfo, animations) p
    cameras    <- (#peek aiMemoryInfo, cameras) p
    lights     <- (#peek aiMemoryInfo, lights) p
    total      <- (#peek aiMemoryInfo, total) p
    return $ MemoryInfo text materials meshes nodes animations cameras lights 
                        total
  poke = undefined

newtype AiString = AiString String deriving (Show)

instance Storable AiString where
  sizeOf _ = #size aiString
  alignment _ = #alignment aiString
  peek p = do
    start <- ((#peek aiString, data) p) :: IO (Ptr CChar)
    if start == nullPtr
      then return $ AiString ""
      else do
        len <- (#peek aiString, length) p
        -- So the string is stored as an array, we need to pass a pointer to
        -- peekCStringLen, so we can't just (#peek aiString, data) because that
        -- would give us the value of the first word of the string instead of
        -- the pointer, so we have to create a pointer.
        str <- peekCStringLen (p `plusPtr` (#offset aiString, data), len)
        return $ AiString str
  poke = undefined

aiStringToString :: AiString -> String
aiStringToString (AiString s) = s

class Position a where
  position :: a -> Vec3

class Name a where
  name :: a -> String

-- Same as peekArray but checks for a null pointer
peekArray' :: Storable a => Int -> Ptr a -> IO [a]
peekArray' n ptr = if ptr == nullPtr
                   then return []
                   else peekArray n ptr

peekArrayPtr :: (Storable a) => Int -> (Ptr (Ptr a)) -> IO [a]
peekArrayPtr n p = peekArray' (fromIntegral n) p >>= mapM peek

toEnumList :: Enum a => CUInt -> [a]
toEnumList ls =
  -- Find the places where the bits are set in ls, then convert them to b.
  let stage1 = map (ls .&.) $ map (2^) [0..(bitSize (undefined::CUInt))]
      stage2 = filter (>0) stage1
  in map (toEnum . fromIntegral) stage2
