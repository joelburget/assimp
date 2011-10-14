{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module : Graphics.Formats.Assimp.Storable
-- Copyright : (c) Joel Burget 2011
-- License BSD3
--
-- Maintainer : Joel Burget <joelburget@gmail.com>
-- Stability : experimental
-- Portability : non-portable
--
-- Data types for (nearly) every type used in assimp.

module Graphics.Formats.Assimp.Types (
    Return(..)
--   , Origin(..)
--  , DefaultLogStream(..)
  , matKeyToTuple
  , Plane3d(..)
  , Ray(..)
  , MemoryInfo(..)
  , AiString(..)
  , position
  ) where

import C2HS

-- Remove the force32bit enums
#define SWIG

#include "assimp.h"        // Plain-C interface
#include "typedefs.h"

{#context lib="assimp"#}
{#context prefix="ai"#}

data Return = ReturnSuccess
            | ReturnFailure
            | ReturnOutOfMemory
            deriving (Show,Eq)
instance Enum Return where
  fromEnum ReturnSuccess = 0
  fromEnum ReturnFailure = (-1)
  fromEnum ReturnOutOfMemory = (-3)

  toEnum 0 = ReturnSuccess
  toEnum (-1) = ReturnFailure
  toEnum (-3) = ReturnOutOfMemory
  toEnum unmatched = error ("Return.toEnum: Cannot match " ++ show unmatched)

--{#enum aiOrigin as Origin                     {} with prefix="aiOrigin_" deriving (Show, Eq)#}
--{#enum aiDefaultLogStream as DefaultLogStream {underscoreToCase} with prefix="aiDefaultLogStream" deriving (Show, Eq)#}


fromEnum' :: TextureType -> CUInt
fromEnum' = fromInteger . toInteger . fromEnum

data Plane3d = Plane3d
  { planeA :: Float
  , planeB :: Float
  , planeC :: Float
  , planeD :: Float
  } deriving (Show)
{#pointer *aiPlane as PlanePtr -> Plane3d#}

data Ray = Ray 
  { rayPos :: Vec3
  , rayDir :: Vec3
  } deriving (Show)
{#pointer *aiRay as RayPtr -> Ray#}

data MemoryInfo = MemoryInfo 
  { memoryInfoTextures   :: CUInt
  , memoryInfoMaterials  :: CUInt
  , memoryInfoMeshes     :: CUInt
  , memoryInfoNodes      :: CUInt
  , memoryInfoAnimations :: CUInt
  , memoryInfoCameras    :: CUInt
  , memoryInfoLights     :: CUInt
  , memoryInfoTotal      :: CUInt
  } deriving (Show)
{#pointer *aiMemoryInfo as MemoryInfoPtr -> MemoryInfo#}

-- data LogStream
-- {#pointer *aiLogStream as LogStreamPtr -> LogStream#}

newtype AiString = AiString String deriving (Show)
{#pointer *aiString as StringPtr -> AiString#}


data UVTransform = UVTransform 
  { translation :: Vec2
  , scaling     :: Vec2
  , rotation    :: Float
  } deriving (Show)

class Position a where
  position :: a -> Vec3

class Name a where
  name :: a -> String
