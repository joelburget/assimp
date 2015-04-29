{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.Formats.Assimp.Anim
-- Copyright   : (c) Joel Burget 2011
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Corresponds to aiAnim.h

module Graphics.Formats.Assimp.Anim (
  -- * Animation types /(Not completely implemented)/
    NodeAnim(..)
  , MeshAnim(..)
  , Animation(..)
  ) where

#include "anim.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Control.Monad (liftM)
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Array (peekArray)
import Graphics.Formats.Assimp.Types

-- | Describes the animation of a single node. /Not yet implemented/
data NodeAnim = NodeAnim 
  { dummy'NodeAnim :: Int
  } deriving (Show)

-- | Describes vertex-based animations for a single mesh or a group of meshes.
-- /Not yet implemented/
data MeshAnim = MeshAnim 
  { dummy'MeshAnim :: Int
  } deriving (Show)

-- | An animation consists of keyframe data for a number of nodes.
-- For each node affected by the animation a separate series of data is given.
data Animation = Animation 
  { animationName  :: String
  , duration       :: Double
  , ticksPerSecond :: Double
  , channels       :: [NodeAnim]
  , meshChannels   :: [MeshAnim]
  } deriving (Show)

instance Name Animation where
  name = animationName

instance Storable NodeAnim where
  sizeOf _ = #size aiNodeAnim
  alignment _ = #alignment aiNodeAnim
  peek _ = return $ NodeAnim 0
  poke = undefined

instance Storable MeshAnim where
  sizeOf _ = #size aiMeshAnim
  alignment _ = #alignment aiMeshAnim
  peek _ = return $ MeshAnim 0
  poke = undefined

instance Storable Animation where
  sizeOf _ = #size aiAnimation
  alignment _ = #alignment aiAnimation
  peek p = do
    mName            <- liftM aiStringToString $ (#peek aiAnimation, mName) p
    print mName
    mDuration        <- (#peek aiAnimation, mDuration) p
    print mDuration
    mTicksPerSecond  <- (#peek aiAnimation, mTicksPerSecond) p
    print mTicksPerSecond
    mNumChannels     <- (#peek aiAnimation, mNumChannels) p :: IO CUInt
    print mNumChannels
    mChannels'       <- (#peek aiAnimation, mChannels) p
    mChannels        <- peekArray (fromIntegral mNumChannels) mChannels'
    mNumMeshChannels <- (#peek aiAnimation, mNumMeshChannels) p :: IO CUInt
    mMeshChannels'   <- (#peek aiAnimation, mMeshChannels) p
    mMeshChannels    <- peekArray (fromIntegral mNumMeshChannels) mMeshChannels'
    return $ Animation mName mDuration mTicksPerSecond mChannels mMeshChannels
  poke = undefined
