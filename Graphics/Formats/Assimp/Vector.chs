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
-- Corresponds to aiVector{2,3}D.h

module Graphics.Formats.Assimp.Vector (
    Vec2F(Vec2F)
  , Vec3F(Vec3F)
  , Vec4F(Vec4F)
  ) where

#include "assimp.h"
#include "typedefs.h"

import Data.Vect.Float (Vec2(..), Vec3(..), Vec4(..))
import Foreign.Storable
import Control.Applicative

newtype Vec2F = Vec2F Vec2
newtype Vec3F = Vec3F Vec3
newtype Vec4F = Vec4F Vec4

-- {#pointer *aiVector2D as Vec2Ptr -> Vec2#}
-- {#pointer *aiVector3D as Vec3Ptr -> Vec3#}
type Vec2Ptr = Ptr Vec2
type Vec3Ptr = Ptr Vec3

instance Storable Vec2F where
  sizeOf _ = (#size aiVector2D)
  alignment _ = (#alignment aiVector2D)
  peek p = Vec2F <$> (Vec2 <$> (#peek aiVector2D, x) p
                           <*> (#peek aiVector2D, y) p)
  poke = undefined

instance Storable Vec3F where
  sizeOf _ = (#size aiVector3D)
  alignment _ = (#alignment aiVector3D)
  peek p = Vec3F <$> (Vec3 <$> (#peek aiVector3D, x) p
                           <*> (#peek aiVector3D, y) p
                           <*> (#peek aiVector3D, z) p)
  poke = undefined
