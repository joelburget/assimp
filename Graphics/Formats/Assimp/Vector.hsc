{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module : Graphics.Formats.Assimp.Vector
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

#include <assimp.h>
#include <typedefs.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Data.Vect.Float (Vec2(..), Vec3(..), Vec4(..))
import Foreign.Storable
import Control.Applicative

newtype Vec2F = Vec2F Vec2 deriving Show
newtype Vec3F = Vec3F Vec3 deriving Show
newtype Vec4F = Vec4F Vec4 deriving Show

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
