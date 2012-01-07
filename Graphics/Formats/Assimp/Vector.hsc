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
  ) where

#include <assimp.h>
#include <typedefs.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Data.Vect.Float (Vec2(..), Vec3(..), Vec4(..))
import Foreign.Storable
import Control.Applicative

instance Storable Vec2 where
  sizeOf _ = (#size aiVector2D)
  alignment _ = (#alignment aiVector2D)
  peek p = Vec2 <$> (#peek aiVector2D, x) p
                <*> (#peek aiVector2D, y) p
  poke = undefined

instance Storable Vec3 where
  sizeOf _ = (#size aiVector3D)
  alignment _ = (#alignment aiVector3D)
  peek p = Vec3 <$> (#peek aiVector3D, x) p
                <*> (#peek aiVector3D, y) p
                <*> (#peek aiVector3D, z) p
  poke = undefined
