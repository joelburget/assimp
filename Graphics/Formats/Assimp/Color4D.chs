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
-- Corresponds to aiColor4D.h

module Graphics.Formats.Assimp.Color4D (
    Color3F(Color3F)
  , Color4F(Color4F)
  ) where

#include "assimp.h"
#include "typedefs.h"

newtype Color3F = Color3F Vec3
newtype Color4F = Color4F Vec4

{#pointer *aiColor3D as Color3Ptr -> Color3F#}
{#pointer *aiColor4D as Color4Ptr -> Color4F#}

instance Storable Color3F where
  sizeOf _ = #size aiColor3D
  alignment _ = #alignment aiColor3D
  peek p = Color3F <$> (Vec3 <$> (#peek aiColor3D, r) p
                             <*> (#peek aiColor3D, g) p
                             <*> (#peek aiColor3D, b) p)
  poke = undefined

instance Storable Color4F where
  sizeOf _ = #size aiColor4D
  alignment _ = #alignment aiColor4D
  peek p = Color4F <$> (Vec4 <$> (#peek aiColor4D, r) p
                             <*> (#peek aiColor4D, g) p
                             <*> (#peek aiColor4D, b) p
                             <*> (#peek aiColor4D, a) p)
  poke = undefined
