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
-- Corresponds to aiQuaternion.h

module Graphics.Formats.Assimp.Quaternion (
    Quaternion(..)
  ) where

#include "assimp.h"
#include "typedefs.h"

data Quaternion = Quaternion 
  { quaternionW :: Float
  , quaternionX :: Float
  , quaternionY :: Float
  , quaternionZ :: Float
  } deriving (Show)
{#pointer *aiQuaternion as QuaternionPtr -> Quaternion#}

instance Storable Quaternion where
  sizeOf _ = #size aiQuaternion
  alignment _ = #alignment aiQuaternion
  peek p = Quaternion <$> (#peek aiQuaternion, w) p
                      <*> (#peek aiQuaternion, w) p
                      <*> (#peek aiQuaternion, w) p
                      <*> (#peek aiQuaternion, w) p
  poke = undefined
