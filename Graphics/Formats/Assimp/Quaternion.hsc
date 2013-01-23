{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.Formats.Assimp.Quaternion
-- Copyright   : (c) Joel Burget 2011-2012
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Corresponds to aiQuaternion.h

module Graphics.Formats.Assimp.Quaternion (
    Quaternion(..)
  ) where

#include "quaternion.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Foreign.Storable
import Control.Applicative ((<$>), (<*>))

data Quaternion = Quaternion 
  { quaternionW :: Float
  , quaternionX :: Float
  , quaternionY :: Float
  , quaternionZ :: Float
  } deriving (Show)

instance Storable Quaternion where
  sizeOf _ = #size aiQuaternion
  alignment _ = #alignment aiQuaternion
  peek p = Quaternion <$> (#peek aiQuaternion, w) p
                      <*> (#peek aiQuaternion, x) p
                      <*> (#peek aiQuaternion, y) p
                      <*> (#peek aiQuaternion, z) p
  poke = undefined
