{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.Formats.Assimp.Color4D
-- Copyright   : (c) Joel Burget 2011
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Corresponds to aiColor4D.h

module Graphics.Formats.Assimp.Color4D (
  -- * RGBA Color
    Color4F(Color4F)
  ) where

#include "color4.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Data.Vect.Float (Vec4(..))

-- | RGBA color structure
newtype Color4F = Color4F Vec4 deriving Show

instance Storable Color4F where
  sizeOf _ = #size aiColor4D
  alignment _ = #alignment aiColor4D
  peek p = Color4F <$> (Vec4 <$> (#peek aiColor4D, r) p
                             <*> (#peek aiColor4D, g) p
                             <*> (#peek aiColor4D, b) p
                             <*> (#peek aiColor4D, a) p)
  poke = undefined
