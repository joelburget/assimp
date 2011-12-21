{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module : Graphics.Formats.Assimp.Camera
-- Copyright : (c) Joel Burget 2011
-- License BSD3
--
-- Maintainer : Joel Burget <joelburget@gmail.com>
-- Stability : experimental
-- Portability : non-portable
--
-- Corresponds to aiCamera.h

module Graphics.Formats.Assimp.Camera (
    Camera(..)
  ) where

#include "aiCamera.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Control.Applicative ((<$>), (<*>), liftA)
import Foreign.Storable
import Graphics.Formats.Assimp.Vector
import Graphics.Formats.Assimp.Types

data Camera = Camera 
  { cameraName     :: String
  , cameraPosition :: Vec3F
  , up             :: Vec3F
  , lookAt         :: Vec3F
  , horizontalFOV  :: Float
  , clipPlaneNear  :: Float
  , clipPlaneFar   :: Float
  , aspect         :: Float
  } deriving (Show)

instance Position Camera where
  position = cameraPosition

instance Name Camera where
  name = cameraName

instance Storable Camera where
  sizeOf _ = #size aiCamera
  alignment _ = #alignment aiCamera
  peek p = Camera
    <$> liftA aiStringToString ((#peek aiCamera, mName) p)
    <*> (#peek aiCamera, mPosition) p
    <*> (#peek aiCamera, mUp) p
    <*> (#peek aiCamera, mLookAt) p
    <*> (#peek aiCamera, mHorizontalFOV) p
    <*> (#peek aiCamera, mClipPlaneNear) p
    <*> (#peek aiCamera, mClipPlaneFar) p
    <*> (#peek aiCamera, mAspect) p
  poke = undefined
