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
-- Corresponds to aiCamera.h

module Graphics.Formats.Assimp.Camera (
    Camera(..)
  ) where

#include "assimp.h"
#include "typedefs.h"

data Camera = Camera 
  { cameraName     :: String
  , cameraPosition :: Vec3
  , up             :: Vec3
  , lookAt         :: Vec3
  , horizontalFOV  :: Float
  , clipPlaneNear  :: Float
  , clipPlaneFar   :: Float
  , aspect         :: Float
  } deriving (Show)
{#pointer *aiCamera as CameraPtr -> Camera#}

instance Position Camera where
  position = cameraPosition

instance Name Camera where
  name = cameraName

instance Storable Camera where
  sizeOf _ = #size aiCamera
  alignment _ = #alignment aiCamera
  peek p = do
    mName <- liftM aiStringToString $ (#peek aiCamera, mName) p
    (Vec3F mPosition) <- (#peek aiCamera, mPosition) p
    (Vec3F mUp) <- (#peek aiCamera, mUp) p
    (Vec3F mLookAt) <- (#peek aiCamera, mLookAt) p
    mHorizontalFOV <- (#peek aiCamera, mHorizontalFOV) p
    mClipPlaneNear <- (#peek aiCamera, mClipPlaneNear) p
    mClipPlaneFar <- (#peek aiCamera, mClipPlaneFar) p
    mAspect <- (#peek aiCamera, mAspect) p
    return $ Camera mName mPosition mUp mLookAt mHorizontalFOV mClipPlaneNear 
                    mClipPlaneFar mAspect
  poke = undefined
