{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.Formats.Assimp.Camera
-- Copyright   : (c) Joel Burget 2011
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Corresponds to aiCamera.h

module Graphics.Formats.Assimp.Camera (
  -- * The Camera type
    Camera(..)
  ) where

#include "aiCamera.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Control.Applicative ((<$>), (<*>), liftA)
import Foreign.Storable
import Data.Vect.Float (Vec3(..))
import Graphics.Formats.Assimp.Types

-- | Helper structure to describe a virtual camera.
--
-- Cameras have a representation in the node graph and can be animated. An
-- important aspect is that the camera itself is also part of the scenegraph.
-- This means, any values such as the look-at vector are not *absolute*,
-- they're relative to the coordinate system defined by the node which
-- corresponds to the camera. This allows for camera animations. For static
-- cameras parameters like the 'look-at' or 'up' vectors are usually specified
-- directly in aiCamera, but beware, they could also be encoded in the node
-- transformation. The following (pseudo)code sample shows how to do it: 
--
-- > // Get the camera matrix for a camera at a specific time
-- > // if the node hierarchy for the camera does not contain
-- > // at least one animated node this is a static computation
-- > get-camera-matrix (node sceneRoot, camera cam) : matrix
-- > {
-- >    node   cnd = find-node-for-camera(cam)
-- >    matrix cmt = identity()
-- > 
-- >    // as usual - get the absolute camera transformation for this frame
-- >    for each node nd in hierarchy from sceneRoot to cnd
-- >      matrix cur
-- >      if (is-animated(nd))
-- >         cur = eval-animation(nd)
-- >      else cur = nd->mTransformation;
-- >      cmt = mult-matrices( cmt, cur )
-- >    end for
-- > 
-- >    // now multiply with the camera's own local transform
-- >    cam = mult-matrices (cam, get-camera-matrix(cmt) )
-- > }
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
