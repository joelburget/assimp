{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.Formats.Assimp.Matrix
-- Copyright   : (c) Joel Burget 2011-2012
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Corresponds to aiMatrix{3x3,4x4}.h

module Graphics.Formats.Assimp.Matrix (
    Mat3F(Mat3F)
  , unMat3F
  , Mat4F(Mat4F)
  , unMat4F
  ) where

#include "matrix3x3.h"
#include "matrix4x4.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Foreign.Storable
import Data.Vect.Float (Mat3(..), Mat4(..), Vec3(..), Vec4(..))

-- | Represents a row-major 3x3 matrix.
--
-- There's much confusion about matrix layouts (colum vs. row order). This is
-- *always* a row-major matrix. Even with the aiProcess_ConvertToLeftHanded
-- flag.
newtype Mat3F = Mat3F {unMat3F :: Mat3} deriving Show

-- | Represents a row-major 4x4 matrix, use this for homogeneous coordinates.
--
-- There's much confusion about matrix layouts (colum vs. row order). This is
-- *always* a row-major matrix. Even with the aiProcess_ConvertToLeftHanded
-- flag.
newtype Mat4F = Mat4F {unMat4F :: Mat4} deriving Show

instance Storable Mat3F where
  sizeOf _ = #size aiMatrix3x3
  alignment _ = #alignment aiMatrix3x3
  peek p = do
    a1 <- (#peek aiMatrix3x3, a1) p
    a2 <- (#peek aiMatrix3x3, a2) p
    a3 <- (#peek aiMatrix3x3, a3) p
    b1 <- (#peek aiMatrix3x3, b1) p
    b2 <- (#peek aiMatrix3x3, b2) p
    b3 <- (#peek aiMatrix3x3, b3) p
    c1 <- (#peek aiMatrix3x3, c1) p
    c2 <- (#peek aiMatrix3x3, c2) p
    c3 <- (#peek aiMatrix3x3, c3) p
    return $ Mat3F $ Mat3
      (Vec3 a1 a2 a3)
      (Vec3 b1 b2 b3)
      (Vec3 c1 c2 c3)
  poke = undefined

instance Storable Mat4F where
  sizeOf _ = #size aiMatrix4x4
  alignment _ = #alignment aiMatrix4x4
  peek p = do
    a1 <- (#peek aiMatrix4x4, a1) p
    a2 <- (#peek aiMatrix4x4, a2) p
    a3 <- (#peek aiMatrix4x4, a3) p
    a4 <- (#peek aiMatrix4x4, a4) p
    b1 <- (#peek aiMatrix4x4, b1) p
    b2 <- (#peek aiMatrix4x4, b2) p
    b3 <- (#peek aiMatrix4x4, b3) p
    b4 <- (#peek aiMatrix4x4, b4) p
    c1 <- (#peek aiMatrix4x4, c1) p
    c2 <- (#peek aiMatrix4x4, c2) p
    c3 <- (#peek aiMatrix4x4, c3) p
    c4 <- (#peek aiMatrix4x4, c4) p
    d1 <- (#peek aiMatrix4x4, d1) p
    d2 <- (#peek aiMatrix4x4, d2) p
    d3 <- (#peek aiMatrix4x4, d3) p
    d4 <- (#peek aiMatrix4x4, d4) p
    return $ Mat4F $ Mat4
      (Vec4 a1 a2 a3 a4)
      (Vec4 b1 b2 b3 b4)
      (Vec4 c1 c2 c3 c4)
      (Vec4 d1 d2 d3 d4)
  poke = undefined
