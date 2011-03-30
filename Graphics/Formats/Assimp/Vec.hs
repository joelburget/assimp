{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}

module Graphics.Formats.Assimp.Vec (
    Vec2D
  , Vec2I
  , Vec2F
  , Vec3D
  , Vec3I
  , Vec3F
  , Color3D
  , Color3I
  , Color3F
  , Vec4D
  , Vec4I
  , Vec4F
  , Color4D
  , Color4I
  , Color4F
  , Vec(..)
  , (|+|)
  , (|-|)
  , (|*|)
  , vmult
  , dot
  , vmap
  , len2
  , len
  , normalize
  , avgColor
  ) where

import Data.List (foldl1')

-- Type level naturals
data N2
data N3
data N4

-- Plain vector or color, intended to be used as a phantom type
-- example: Vec3D 1 2 3 :: Vec3D Color
data Color

class Vector n a where
  data Vec n a :: * -> *
  (|+|) :: Vec n a t -> Vec n a t -> Vec n a t
  (|-|) :: Vec n a t -> Vec n a t -> Vec n a t
  (|*|) :: Vec n a t -> a -> Vec n a t
  vmult :: Vec n a t -> Vec n a t -> Vec n a t
  dot   :: Vec n a t -> Vec n a t -> a
  vmap  :: (a -> a) -> Vec n a t -> Vec n a t
  len2  :: Vec n a t -> a

len :: (Floating a, Vector n a) => Vec n a t -> a
len = sqrt . len2

normalize :: (Floating a, Vector n a) => Vec n a t -> Vec n a t
normalize v = v |*| (1 / len v)

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Clearly this is intended to be used with colors
avgColor :: (Vector n a, Fractional a) => [Vec n a t] -> Vec n a t
avgColor xs = (foldl1' (|+|) xs) 
  |*| (1 / ((fromInteger . toInteger) (length xs)))

--
-- Begin uninteresting code
--

-- Don't include color type synonyms because nobody uses 2-d colors
type Vec2D = Vec N2 Double
type Vec2F = Vec N2 Float
type Vec2I = Vec N2 Int

type Vec3D = Vec N3 Double
type Color3D = Vec3D
type Vec3F = Vec N3 Float
type Color3F = Vec3F
type Vec3I = Vec N3 Int
type Color3I = Vec3I

type Vec4D = Vec N4 Double
type Color4D = Vec4D
type Vec4F = Vec N4 Float
type Color4F = Vec4F
type Vec4I = Vec N4 Int
type Color4I = Vec4I

instance Vector N2 Double where
  data Vec N2 Double t = Vec2D !Double !Double deriving (Show, Eq)
  (Vec2D x1 y1) |+| (Vec2D x2 y2 ) = Vec2D (x1 + x2) (y1 + y2)
  (Vec2D x1 y1) |-| (Vec2D x2 y2 ) = Vec2D (x1 - x2) (y1 - y2)
  (Vec2D x y) |*| n = Vec2D (n*x) (n*y)
  (Vec2D x1 y1) `vmult` (Vec2D x2 y2) = Vec2D (x1 * x2) (y1 * y2)
  (Vec2D x1 y1) `dot` (Vec2D x2 y2) = (x1 * x2) + (y1 * y2)
  vmap f (Vec2D x y) = Vec2D (f x) (f y)
  len2 (Vec2D x y) = x*x + y*y

instance Vector N2 Float where
  data Vec N2 Float t = Vec2F !Float !Float deriving (Show, Eq)
  (Vec2F x1 y1) |+| (Vec2F x2 y2 ) = Vec2F (x1 + x2) (y1 + y2)
  (Vec2F x1 y1) |-| (Vec2F x2 y2 ) = Vec2F (x1 - x2) (y1 - y2)
  (Vec2F x y) |*| n = Vec2F (n*x) (n*y)
  (Vec2F x1 y1) `vmult` (Vec2F x2 y2) = Vec2F (x1 * x2) (y1 * y2)
  (Vec2F x1 y1) `dot` (Vec2F x2 y2) = (x1 * x2) + (y1 * y2)
  vmap f (Vec2F x y) = Vec2F (f x) (f y)
  len2 (Vec2F x y) = x*x + y*y

instance Vector N2 Int where
  data Vec N2 Int t = Vec2I !Int !Int deriving (Show, Eq)
  (Vec2I x1 y1) |+| (Vec2I x2 y2 ) = Vec2I (x1 + x2) (y1 + y2)
  (Vec2I x1 y1) |-| (Vec2I x2 y2 ) = Vec2I (x1 - x2) (y1 - y2)
  (Vec2I x y) |*| n = Vec2I (n*x) (n*y)
  (Vec2I x1 y1) `vmult` (Vec2I x2 y2) = Vec2I (x1 * x2) (y1 * y2)
  (Vec2I x1 y1) `dot` (Vec2I x2 y2) = (x1 * x2) + (y1 * y2)
  vmap f (Vec2I x y) = Vec2I (f x) (f y)
  len2 (Vec2I x y) = x*x + y*y

instance Vector N3 Double where
  data Vec N3 Double t = Vec3D !Double !Double !Double deriving (Show, Eq)
  (Vec3D x1 y1 z1) |+| (Vec3D x2 y2 z2) = Vec3D (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3D x1 y1 z1) |-| (Vec3D x2 y2 z2) = Vec3D (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3D x y z) |*| n = Vec3D (n*x) (n*y) (n*z)
  (Vec3D x1 y1 z1) `vmult` (Vec3D x2 y2 z2) = Vec3D (x1 * x2) (y1 * y2) (z1 * z2)
  (Vec3D x1 y1 z1) `dot` (Vec3D x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)
  vmap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)
  len2 (Vec3D x y z) = x*x + y*y + z*z

instance Vector N3 Float where
  data Vec N3 Float t = Vec3F !Float !Float !Float deriving (Show, Eq)
  (Vec3F x1 y1 z1) |+| (Vec3F x2 y2 z2) = Vec3F (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3F x1 y1 z1) |-| (Vec3F x2 y2 z2) = Vec3F (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3F x y z) |*| n = Vec3F (n*x) (n*y) (n*z)
  (Vec3F x1 y1 z1) `vmult` (Vec3F x2 y2 z2) = Vec3F (x1 * x2) (y1 * y2) (z1 * z2)
  (Vec3F x1 y1 z1) `dot` (Vec3F x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)
  vmap f (Vec3F x y z) = Vec3F (f x) (f y) (f z)
  len2 (Vec3F x y z) = x*x + y*y + z*z

instance Vector N3 Int where
  data Vec N3 Int t = Vec3I !Int !Int !Int deriving (Show, Eq)
  (Vec3I x1 y1 z1) |+| (Vec3I x2 y2 z2) = Vec3I (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3I x1 y1 z1) |-| (Vec3I x2 y2 z2) = Vec3I (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3I x y z) |*| n = Vec3I (n*x) (n*y) (n*z)
  (Vec3I x1 y1 z1) `vmult` (Vec3I x2 y2 z2) = Vec3I (x1 * x2) (y1 * y2) (z1 * z2)
  (Vec3I x1 y1 z1) `dot` (Vec3I x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)
  vmap f (Vec3I x y z) = Vec3I (f x) (f y) (f z)
  len2 (Vec3I x y z) = x*x + y*y + z*z

instance Vector N4 Double where
  data Vec N4 Double t = Vec4D !Double !Double !Double !Double deriving (Show, Eq)
  (Vec4D x1 y1 z1 w1) |+| (Vec4D x2 y2 z2 w2) = Vec4D (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  (Vec4D x1 y1 z1 w1) |-| (Vec4D x2 y2 z2 w2) = Vec4D (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
  (Vec4D x y z w) |*| n = Vec4D (n*x) (n*y) (n*z) (n*w)
  (Vec4D x1 y1 z1 w1) `vmult` (Vec4D x2 y2 z2 w2) = Vec4D (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)
  (Vec4D x1 y1 z1 w1) `dot` (Vec4D x2 y2 z2 w2) = (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)
  vmap f (Vec4D x y z w) = Vec4D (f x) (f y) (f z) (f w)
  len2 (Vec4D x y z w) = x*x + y*y + z*z + w*w

instance Vector N4 Float where
  data Vec N4 Float t = Vec4F !Float !Float !Float !Float deriving (Show, Eq)
  (Vec4F x1 y1 z1 w1) |+| (Vec4F x2 y2 z2 w2) = Vec4F (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  (Vec4F x1 y1 z1 w1) |-| (Vec4F x2 y2 z2 w2) = Vec4F (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
  (Vec4F x y z w) |*| n = Vec4F (n*x) (n*y) (n*z) (n*w)
  (Vec4F x1 y1 z1 w1) `vmult` (Vec4F x2 y2 z2 w2) = Vec4F (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)
  (Vec4F x1 y1 z1 w1) `dot` (Vec4F x2 y2 z2 w2) = (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)
  vmap f (Vec4F x y z w) = Vec4F (f x) (f y) (f z) (f w)
  len2 (Vec4F x y z w) = x*x + y*y + z*z + w*w

instance Vector N4 Int where
  data Vec N4 Int t = Vec4I !Int !Int !Int !Int deriving (Show, Eq)
  (Vec4I x1 y1 z1 w1) |+| (Vec4I x2 y2 z2 w2) = Vec4I (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  (Vec4I x1 y1 z1 w1) |-| (Vec4I x2 y2 z2 w2) = Vec4I (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
  (Vec4I x y z w) |*| n = Vec4I (n*x) (n*y) (n*z) (n*w)
  (Vec4I x1 y1 z1 w1) `vmult` (Vec4I x2 y2 z2 w2) = Vec4I (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)
  (Vec4I x1 y1 z1 w1) `dot` (Vec4I x2 y2 z2 w2) = (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)
  vmap f (Vec4I x y z w) = Vec4I (f x) (f y) (f z) (f w)
  len2 (Vec4I x y z w) = x*x + y*y + z*z + w*w
