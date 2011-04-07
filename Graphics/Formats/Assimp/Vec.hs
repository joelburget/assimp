{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
  , (|*)
  , (*|)
  , (|*|)
  , dot
  , vmap
  , len2
  , len
  , normalize
  , cross
  , Matrix2D
  , Matrix2I
  , Matrix2F
  , Matrix3D
  , Matrix3I
  , Matrix3F
  , Matrix4D
  , Matrix4I
  , Matrix4F
  , Mat(..)
  , (||+||)
  , (||-||)
  , (||*||)
  , (|*||)
  , (||*|)
  , (||*)
  , (*||)
  , transpose
  , (!)
  , avgColor
  , toInt
  ) where

import Data.List (foldl1')
import Control.Exception (throw, ArrayException(IndexOutOfBounds))

-- Type level naturals
data N2
data N3
data N4

-- Plain vector or color, intended to be used as a phantom type
-- example: Vec3D 1 2 3 :: Vec3D Color
--          Vec3D 1 2 3 :: Vec3D ()
-- TODO: remove () in lieu of Direction and Location
data Color

class Num a => Vector n a where
  data Vec n a :: * -> *
  (|+|)   :: Vec n a t -> Vec n a t -> Vec n a t
  (|-|)   :: Vec n a t -> Vec n a t -> Vec n a t
  x |-| y = x |+| (y |* (-1))
  (|*)    :: Vec n a t -> a -> Vec n a t
  (|*)    = flip (*|)
  (*|)    :: a -> Vec n a t -> Vec n a t
  (*|)    = flip (|*)
  (|*|)   :: Vec n a t -> Vec n a t -> Vec n a t
  dot     :: Vec n a t -> Vec n a t -> a
  vmap    :: (a -> a) -> Vec n a t -> Vec n a t
  len2    :: Vec n a t -> a
  vIndex  :: Vec n a t -> Int -> a

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|
infixl 7 |*
infixl 7 *|

class Vector3d n a where
  cross :: Vec n a t -> Vec n a t -> Vec n a t

class Num a => Matrix n a where
  -- Note: I'll be defining matrices as rows of vectors instead of columns of
  -- vectors, simply because it's slightly easier to prettyprint and I don't
  -- see a good reason to do it one way or another.
  data Mat n a :: * -> *
  (||+||)   :: Mat n a t -> Mat n a t -> Mat n a t
  (||-||)   :: Mat n a t -> Mat n a t -> Mat n a t
  x ||-|| y = x ||+|| (y ||* (-1))
  (||*||)   :: Mat n a t -> Mat n a t -> Mat n a t
  (||*|)    :: Mat n a t -> Vec n a t -> Vec n a t
  m ||*| v  = v |*|| (transpose m)
  (|*||)    :: Vec n a t -> Mat n a t -> Vec n a t
  v |*|| m  = (transpose m) ||*| v
  (||*)     :: Mat n a t -> a -> Mat n a t
  (||*)     = flip (*||)
  (*||)     :: a -> Mat n a t -> Mat n a t
  (*||)     = flip (||*)
  mIndex    :: Mat n a t -> Int -> Vec n a t
  transpose :: Mat n a t -> Mat n a t

infixl 6 ||+||
infixl 6 ||-||
infixl 7 |*||
infixl 7 ||*|
infixl 7 ||*||
infixl 7 ||*
infixl 7 *||

class Indexable a b where
  (!) :: a -> Int -> b

instance Matrix n a => Indexable (Mat n a t) (Vec n a t) where
  (!) = mIndex
  {-# INLINE (!) #-}

instance Vector n a => Indexable (Vec n a t) a where
  (!) = vIndex
  {-# INLINE (!) #-}

len :: (Floating a, Vector n a) => Vec n a t -> a
len = sqrt . len2

normalize :: (Floating a, Vector n a) => Vec n a t -> Vec n a t
normalize v = v |* (1 / len v)

-- Clearly this is intended to be used with colors
avgColor :: (Vector n a, Fractional a) => [Vec n a t] -> Vec n a t
avgColor xs = (foldl1' (|+|) xs) 
  |* (1 / ((fromInteger . toInteger) (length xs)))

toInt :: (Floating a, Ord a, RealFrac a, Integral b) => a -> b
toInt x = truncate (((clamp x ** (1 / 2.2)) * 255) + 0.5)

clamp :: (Num a, Ord a) => a -> a
clamp x | x < 0     = 0
        | x > 1     = 1
        | otherwise = x

--
-- Begin uninteresting code
--

-- Don't include color type synonyms because nobody uses 2-d colors
type Vec2D = Vec N2 Double ()
type Vec2F = Vec N2 Float ()
type Vec2I = Vec N2 Int ()

type Vec3D   = Vec N3 Double ()
type Color3D = Vec N3 Double Color
type Vec3F   = Vec N3 Float ()
type Color3F = Vec N3 Float Color
type Vec3I   = Vec N3 Int ()
type Color3I = Vec N3 Int Color

type Vec4D   = Vec N4 Double ()
type Color4D = Vec N4 Double Color
type Vec4F   = Vec N4 Float ()
type Color4F = Vec N4 Float Color
type Vec4I   = Vec N4 Int ()
type Color4I = Vec N4 Int Color

type Matrix2D = Mat N2 Double ()
type Matrix2F = Mat N2 Float ()
type Matrix2I = Mat N2 Int ()

type Matrix3D = Mat N3 Double ()
type Matrix3F = Mat N3 Float ()
type Matrix3I = Mat N3 Int ()

type Matrix4D = Mat N4 Double ()
type Matrix4F = Mat N4 Float ()
type Matrix4I = Mat N4 Int ()

instance Vector N2 Double where
  data Vec N2 Double t = Vec2D !Double !Double deriving (Show, Eq)
  (Vec2D x1 y1) |+| (Vec2D x2 y2 ) = Vec2D (x1 + x2) (y1 + y2)
  (Vec2D x1 y1) |-| (Vec2D x2 y2 ) = Vec2D (x1 - x2) (y1 - y2)
  (Vec2D x y) |* n = Vec2D (n*x) (n*y)
  (Vec2D x1 y1) |*| (Vec2D x2 y2) = Vec2D (x1 * x2) (y1 * y2)
  (Vec2D x1 y1) `dot` (Vec2D x2 y2) = (x1 * x2) + (y1 * y2)
  vmap f (Vec2D x y) = Vec2D (f x) (f y)
  len2 (Vec2D x y) = x*x + y*y
  vIndex (Vec2D x y) 0 = x
  vIndex (Vec2D x y) 1 = y
  vIndex _ _ = throw $ IndexOutOfBounds ""
  {-# INLINE (|+|) #-}
  {-# INLINE (|-|) #-}
  {-# INLINE (|*) #-}
  {-# INLINE (|*|) #-}
  {-# INLINE dot #-}
  {-# INLINE vmap #-}
  {-# INLINE len2 #-}

instance Vector N2 Float where
  data Vec N2 Float t = Vec2F !Float !Float deriving (Show, Eq)
  (Vec2F x1 y1) |+| (Vec2F x2 y2 ) = Vec2F (x1 + x2) (y1 + y2)
  (Vec2F x1 y1) |-| (Vec2F x2 y2 ) = Vec2F (x1 - x2) (y1 - y2)
  (Vec2F x y) |* n = Vec2F (n*x) (n*y)
  (Vec2F x1 y1) |*| (Vec2F x2 y2) = Vec2F (x1 * x2) (y1 * y2)
  (Vec2F x1 y1) `dot` (Vec2F x2 y2) = (x1 * x2) + (y1 * y2)
  vmap f (Vec2F x y) = Vec2F (f x) (f y)
  len2 (Vec2F x y) = x*x + y*y
  vIndex (Vec2F x y) 0 = x
  vIndex (Vec2F x y) 1 = y
  vIndex _ _ = throw $ IndexOutOfBounds ""
  {-# INLINE (|+|) #-}
  {-# INLINE (|-|) #-}
  {-# INLINE (|*) #-}
  {-# INLINE (|*|) #-}
  {-# INLINE dot #-}
  {-# INLINE vmap #-}
  {-# INLINE len2 #-}

instance Vector N2 Int where
  data Vec N2 Int t = Vec2I !Int !Int deriving (Show, Eq)
  (Vec2I x1 y1) |+| (Vec2I x2 y2 ) = Vec2I (x1 + x2) (y1 + y2)
  (Vec2I x1 y1) |-| (Vec2I x2 y2 ) = Vec2I (x1 - x2) (y1 - y2)
  (Vec2I x y) |* n = Vec2I (n*x) (n*y)
  (Vec2I x1 y1) |*| (Vec2I x2 y2) = Vec2I (x1 * x2) (y1 * y2)
  (Vec2I x1 y1) `dot` (Vec2I x2 y2) = (x1 * x2) + (y1 * y2)
  vmap f (Vec2I x y) = Vec2I (f x) (f y)
  len2 (Vec2I x y) = x*x + y*y
  {-# INLINE (|+|) #-}
  {-# INLINE (|-|) #-}
  {-# INLINE (|*) #-}
  {-# INLINE (|*|) #-}
  {-# INLINE dot #-}
  {-# INLINE vmap #-}
  {-# INLINE len2 #-}

instance Indexable Vec2I Int where
  (Vec2I x y) ! 0 = x
  (Vec2I x y) ! 1 = y
  _ ! _ = throw $ IndexOutOfBounds ""
  {-# INLINE (!) #-}

instance Vector N3 Double where
  data Vec N3 Double t = Vec3D !Double !Double !Double deriving (Show, Eq)
  (Vec3D x1 y1 z1) |+| (Vec3D x2 y2 z2) = Vec3D (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3D x1 y1 z1) |-| (Vec3D x2 y2 z2) = Vec3D (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3D x y z) |* n = Vec3D (n*x) (n*y) (n*z)
  (Vec3D x1 y1 z1) |*| (Vec3D x2 y2 z2) = Vec3D (x1 * x2) (y1 * y2) (z1 * z2)
  (Vec3D x1 y1 z1) `dot` (Vec3D x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)
  vmap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)
  len2 (Vec3D x y z) = x*x + y*y + z*z
  {-# INLINE (|+|) #-}
  {-# INLINE (|-|) #-}
  {-# INLINE (|*) #-}
  {-# INLINE (|*|) #-}
  {-# INLINE dot #-}
  {-# INLINE vmap #-}
  {-# INLINE len2 #-}

instance Vector3d N3 Double where
  (Vec3D x1 y1 z1) `cross` (Vec3D x2 y2 z2) = 
    Vec3D (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)

instance Vector N3 Float where
  data Vec N3 Float t = Vec3F !Float !Float !Float deriving (Show, Eq)
  (Vec3F x1 y1 z1) |+| (Vec3F x2 y2 z2) = Vec3F (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3F x1 y1 z1) |-| (Vec3F x2 y2 z2) = Vec3F (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3F x y z) |* n = Vec3F (n*x) (n*y) (n*z)
  (Vec3F x1 y1 z1) |*| (Vec3F x2 y2 z2) = Vec3F (x1 * x2) (y1 * y2) (z1 * z2)
  (Vec3F x1 y1 z1) `dot` (Vec3F x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)
  vmap f (Vec3F x y z) = Vec3F (f x) (f y) (f z)
  len2 (Vec3F x y z) = x*x + y*y + z*z
  {-# INLINE (|+|) #-}
  {-# INLINE (|-|) #-}
  {-# INLINE (|*) #-}
  {-# INLINE (|*|) #-}
  {-# INLINE dot #-}
  {-# INLINE vmap #-}
  {-# INLINE len2 #-}

instance Vector3d N3 Float where
  (Vec3F x1 y1 z1) `cross` (Vec3F x2 y2 z2) = 
    Vec3F (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)
  {-# INLINE cross #-}

instance Vector N3 Int where
  data Vec N3 Int t = Vec3I !Int !Int !Int deriving (Show, Eq)
  (Vec3I x1 y1 z1) |+| (Vec3I x2 y2 z2) = Vec3I (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3I x1 y1 z1) |-| (Vec3I x2 y2 z2) = Vec3I (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3I x y z) |* n = Vec3I (n*x) (n*y) (n*z)
  (Vec3I x1 y1 z1) |*| (Vec3I x2 y2 z2) = Vec3I (x1 * x2) (y1 * y2) (z1 * z2)
  (Vec3I x1 y1 z1) `dot` (Vec3I x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)
  vmap f (Vec3I x y z) = Vec3I (f x) (f y) (f z)
  len2 (Vec3I x y z) = x*x + y*y + z*z
  {-# INLINE (|+|) #-}
  {-# INLINE (|-|) #-}
  {-# INLINE (|*) #-}
  {-# INLINE (|*|) #-}
  {-# INLINE dot #-}
  {-# INLINE vmap #-}
  {-# INLINE len2 #-}

instance Vector3d N3 Int where
  (Vec3I x1 y1 z1) `cross` (Vec3I x2 y2 z2) = 
    Vec3I (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)
  {-# INLINE cross #-}

instance Vector N4 Double where
  data Vec N4 Double t = Vec4D !Double !Double !Double !Double deriving (Show, Eq)
  (Vec4D x1 y1 z1 w1) |+| (Vec4D x2 y2 z2 w2) = Vec4D (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  (Vec4D x1 y1 z1 w1) |-| (Vec4D x2 y2 z2 w2) = Vec4D (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
  (Vec4D x y z w) |* n = Vec4D (n*x) (n*y) (n*z) (n*w)
  (Vec4D x1 y1 z1 w1) |*| (Vec4D x2 y2 z2 w2) = Vec4D (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)
  (Vec4D x1 y1 z1 w1) `dot` (Vec4D x2 y2 z2 w2) = (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)
  vmap f (Vec4D x y z w) = Vec4D (f x) (f y) (f z) (f w)
  len2 (Vec4D x y z w) = x*x + y*y + z*z + w*w
  {-# INLINE (|+|) #-}
  {-# INLINE (|-|) #-}
  {-# INLINE (|*) #-}
  {-# INLINE (|*|) #-}
  {-# INLINE dot #-}
  {-# INLINE vmap #-}
  {-# INLINE len2 #-}

instance Vector N4 Float where
  data Vec N4 Float t = Vec4F !Float !Float !Float !Float deriving (Show, Eq)
  (Vec4F x1 y1 z1 w1) |+| (Vec4F x2 y2 z2 w2) = Vec4F (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  (Vec4F x1 y1 z1 w1) |-| (Vec4F x2 y2 z2 w2) = Vec4F (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
  (Vec4F x y z w) |* n = Vec4F (n*x) (n*y) (n*z) (n*w)
  (Vec4F x1 y1 z1 w1) |*| (Vec4F x2 y2 z2 w2) = Vec4F (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)
  (Vec4F x1 y1 z1 w1) `dot` (Vec4F x2 y2 z2 w2) = (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)
  vmap f (Vec4F x y z w) = Vec4F (f x) (f y) (f z) (f w)
  len2 (Vec4F x y z w) = x*x + y*y + z*z + w*w
  {-# INLINE (|+|) #-}
  {-# INLINE (|-|) #-}
  {-# INLINE (|*) #-}
  {-# INLINE (|*|) #-}
  {-# INLINE dot #-}
  {-# INLINE vmap #-}
  {-# INLINE len2 #-}

instance Vector N4 Int where
  data Vec N4 Int t = Vec4I !Int !Int !Int !Int deriving (Show, Eq)
  (Vec4I x1 y1 z1 w1) |+| (Vec4I x2 y2 z2 w2) = Vec4I (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  (Vec4I x1 y1 z1 w1) |-| (Vec4I x2 y2 z2 w2) = Vec4I (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
  (Vec4I x y z w) |* n = Vec4I (n*x) (n*y) (n*z) (n*w)
  (Vec4I x1 y1 z1 w1) |*| (Vec4I x2 y2 z2 w2) = Vec4I (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)
  (Vec4I x1 y1 z1 w1) `dot` (Vec4I x2 y2 z2 w2) = (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)
  vmap f (Vec4I x y z w) = Vec4I (f x) (f y) (f z) (f w)
  len2 (Vec4I x y z w) = x*x + y*y + z*z + w*w
  {-# INLINE (|+|) #-}
  {-# INLINE (|-|) #-}
  {-# INLINE (|*) #-}
  {-# INLINE (|*|) #-}
  {-# INLINE dot #-}
  {-# INLINE vmap #-}
  {-# INLINE len2 #-}

instance Matrix N2 Double where
  data Mat N2 Double t = Matrix2D !(Vec N2 Double t) !(Vec N2 Double t) deriving (Show, Eq)
  (Matrix2D v1 v2) ||+|| (Matrix2D u1 u2) = Matrix2D (v1|+|u1) (v2|+|u2)
  (Matrix2D v1 v2) ||*|| m = let (Matrix2D u1 u2) = transpose m
    in Matrix2D (Vec2D (v1 `dot` u1) (v1 `dot` u2)) 
                (Vec2D (v2 `dot` u1) (v2 `dot` u2))
  (Matrix2D v1 v2) ||*| v = Vec2D (v1 `dot` v) (v2 `dot` v)
  (Matrix2D v1 v2) ||* s = Matrix2D (v1|*s) (v2|*s)
  mIndex (Matrix2D v1 _) 0 = v1
  mIndex (Matrix2D _ v2) 1 = v2
  mIndex _ _ = throw $ IndexOutOfBounds ""
  transpose (Matrix2D (Vec2D x1 y1) (Vec2D x2 y2)) = 
    Matrix2D (Vec2D x1 x2) (Vec2D y1 y2)

instance Matrix N2 Float where
  data Mat N2 Float t = Matrix2F !(Vec N2 Float t) !(Vec N2 Float t) deriving (Show, Eq)
  (Matrix2F v1 v2) ||+|| (Matrix2F u1 u2) = Matrix2F (v1|+|u1) (v2|+|u2)
  (Matrix2F v1 v2) ||*|| m = let (Matrix2F u1 u2) = transpose m
    in Matrix2F (Vec2F (v1 `dot` u1) (v1 `dot` u2)) 
                (Vec2F (v2 `dot` u1) (v2 `dot` u2))
  (Matrix2F v1 v2) ||*| v = Vec2F (v1 `dot` v) (v2 `dot` v)
  (Matrix2F v1 v2) ||* s = Matrix2F (v1|*s) (v2|*s)
  mIndex (Matrix2F v1 _) 0 = v1
  mIndex (Matrix2F _ v2) 1 = v2
  mIndex _ _ = throw $ IndexOutOfBounds ""
  transpose (Matrix2F (Vec2F x1 y1) (Vec2F x2 y2)) = 
    Matrix2F (Vec2F x1 x2) (Vec2F y1 y2)

instance Matrix N2 Int where
  data Mat N2 Int t = Matrix2I !(Vec N2 Int t) !(Vec N2 Int t) deriving (Show, Eq)
  (Matrix2I v1 v2) ||+|| (Matrix2I u1 u2) = Matrix2I (v1|+|u1) (v2|+|u2)
  (Matrix2I v1 v2) ||*|| m = let (Matrix2I u1 u2) = transpose m
    in Matrix2I (Vec2I (v1 `dot` u1) (v1 `dot` u2)) 
                (Vec2I (v2 `dot` u1) (v2 `dot` u2))
  (Matrix2I v1 v2) ||*| v = Vec2I (v1 `dot` v) (v2 `dot` v)
  (Matrix2I v1 v2) ||* s = Matrix2I (v1|*s) (v2|*s)
  mIndex (Matrix2I v1 _) 0 = v1
  mIndex (Matrix2I _ v2) 1 = v2
  mIndex _ _ = throw $ IndexOutOfBounds ""
  transpose (Matrix2I (Vec2I x1 y1) (Vec2I x2 y2)) = 
    Matrix2I (Vec2I x1 x2) (Vec2I y1 y2)

instance Matrix N3 Double where
  data Mat N3 Double t = Matrix3D !(Vec N3 Double t) !(Vec N3 Double t) !(Vec N3 Double t) deriving (Show, Eq)
  (Matrix3D v1 v2 v3) ||+|| (Matrix3D u1 u2 u3) = Matrix3D (v1|+|u1) (v2|+|u2) (v3|+|u3)
  (Matrix3D v1 v2 v3) ||*|| m = let (Matrix3D u1 u2 u3) = transpose m
    in Matrix3D (Vec3D (v1 `dot` u1) (v1 `dot` u2) (v1 `dot` u3)) 
                (Vec3D (v2 `dot` u1) (v2 `dot` u2) (v2 `dot` u3))
                (Vec3D (v3 `dot` u1) (v3 `dot` u2) (v3 `dot` u3))
  (Matrix3D v1 v2 v3) ||*| v = Vec3D (v1 `dot` v) (v2 `dot` v) (v3 `dot` v)
  (Matrix3D v1 v2 v3) ||* s = Matrix3D (v1|*s) (v2|*s) (v3|*s)
  mIndex (Matrix3D v1 _ _) 0 = v1
  mIndex (Matrix3D _ v2 _) 1 = v2
  mIndex (Matrix3D _ _ v3) 2 = v3
  mIndex _ _ = throw $ IndexOutOfBounds ""
  transpose (Matrix3D (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) (Vec3D x3 y3 z3)) = 
    Matrix3D (Vec3D x1 x2 x3) (Vec3D y1 y2 y3) (Vec3D z1 z2 z3)

instance Matrix N3 Float where
  data Mat N3 Float t = Matrix3F !(Vec N3 Float t) !(Vec N3 Float t) !(Vec N3 Float t) deriving (Show, Eq)
  (Matrix3F v1 v2 v3) ||+|| (Matrix3F u1 u2 u3) = Matrix3F (v1|+|u1) (v2|+|u2) (v3|+|u3)
  (Matrix3F v1 v2 v3) ||*|| m = let (Matrix3F u1 u2 u3) = transpose m
    in Matrix3F (Vec3F (v1 `dot` u1) (v1 `dot` u2) (v1 `dot` u3)) 
                (Vec3F (v2 `dot` u1) (v2 `dot` u2) (v2 `dot` u3))
                (Vec3F (v3 `dot` u1) (v3 `dot` u2) (v3 `dot` u3))
  (Matrix3F v1 v2 v3) ||*| v = Vec3F (v1 `dot` v) (v2 `dot` v) (v3 `dot` v)
  (Matrix3F v1 v2 v3) ||* s = Matrix3F (v1|*s) (v2|*s) (v3|*s)
  mIndex (Matrix3F v1 _ _) 0 = v1
  mIndex (Matrix3F _ v2 _) 1 = v2
  mIndex (Matrix3F _ _ v3) 2 = v3
  mIndex _ _ = throw $ IndexOutOfBounds ""
  transpose (Matrix3F (Vec3F x1 y1 z1) (Vec3F x2 y2 z2) (Vec3F x3 y3 z3)) = 
    Matrix3F (Vec3F x1 x2 x3) (Vec3F y1 y2 y3) (Vec3F z1 z2 z3)

instance Matrix N3 Int where
  data Mat N3 Int t = Matrix3I !(Vec N3 Int t) !(Vec N3 Int t) !(Vec N3 Int t) deriving (Show, Eq)
  (Matrix3I v1 v2 v3) ||+|| (Matrix3I u1 u2 u3) = Matrix3I (v1|+|u1) (v2|+|u2) (v3|+|u3)
  (Matrix3I v1 v2 v3) ||*|| m = let (Matrix3I u1 u2 u3) = transpose m
    in Matrix3I (Vec3I (v1 `dot` u1) (v1 `dot` u2) (v1 `dot` u3)) 
                (Vec3I (v2 `dot` u1) (v2 `dot` u2) (v2 `dot` u3))
                (Vec3I (v3 `dot` u1) (v3 `dot` u2) (v3 `dot` u3))
  (Matrix3I v1 v2 v3) ||*| v = Vec3I (v1 `dot` v) (v2 `dot` v) (v3 `dot` v)
  (Matrix3I v1 v2 v3) ||* s = Matrix3I (v1|*s) (v2|*s) (v3|*s)
  mIndex (Matrix3I v1 _ _) 0 = v1
  mIndex (Matrix3I _ v2 _) 1 = v2
  mIndex (Matrix3I _ _ v3) 2 = v3
  mIndex _ _ = throw $ IndexOutOfBounds ""
  transpose (Matrix3I (Vec3I x1 y1 z1) (Vec3I x2 y2 z2) (Vec3I x3 y3 z3)) = 
    Matrix3I (Vec3I x1 x2 x3) (Vec3I y1 y2 y3) (Vec3I z1 z2 z3)

instance Matrix N4 Double where
  data Mat N4 Double t = Matrix4D !(Vec N4 Double t) !(Vec N4 Double t) !(Vec N4 Double t) !(Vec N4 Double t) deriving (Show, Eq)
  (Matrix4D v1 v2 v3 v4) ||+|| (Matrix4D u1 u2 u3 u4) = Matrix4D (v1|+|u1) (v2|+|u2) (v3|+|u3) (v4|+|u4)
  (Matrix4D v1 v2 v3 v4) ||*|| m = let (Matrix4D u1 u2 u3 u4) = transpose m
    in Matrix4D (Vec4D (v1 `dot` u1) (v1 `dot` u2) (v1 `dot` u3) (v1 `dot` u4))
                (Vec4D (v2 `dot` u1) (v2 `dot` u2) (v2 `dot` u3) (v2 `dot` u4))
                (Vec4D (v3 `dot` u1) (v3 `dot` u2) (v3 `dot` u3) (v3 `dot` u4))
                (Vec4D (v4 `dot` u1) (v4 `dot` u2) (v4 `dot` u3) (v4 `dot` u4))
  (Matrix4D v1 v2 v3 v4) ||*| v = Vec4D (v1 `dot` v) (v2 `dot` v) (v3 `dot` v) (v4 `dot` v)
  (Matrix4D v1 v2 v3 v4) ||* s = Matrix4D (v1|*s) (v2|*s) (v3|*s) (v4|*s)
  mIndex (Matrix4D v1 _ _ _) 0 = v1
  mIndex (Matrix4D _ v2 _ _) 1 = v2
  mIndex (Matrix4D _ _ v3 _) 2 = v3
  mIndex (Matrix4D _ _ _ v4) 3 = v4
  mIndex _ _ = throw $ IndexOutOfBounds ""
  transpose (Matrix4D (Vec4D x1 y1 z1 w1) (Vec4D x2 y2 z2 w2) (Vec4D x3 y3 z3 w3) (Vec4D x4 y4 z4 w4)) = 
    Matrix4D (Vec4D x1 x2 x3 x4) (Vec4D y1 y2 y3 y4) (Vec4D z1 z2 z3 z4) (Vec4D w1 w2 w3 w4)

instance Matrix N4 Float where
  data Mat N4 Float t = Matrix4F !(Vec N4 Float t) !(Vec N4 Float t) !(Vec N4 Float t) !(Vec N4 Float t) deriving (Show, Eq)
  (Matrix4F v1 v2 v3 v4) ||+|| (Matrix4F u1 u2 u3 u4) = Matrix4F (v1|+|u1) (v2|+|u2) (v3|+|u3) (v4|+|u4)
  (Matrix4F v1 v2 v3 v4) ||*|| m = let (Matrix4F u1 u2 u3 u4) = transpose m
    in Matrix4F (Vec4F (v1 `dot` u1) (v1 `dot` u2) (v1 `dot` u3) (v1 `dot` u4))
                (Vec4F (v2 `dot` u1) (v2 `dot` u2) (v2 `dot` u3) (v2 `dot` u4))
                (Vec4F (v3 `dot` u1) (v3 `dot` u2) (v3 `dot` u3) (v3 `dot` u4))
                (Vec4F (v4 `dot` u1) (v4 `dot` u2) (v4 `dot` u3) (v4 `dot` u4))
  (Matrix4F v1 v2 v3 v4) ||*| v = Vec4F (v1 `dot` v) (v2 `dot` v) (v3 `dot` v) (v4 `dot` v)
  (Matrix4F v1 v2 v3 v4) ||* s = Matrix4F (v1|*s) (v2|*s) (v3|*s) (v4|*s)
  mIndex (Matrix4F v1 _ _ _) 0 = v1
  mIndex (Matrix4F _ v2 _ _) 1 = v2
  mIndex (Matrix4F _ _ v3 _) 2 = v3
  mIndex (Matrix4F _ _ _ v4) 3 = v4
  mIndex _ _ = throw $ IndexOutOfBounds ""
  transpose (Matrix4F (Vec4F x1 y1 z1 w1) (Vec4F x2 y2 z2 w2) (Vec4F x3 y3 z3 w3) (Vec4F x4 y4 z4 w4)) = 
    Matrix4F (Vec4F x1 x2 x3 x4) (Vec4F y1 y2 y3 y4) (Vec4F z1 z2 z3 z4) (Vec4F w1 w2 w3 w4)

instance Matrix N4 Int where
  data Mat N4 Int t = Matrix4I !(Vec N4 Int t) !(Vec N4 Int t) !(Vec N4 Int t) !(Vec N4 Int t) deriving (Show, Eq)
  (Matrix4I v1 v2 v3 v4) ||+|| (Matrix4I u1 u2 u3 u4) = Matrix4I (v1|+|u1) (v2|+|u2) (v3|+|u3) (v4|+|u4)
  (Matrix4I v1 v2 v3 v4) ||*|| m = let (Matrix4I u1 u2 u3 u4) = transpose m
    in Matrix4I (Vec4I (v1 `dot` u1) (v1 `dot` u2) (v1 `dot` u3) (v1 `dot` u4))
                (Vec4I (v2 `dot` u1) (v2 `dot` u2) (v2 `dot` u3) (v2 `dot` u4))
                (Vec4I (v3 `dot` u1) (v3 `dot` u2) (v3 `dot` u3) (v3 `dot` u4))
                (Vec4I (v4 `dot` u1) (v4 `dot` u2) (v4 `dot` u3) (v4 `dot` u4))
  (Matrix4I v1 v2 v3 v4) ||*| v = Vec4I (v1 `dot` v) (v2 `dot` v) (v3 `dot` v) (v4 `dot` v)
  (Matrix4I v1 v2 v3 v4) ||* s = Matrix4I (v1|*s) (v2|*s) (v3|*s) (v4|*s)
  mIndex (Matrix4I v1 _ _ _) 0 = v1
  mIndex (Matrix4I _ v2 _ _) 1 = v2
  mIndex (Matrix4I _ _ v3 _) 2 = v3
  mIndex (Matrix4I _ _ _ v4) 3 = v4
  mIndex _ _ = throw $ IndexOutOfBounds ""
  transpose (Matrix4I (Vec4I x1 y1 z1 w1) (Vec4I x2 y2 z2 w2) (Vec4I x3 y3 z3 w3) (Vec4I x4 y4 z4 w4)) = 
    Matrix4I (Vec4I x1 x2 x3 x4) (Vec4I y1 y2 y3 y4) (Vec4I z1 z2 z3 z4) (Vec4I w1 w2 w3 w4)
