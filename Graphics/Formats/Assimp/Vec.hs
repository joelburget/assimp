{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

{- |
   Module      : Graphics.Formats.Assimp.Vec
   Copyright   : (c) Joel Burget 2011
   License     : BSD3
  
   Maintainer  : joelburget@gmail.com
   Stability   : Unstable
   Portability : GHC

   Fast 2-4 dimensional linear algebra for use in Assimp and graphics
   applications.
-}

module Graphics.Formats.Assimp.Vec (
    Color
  , Direction
  , Location
  , Vec2D
  , Vec2I
  , Vec2F
  , Vec3D
  , Vec3I
  , Vec3F
  , Vec4D
  , Vec4I
  , Vec4F
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
data Direction
data Location

class Num a => Vector n a where
  data Vec n a :: * -> *
  -- | Add vectors
  (|+|)   :: Vec n a t1 -> Vec n a t2 -> Vec n a t3
  -- | Subtract vectors
  (|-|)   :: Vec n a t1 -> Vec n a t2 -> Vec n a t3
  x |-| y = x |+| (y |* (-1))
  -- | Multiply a vector on the left and a scalar on the right
  (|*)    :: Vec n a t1 -> a -> Vec n a t2
  (|*)    = flip (*|)
  -- | Multiply a scalar on the left and a vector on the right
  (*|)    :: a -> Vec n a t1 -> Vec n a t2
  (*|)    = flip (|*)
  -- | Multiply vectors componentwise
  (|*|)   :: Vec n a t1 -> Vec n a t2 -> Vec n a t3
  -- | Dot product
  dot     :: Vec n a t1 -> Vec n a t2 -> a
  -- | Apply a function to each element
  vmap    :: (a -> a) -> Vec n a t1 -> Vec n a t2
  -- | The squared length of the vector
  len2    :: Vec n a t -> a
  -- | Get an element, 0-indexed
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
  data Mat n a :: *
  -- | Add matrices
  (||+||)   :: Mat n a -> Mat n a -> Mat n a 
  -- | Subtract matrices
  (||-||)   :: Mat n a -> Mat n a -> Mat n a 
  x ||-|| y = x ||+|| (y ||* (-1))
  -- | Multiply matrices
  (||*||)   :: Mat n a -> Mat n a -> Mat n a 
  -- | Multiply a matrix on the left and a vector on the right
  (||*|)    :: Mat n a -> Vec n a t -> Vec n a t
  m ||*| v  = v |*|| (transpose m)
  -- | Multiply a vector on the left and a matrix on the right
  (|*||)    :: Vec n a t -> Mat n a -> Vec n a t
  v |*|| m  = (transpose m) ||*| v
  -- | Multiply a vector on the left and a scalar on the right
  (||*)     :: Mat n a -> a -> Mat n a 
  (||*)     = flip (*||)
  -- | Multiply a scalar on the left and a matrix on the right
  (*||)     :: a -> Mat n a -> Mat n a 
  (*||)     = flip (||*)
  -- | Get a row from a matrix, 0-indexed
  mIndex    :: Mat n a -> Int -> Vec n a ()
  -- | Transpose a matrix
  transpose :: Mat n a -> Mat n a 

infixl 6 ||+||
infixl 6 ||-||
infixl 7 |*||
infixl 7 ||*|
infixl 7 ||*||
infixl 7 ||*
infixl 7 *||

class Indexable a b where
  (!) :: a -> Int -> b

instance Matrix n a => Indexable (Mat n a) (Vec n a ()) where
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
avgColor :: (Vector n a, Fractional a) => [Vec n a Color] -> Vec n a Color
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

-- All of the Vec type synonyms have kind * -> *
-- Example use: color :: Vec3F Color
type Vec2D = Vec N2 Double
type Vec2F = Vec N2 Float
type Vec2I = Vec N2 Int

type Vec3D = Vec N3 Double
type Vec3F = Vec N3 Float
type Vec3I = Vec N3 Int

type Vec4D = Vec N4 Double
type Vec4F = Vec N4 Float
type Vec4I = Vec N4 Int

type Matrix2D = Mat N2 Double
type Matrix2F = Mat N2 Float
type Matrix2I = Mat N2 Int

type Matrix3D = Mat N3 Double
type Matrix3F = Mat N3 Float
type Matrix3I = Mat N3 Int

type Matrix4D = Mat N4 Double
type Matrix4F = Mat N4 Float
type Matrix4I = Mat N4 Int

#define InstanceVec2(type, constructor)                                     \
instance Vector N2 type where                                               \
{ data Vec N2 type t = constructor !type !type deriving (Show, Eq)          \
; (constructor x1 y1) |+| (constructor x2 y2) = constructor (x1+x2) (y1+y2) \
; (constructor x1 y1) |-| (constructor x2 y2) = constructor (x1-x2) (y1-y2) \
; (constructor x y) |* n = constructor (n*x) (n*y)                          \
; (constructor x1 y1) |*| (constructor x2 y2) = constructor (x1*x2) (y1*y2) \
; (constructor x1 y1) `dot` (constructor x2 y2) = (x1*x2) + (y1*y2)         \
; vmap f (constructor x y) = constructor (f x) (f y)                        \
; len2 (constructor x y) = x*x + y*y                                        \
; vIndex (constructor x _) 0 = x                                            \
; vIndex (constructor _ y) 1 = y                                            \
; vIndex _ _ = throw $ IndexOutOfBounds ""                                  \
; {-# INLINE (|+|) #-}                                                      \
; {-# INLINE (|-|) #-}                                                      \
; {-# INLINE (|*) #-}                                                       \
; {-# INLINE (|*|) #-}                                                      \
; {-# INLINE dot #-}                                                        \
; {-# INLINE vmap #-}                                                       \
; {-# INLINE len2 #-}                                                       \
}

InstanceVec2(Double, Vec2D)
InstanceVec2(Float, Vec2F)
InstanceVec2(Int, Vec2I)

-- Note we also define cross here
#define InstanceVec3(type, constructor)                                  \
instance Vector N3 type where                                            \
{ data Vec N3 type t = constructor !type !type !type deriving (Show, Eq) \
; (constructor x1 y1 z1) |+| (constructor x2 y2 z2) =                    \
    constructor (x1+x2) (y1+y2) (z1+z2)                                  \
; (constructor x1 y1 z1) |-| (constructor x2 y2 z2) =                    \
    constructor (x1-x2) (y1-y2) (z1-z2)                                  \
; (constructor x y z) |* n = constructor (n*x) (n*y) (n*z)               \
; (constructor x1 y1 z1) |*| (constructor x2 y2 z2) =                    \
    constructor (x1*x2) (y1*y2) (z1*z2)                                  \
; (constructor x1 y1 z1) `dot` (constructor x2 y2 z2) =                  \
    (x1*x2) + (y1*y2) + (z1*z2)                                          \
; vmap f (constructor x y z) = constructor (f x) (f y) (f z)             \
; len2 (constructor x y z) = x*x + y*y + z*z                             \
; vIndex (constructor x _ _) 0 = x                                       \
; vIndex (constructor _ y _) 1 = y                                       \
; vIndex (constructor _ _ z) 2 = z                                       \
; vIndex _ _ = throw $ IndexOutOfBounds ""                               \
; {-# INLINE (|+|) #-}                                                   \
; {-# INLINE (|-|) #-}                                                   \
; {-# INLINE (|*) #-}                                                    \
; {-# INLINE (|*|) #-}                                                   \
; {-# INLINE dot #-}                                                     \
; {-# INLINE vmap #-}                                                    \
; {-# INLINE len2 #-}                                                    \
};                                                                       \
instance Vector3d N3 type where                                          \
{ (constructor x1 y1 z1) `cross` (constructor x2 y2 z2) =                \
    constructor (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)          \
;  {-# INLINE cross #-}                                                  \
}

InstanceVec3(Double, Vec3D)
InstanceVec3(Float, Vec3F)
InstanceVec3(Int, Vec3I)

#define InstanceVec4(type, constructor)                                        \
instance Vector N4 type where                                                  \
{ data Vec N4 type t = constructor !type !type !type !type deriving (Show, Eq) \
; (constructor x1 y1 z1 w1) |+| (constructor x2 y2 z2 w2) =                    \
    constructor (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)                        \
; (constructor x1 y1 z1 w1) |-| (constructor x2 y2 z2 w2) =                    \
    constructor (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)                        \
; (constructor x y z w) |* n = constructor (n*x) (n*y) (n*z) (n*w)             \
; (constructor x1 y1 z1 w1) |*| (constructor x2 y2 z2 w2) =                    \
    constructor (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)                        \
; (constructor x1 y1 z1 w1) `dot` (constructor x2 y2 z2 w2) =                  \
    (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)                              \
; vmap f (constructor x y z w) = constructor (f x) (f y) (f z) (f w)           \
; len2 (constructor x y z w) = x*x + y*y + z*z + w*w                           \
; vIndex (constructor x _ _ _) 0 = x                                           \
; vIndex (constructor _ y _ _) 1 = y                                           \
; vIndex (constructor _ _ z _) 2 = z                                           \
; vIndex (constructor _ _ _ w) 3 = w                                           \
; vIndex _ _ = throw $ IndexOutOfBounds ""                                     \
; {-# INLINE (|+|) #-}                                                         \
; {-# INLINE (|-|) #-}                                                         \
; {-# INLINE (|*) #-}                                                          \
; {-# INLINE (|*|) #-}                                                         \
; {-# INLINE dot #-}                                                           \
; {-# INLINE vmap #-}                                                          \
; {-# INLINE len2 #-}                                                          \
}

InstanceVec4(Double, Vec4D)
InstanceVec4(Float, Vec4F)
InstanceVec4(Int, Vec4I)

instance Matrix N2 Double where
  data Mat N2 Double = Matrix2D !(Vec N2 Double ()) !(Vec N2 Double ()) deriving (Show, Eq)
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
  data Mat N2 Float = Matrix2F !(Vec N2 Float ()) !(Vec N2 Float ()) deriving (Show, Eq)
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
  data Mat N2 Int = Matrix2I !(Vec N2 Int ()) !(Vec N2 Int ()) deriving (Show, Eq)
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
  data Mat N3 Double = Matrix3D !(Vec N3 Double ()) !(Vec N3 Double ()) !(Vec N3 Double ()) deriving (Show, Eq)
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
  data Mat N3 Float = Matrix3F !(Vec N3 Float ()) !(Vec N3 Float ()) !(Vec N3 Float ()) deriving (Show, Eq)
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
  data Mat N3 Int = Matrix3I !(Vec N3 Int ()) !(Vec N3 Int ()) !(Vec N3 Int ()) deriving (Show, Eq)
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
  data Mat N4 Double = Matrix4D !(Vec N4 Double ()) !(Vec N4 Double ()) !(Vec N4 Double ()) !(Vec N4 Double ()) deriving (Show, Eq)
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
  data Mat N4 Float = Matrix4F !(Vec N4 Float ()) !(Vec N4 Float ()) !(Vec N4 Float ()) !(Vec N4 Float ()) deriving (Show, Eq)
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
  data Mat N4 Int = Matrix4I !(Vec N4 Int ()) !(Vec N4 Int ()) !(Vec N4 Int ()) !(Vec N4 Int ()) deriving (Show, Eq)
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
