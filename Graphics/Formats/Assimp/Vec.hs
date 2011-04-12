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

#define InstanceVec2(typ, constructor)                                      \
instance Vector N2 typ where                                                \
{ data Vec N2 typ t = constructor !typ !typ deriving (Show, Eq)             \
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
#define InstanceVec3(typ, constructor)                               \
instance Vector N3 typ where                                         \
{ data Vec N3 typ t = constructor !typ !typ !typ deriving (Show, Eq) \
; (constructor x1 y1 z1) |+| (constructor x2 y2 z2) =                \
    constructor (x1+x2) (y1+y2) (z1+z2)                              \
; (constructor x1 y1 z1) |-| (constructor x2 y2 z2) =                \
    constructor (x1-x2) (y1-y2) (z1-z2)                              \
; (constructor x y z) |* n = constructor (n*x) (n*y) (n*z)           \
; (constructor x1 y1 z1) |*| (constructor x2 y2 z2) =                \
    constructor (x1*x2) (y1*y2) (z1*z2)                              \
; (constructor x1 y1 z1) `dot` (constructor x2 y2 z2) =              \
    (x1*x2) + (y1*y2) + (z1*z2)                                      \
; vmap f (constructor x y z) = constructor (f x) (f y) (f z)         \
; len2 (constructor x y z) = x*x + y*y + z*z                         \
; vIndex (constructor x _ _) 0 = x                                   \
; vIndex (constructor _ y _) 1 = y                                   \
; vIndex (constructor _ _ z) 2 = z                                   \
; vIndex _ _ = throw $ IndexOutOfBounds ""                           \
; {-# INLINE (|+|) #-}                                               \
; {-# INLINE (|-|) #-}                                               \
; {-# INLINE (|*) #-}                                                \
; {-# INLINE (|*|) #-}                                               \
; {-# INLINE dot #-}                                                 \
; {-# INLINE vmap #-}                                                \
; {-# INLINE len2 #-}                                                \
};                                                                   \
instance Vector3d N3 typ where                                       \
{ (constructor x1 y1 z1) `cross` (constructor x2 y2 z2) =            \
    constructor (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)      \
;  {-# INLINE cross #-}                                              \
}

InstanceVec3(Double, Vec3D)
InstanceVec3(Float, Vec3F)
InstanceVec3(Int, Vec3I)

#define InstanceVec4(typ, constructor)                                    \
instance Vector N4 typ where                                              \
{ data Vec N4 typ t = constructor !typ !typ !typ !typ deriving (Show, Eq) \
; (constructor x1 y1 z1 w1) |+| (constructor x2 y2 z2 w2) =               \
    constructor (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)                   \
; (constructor x1 y1 z1 w1) |-| (constructor x2 y2 z2 w2) =               \
    constructor (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)                   \
; (constructor x y z w) |* n = constructor (n*x) (n*y) (n*z) (n*w)        \
; (constructor x1 y1 z1 w1) |*| (constructor x2 y2 z2 w2) =               \
    constructor (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)                   \
; (constructor x1 y1 z1 w1) `dot` (constructor x2 y2 z2 w2) =             \
    (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)                         \
; vmap f (constructor x y z w) = constructor (f x) (f y) (f z) (f w)      \
; len2 (constructor x y z w) = x*x + y*y + z*z + w*w                      \
; vIndex (constructor x _ _ _) 0 = x                                      \
; vIndex (constructor _ y _ _) 1 = y                                      \
; vIndex (constructor _ _ z _) 2 = z                                      \
; vIndex (constructor _ _ _ w) 3 = w                                      \
; vIndex _ _ = throw $ IndexOutOfBounds ""                                \
; {-# INLINE (|+|) #-}                                                    \
; {-# INLINE (|-|) #-}                                                    \
; {-# INLINE (|*) #-}                                                     \
; {-# INLINE (|*|) #-}                                                    \
; {-# INLINE dot #-}                                                      \
; {-# INLINE vmap #-}                                                     \
; {-# INLINE len2 #-}                                                     \
}

InstanceVec4(Double, Vec4D)
InstanceVec4(Float, Vec4F)
InstanceVec4(Int, Vec4I)

#define InstanceMatrix2(typ, constructor, subconstructor)                             \
instance Matrix N2 typ where                                                          \
{ data Mat N2 typ = constructor !(Vec N2 typ ()) !(Vec N2 typ ()) deriving (Show, Eq) \
; (constructor v1 v2) ||+|| (constructor u1 u2) = constructor (v1|+|u1) (v2|+|u2)     \
; (constructor v1 v2) ||*|| m = let (constructor u1 u2) = transpose m                 \
    in constructor (subconstructor (v1 `dot` u1) (v1 `dot` u2))                       \
                   (subconstructor (v2 `dot` u1) (v2 `dot` u2))                       \
; (constructor v1 v2) ||*| v = subconstructor (v1 `dot` v) (v2 `dot` v)               \
; (constructor v1 v2) ||* s = constructor (v1|*s) (v2|*s)                             \
; mIndex (constructor v1 _) 0 = v1                                                    \
; mIndex (constructor _ v2) 1 = v2                                                    \
; mIndex _ _ = throw $ IndexOutOfBounds ""                                            \
; transpose (constructor (subconstructor x1 y1) (subconstructor x2 y2)) =             \
    constructor (subconstructor x1 x2) (subconstructor y1 y2)                         \
}

InstanceMatrix2(Double, Matrix2D, Vec2D)
InstanceMatrix2(Float, Matrix2F, Vec2F)
InstanceMatrix2(Int, Matrix2I, Vec2I)

#define InstanceMatrix3(typ, constructor, subconstructor)                     \
instance Matrix N3 typ where                                                  \
{ data Mat N3 typ = constructor                                               \
  !(Vec N3 typ ())                                                            \
  !(Vec N3 typ ())                                                            \
  !(Vec N3 typ ())                                                            \
    deriving (Show, Eq)                                                       \
; (constructor v1 v2 v3) ||+|| (constructor u1 u2 u3) =                       \
    constructor (v1|+|u1) (v2|+|u2) (v3|+|u3)                                 \
; (constructor v1 v2 v3) ||*|| m = let (constructor u1 u2 u3) = transpose m   \
    in constructor (subconstructor (v1 `dot` u1) (v1 `dot` u2) (v1 `dot` u3)) \
                   (subconstructor (v2 `dot` u1) (v2 `dot` u2) (v2 `dot` u3)) \
                   (subconstructor (v3 `dot` u1) (v3 `dot` u2) (v3 `dot` u3)) \
; (constructor v1 v2 v3) ||*| v =                                             \
    subconstructor (v1 `dot` v) (v2 `dot` v) (v3 `dot` v)                     \
; (constructor v1 v2 v3) ||* s = constructor (v1|*s) (v2|*s) (v3|*s)          \
; mIndex (constructor v0 _ _) 0 = v0                                          \
; mIndex (constructor _ v1 _) 1 = v1                                          \
; mIndex (constructor _ _ v2) 2 = v2                                          \
; mIndex _ _ = throw $ IndexOutOfBounds ""                                    \
; transpose (constructor (subconstructor x1 y1 z1)                            \
                         (subconstructor x2 y2 z2)                            \
                         (subconstructor x3 y3 z3)) =                         \
    constructor (subconstructor x1 x2 x3)                                     \
                (subconstructor y1 y2 y3)                                     \
                (subconstructor z1 z2 z3)                                     \
}

InstanceMatrix3(Double, Matrix3D, Vec3D)
InstanceMatrix3(Float, Matrix3F, Vec3F)
InstanceMatrix3(Int, Matrix3I, Vec3I)

#define InstanceMatrix4(typ, constructor, subconstructor)                                   \
instance Matrix N4 typ where                                                                \
{ data Mat N4 typ = constructor                                                             \
  !(Vec N4 typ ())                                                                          \
  !(Vec N4 typ ())                                                                          \
  !(Vec N4 typ ())                                                                          \
  !(Vec N4 typ ())                                                                          \
    deriving (Show, Eq)                                                                     \
; (constructor v1 v2 v3 v4) ||+|| (constructor u1 u2 u3 u4) =                               \
    constructor (v1|+|u1) (v2|+|u2) (v3|+|u3) (v4|+|u4)                                     \
; (constructor v1 v2 v3 v4) ||*|| m = let (constructor u1 u2 u3 u4) = transpose m           \
    in constructor (subconstructor (v1 `dot` u1) (v1 `dot` u2) (v1 `dot` u3) (v1 `dot` u4)) \
                (subconstructor (v2 `dot` u1) (v2 `dot` u2) (v2 `dot` u3) (v2 `dot` u4))    \
                (subconstructor (v3 `dot` u1) (v3 `dot` u2) (v3 `dot` u3) (v3 `dot` u4))    \
                (subconstructor (v4 `dot` u1) (v4 `dot` u2) (v4 `dot` u3) (v4 `dot` u4))    \
; (constructor v1 v2 v3 v4) ||*| v =                                                        \
    subconstructor (v1 `dot` v) (v2 `dot` v) (v3 `dot` v) (v4 `dot` v)                      \
; (constructor v1 v2 v3 v4) ||* s = constructor (v1|*s) (v2|*s) (v3|*s) (v4|*s)             \
; mIndex (constructor v0 _ _ _) 0 = v0                                                      \
; mIndex (constructor _ v1 _ _) 1 = v1                                                      \
; mIndex (constructor _ _ v2 _) 2 = v2                                                      \
; mIndex (constructor _ _ _ v3) 3 = v3                                                      \
; mIndex _ _ = throw $ IndexOutOfBounds ""                                                  \
; transpose (constructor (subconstructor x1 y1 z1 w1)                                       \
                      (subconstructor x2 y2 z2 w2)                                          \
                      (subconstructor x3 y3 z3 w3)                                          \
                      (subconstructor x4 y4 z4 w4)) =                                       \
    constructor (subconstructor x1 x2 x3 x4)                                                \
             (subconstructor y1 y2 y3 y4)                                                   \
             (subconstructor z1 z2 z3 z4)                                                   \
             (subconstructor w1 w2 w3 w4)                                                   \
}

InstanceMatrix4(Double, Matrix4D, Vec4D)
InstanceMatrix4(Float, Matrix4F, Vec4F)
InstanceMatrix4(Int, Matrix4I, Vec4I)
