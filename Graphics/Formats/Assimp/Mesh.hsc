{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module : Graphics.Formats.Assimp.Mesh
-- Copyright : (c) Joel Burget 2011
-- License BSD3
--
-- Maintainer : Joel Burget <joelburget@gmail.com>
-- Stability : experimental
-- Portability : non-portable
--
-- Corresponds to aiMesh.h

module Graphics.Formats.Assimp.Mesh (
    PrimitiveType(..)
  , Mesh(..)
  , Bone(..)
  , Face(..)
  , VertexWeight(..)
  ) where

#include "assimp.h"
#include "aiMesh.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Array
import Data.Bits (shiftR)
import Control.Monad (liftM, join)
import Control.Applicative ((<$>), (<*>))
import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Vector
import Graphics.Formats.Assimp.Color4D
import Graphics.Formats.Assimp.Matrix (Mat4F)
import Graphics.Formats.Assimp.Material

data PrimitiveType = PrimitiveTypePoint
                   | PrimitiveTypeLine
                   | PrimitiveTypeTriangle
                   | PrimitiveTypePolygon
                   deriving (Show,Eq)
instance Enum PrimitiveType where
  fromEnum PrimitiveTypePoint = 1
  fromEnum PrimitiveTypeLine = 2
  fromEnum PrimitiveTypeTriangle = 4
  fromEnum PrimitiveTypePolygon = 8

  toEnum 1 = PrimitiveTypePoint
  toEnum 2 = PrimitiveTypeLine
  toEnum 4 = PrimitiveTypeTriangle
  toEnum 8 = PrimitiveTypePolygon
  toEnum unmatched = error ("PrimitiveType.toEnum: Cannot match " ++ show unmatched)

data Mesh = Mesh
  { primitiveTypes  :: [PrimitiveType]
  , vertices        :: [Vec3F]
  , normals         :: [Vec3F]
  , tangents        :: [Vec3F]
  , bitangents      :: [Vec3F]
  , colors          :: [Color4F]
  , textureCoords   :: [Vec3F]
  , numUVComponents :: CUInt
  , faces           :: [Face]
  , bones           :: [Bone]
  , materialIndex   :: CUInt
  , meshName        :: String
  } deriving (Show)

instance Name Mesh where
  name = meshName

instance Storable Mesh where
  sizeOf _ = #size aiMesh
  alignment _ = #alignment aiMesh
  peek p = do
    -- Note for some reason I had to shift the bits right by 1 but I don't
    -- think that should have been necessary.
    mPrimitiveTypes  <- liftM (toEnumList . (flip shiftR 1))
                        ((#peek aiMesh, mPrimitiveTypes) p :: IO CUInt)
    mNumVs           <- liftM fromIntegral
                        $ ((#peek aiMesh, mNumVertices) p :: IO CUInt)
    mVertices        <- (#peek aiMesh, mVertices) p      >>= peekArray  mNumVs
    mNormals         <- (#peek aiMesh, mNormals) p       >>= peekArray' mNumVs
    mTangents        <- (#peek aiMesh, mTangents) p      >>= peekArray' mNumVs
    mBitangents      <- (#peek aiMesh, mBitangents) p    >>= peekArray' mNumVs
    mColors          <- (#peek aiMesh, mColors) p        >>= peekArray' mNumVs
    mTextureCoords   <- (#peek aiMesh, mTextureCoords) p >>= peekArray' mNumVs
    mNumUVComponents <- (#peek aiMesh, mNumUVComponents) p
    mNumFaces        <- liftM fromIntegral 
                        ((#peek aiMesh, mNumFaces) p :: IO CUInt)
    mFaces           <- (#peek aiMesh, mFaces) p >>= peekArray mNumFaces
    mBones           <- join $ peekArrayPtr <$> ((#peek aiMesh, mNumBones) p)
                                            <*> ((#peek aiMesh, mBones) p)
    mMaterialIndex   <- (#peek aiMesh, mMaterialIndex) p
    mName            <- liftM aiStringToString $ (#peek aiMesh, mName) p
    return $ Mesh
               mPrimitiveTypes mVertices mNormals mTangents mBitangents
               mColors mTextureCoords mNumUVComponents mFaces mBones
               mMaterialIndex mName
  poke = undefined

data Bone = Bone
  { boneName      :: String
  , weights       :: [VertexWeight]
  , offpokeMatrix :: Mat4F
  } deriving (Show)

instance Name Bone where
  name = boneName

instance Storable Bone where
  sizeOf _ = #size aiBone
  alignment _ = #alignment aiBone
  peek p = do
    mN <- liftM aiStringToString $ (#peek aiBone, mName) p
    mNW <- (#peek aiBone, mNumWeights) p
    mW <- (#peek aiBone, mWeights) p
    lst <- peekArray mNW mW
    mO <- (#peek aiBone, mOffsetMatrix) p
    return $ Bone mN lst mO
  poke = undefined

data Face = Face
  { indices :: [CUInt] -- Holds indices defining the face
  --, debug :: String
  } deriving (Show)

instance Storable Face where
  sizeOf _ = #size aiFace
  alignment _ = #alignment aiFace
  peek p = do
    mNumIndices <- (#peek aiFace, mNumIndices) p :: IO CUInt
    mIndices <- (#peek aiFace, mIndices) p
    lst <- peekArray (fromIntegral mNumIndices) mIndices
    return $ Face lst
  poke = undefined

data VertexWeight = VertexWeight
  { vertexId :: CUInt
  , weight   :: CFloat
  } deriving (Show)

instance Storable VertexWeight where
  sizeOf _ = #size aiVertexWeight
  alignment _ = #alignment aiVertexWeight
  peek p = VertexWeight <$> (#peek aiVertexWeight, mVertexId) p
                        <*> (#peek aiVertexWeight, mWeight) p
  poke = undefined
