{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

-- |
-- Module      : Graphics.Formats.Assimp.Mesh
-- Copyright   : (c) Joel Burget 2011-2012
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
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

#include "mesh.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array
import Data.Bits (shiftR)
import Control.Monad (liftM, join)
import Control.Applicative ((<$>), (<*>))
import Data.Vect.Float (Vec3(..), Mat4)
import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Color4D
import Graphics.Formats.Assimp.Matrix (Mat4F, unMat4F)
import Graphics.Formats.Assimp.Material

-- | Enumerates the types of geometric primitives supported by Assimp.
data PrimitiveType 
  -- | A point primitive.
  --
  -- This is just a single vertex in the virtual world, 'aiFace' contains just
  -- one index for such a primitive.
  = PrimitiveTypePoint
  -- | A line primitive.
  --
  -- This is a line defined through a start and an end position. 'aiFace'
  -- contains exactly two indices for such a primitive.
  | PrimitiveTypeLine
  -- | A triangular primitive.
  --
  -- A triangle consists of three indices.
  | PrimitiveTypeTriangle
  -- | A higher-level polygon with more than 3 edges.
  --
  -- A triangle is a polygon, but polygon in this context means "all polygons
  -- that are not triangles". The "Triangulate"-Step is provided for your
  -- convenience, it splits all polygons in triangles (which are much easier to
  -- handle).
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

-- | A mesh represents a geometry or model with a single material.
--
-- It usually consists of a number of vertices and a series of primitives/faces
-- referencing the vertices. In addition there might be a series of bones, each
-- of them addressing a number of vertices with a certain weight. Vertex data
-- is presented in channels with each channel containing a single per-vertex
-- information such as a set of texture coords or a normal vector. If a data
-- pointer is non-null, the corresponding data stream is present.
-- 
-- A Mesh uses only a single material which is referenced by a material ID.
data Mesh = Mesh {
  -- | Bitwise combination of the members of the 'PrimitiveType' enum.
  --
  -- This specifies which types of primitives are present in the mesh. The
  -- "SortByPrimitiveType"-Step can be used to make sure the output meshes
  -- consist of one primitive type each.
    primitiveTypes  :: [PrimitiveType]
  -- | Vertex positions.
  , vertices        :: [Vec3]
  -- | Vertex normals
  --
  -- Meshes with mixed primitive types (i.e. lines and triangles) may have
  -- normals, but the normals for vertices that are only referenced by point or
  -- line primitives are undefined and set to QNaN
  --
  -- Note:
  -- Normal vectors computed by Assimp are always unit-length. However, this
  -- needn't apply for normals that have been taken directly from the model
  -- file.
  , normals         :: [Vec3]
  -- | Vertex tangents.
  --
  -- The tangent of a vertex points in the direction of the positive X texture
  -- axis. The list contains normalized vectors, NULL if not present. A mesh
  -- consisting of points and lines only may not have normal vectors. Meshes
  -- with mixed primitive types (i.e. lines and triangles) may have normals,
  -- but the normals for vertices that are only referenced by point or line
  -- primitives are undefined and set to qNaN.  See the normals member for a
  -- detailed discussion of qNaNs.
  --
  -- Note:
  -- If the mesh contains tangents, it automatically also contains bitangents
  -- (the bitangent is just the cross product of tangent and normal vectors).
  , tangents        :: [Vec3]
  -- | Vertex bitangents.
  --
  -- The bitangent of a vertex points in the direction of the positive Y
  -- texture axis. The list contains normalized vectors.
  --
  -- Note:
  -- If the mesh contains tangents, it automatically also contains bitangents.
  , bitangents      :: [Vec3]
  -- | Vertex color sets.
  , colors          :: [Color4F]
  -- | Vertex texture coords, also known as UV channels.
  , textureCoords   :: [Vec3]
  -- | Specifies the number of components for a given UV channel.
  --
  -- Up to three channels are supported (UVW, for accessing volume or cube maps). If the value is 2 for a given channel n, the component p.z of mTextureCoords[n][p] is set to 0.0f. If the value is 1 for a given channel, p.y is set to 0.0f, too.
  --
  -- Note:
  -- 4D coords are not supported
  , numUVComponents :: CUInt
  -- | The faces the mesh is constructed from.
  --
  -- Each face refers to a number of vertices by their indices. If the
  -- 'FlagsNonVerboseFormat' is NOT set each face references an
  -- unique set of vertices.
  , faces           :: [Face]
  -- | The bones of this mesh.
  --
  -- A bone consists of a name by which it can be found in the frame hierarchy
  -- and a set of vertex weights.
  , bones           :: [Bone]
  -- | The material used by this mesh.
  --
  -- A mesh does use only a single material. If an imported model uses multiple
  -- materials, the import splits up the mesh. Use this value as index into the
  -- scene's material list.
  , materialIndex   :: CUInt
  -- | Name of the mesh.
  --
  -- Meshes can be named, but this is not a requirement and leaving this field
  -- empty is totally fine. There are mainly three uses for mesh names:
  -- 
  --   * some formats name nodes and meshes independently.
  --
  --   * importers tend to split meshes up to meet the one-material-per-mesh
  --   requirement. Assigning the same (dummy) name to each of the result
  --   meshes aids the caller at recovering the original mesh partitioning.
  --
  --   * Vertex animations refer to meshes by their names.
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
    putStrLn "peeking mPrimitiveTypes"
    mPrimitiveTypes  <- liftM (toEnumList . (flip shiftR 1))
                        ((#peek aiMesh, mPrimitiveTypes) p :: IO CUInt)
    mNumVs           <- liftM fromIntegral
                        $ ((#peek aiMesh, mNumVertices) p :: IO CUInt)
    mVertices        <- (#peek aiMesh, mVertices) p      >>= peekArray  mNumVs
    mNormals         <- (#peek aiMesh, mNormals) p       >>= peekArray' mNumVs
    mTangents        <- (#peek aiMesh, mTangents) p      >>= peekArray' mNumVs
    mBitangents      <- (#peek aiMesh, mBitangents) p    >>= peekArray' mNumVs
    putStrLn "peeking mColors"
    mColors          <- (#peek aiMesh, mColors) p        >>= peekArray' mNumVs
    putStrLn "peeking mTextureCoords"
    mTextureCoords   <- (#peek aiMesh, mTextureCoords) p >>= peekArray' mNumVs
    putStrLn "peeking mNumUVComponents"
    mNumUVComponents <- (#peek aiMesh, mNumUVComponents) p
    putStrLn "peeking mNumFaces"
    mNumFaces        <- liftM fromIntegral 
                        ((#peek aiMesh, mNumFaces) p :: IO CUInt)
    print (mPrimitiveTypes, mNumVs, mVertices, mNormals, mTangents, mBitangents, mColors, mTextureCoords, mNumUVComponents)
    putStrLn "peeking mFaces"
    putStrLn $ "num faces: " ++ (show mNumFaces)
    (#peek aiMesh, mFaces) p >>= \(x::Ptr ()) -> putStrLn $ "mFaces: " ++ (show x)
    mFaces           <- (#peek aiMesh, mFaces) p >>= peekArray mNumFaces
    putStrLn $ "numBones: "  
    -- mBones           <- join $ peekArrayPtr <$> ((#peek aiMesh, mNumBones) p)
    --                                        <*> ((#peek aiMesh, mBones) p)
    let mBones = error "got me"
    putStrLn "peeking mMaterialIndex"
    mMaterialIndex   <- (#peek aiMesh, mMaterialIndex) p
    putStrLn "peeking mName"
    mName            <- liftM aiStringToString $ (#peek aiMesh, mName) p
    return $ Mesh
               mPrimitiveTypes mVertices mNormals mTangents mBitangents
               mColors mTextureCoords mNumUVComponents mFaces mBones
               mMaterialIndex mName
  poke = undefined

-- | A single bone of a mesh.
--
-- A bone has a name by which it can be found in the frame hierarchy and by
-- which it can be addressed by animations. In addition it has a number of
-- influences on vertices.
data Bone = Bone {
  -- | The name of the bone. Use 'name' to access.
    boneName      :: String
  -- | The vertices affected by this bone.
  , weights       :: [VertexWeight]
  -- | Matrix that transforms from mesh space to bone space in bind pose.
  , offsetMatrix :: Mat4
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
    mO <- unMat4F <$> (#peek aiBone, mOffsetMatrix) p
    return $ Bone mN lst mO
  poke = undefined

-- | A single face in a mesh, referring to multiple vertices.
--
-- If mNumIndices is 3, we call the face 'triangle', for mNumIndices > 3 it's
-- called 'polygon' (hey, that's just a definition!).
--
-- 'Mesh' :: 'primitiveTypes' can be queried to quickly examine which types of
-- primitive are actually present in a mesh. The SortByPrimitiveType flag
-- executes a special post-processing algorithm which splits meshes with
-- *different* primitive types mixed up (e.g. lines and triangles) in several
-- 'clean' submeshes. Furthermore there is a configuration option (
-- AI_CONFIG_PP_SBP_REMOVE) to force SortByPrimitiveType to remove specific
-- kinds of primitives from the imported scene, completely and forever. In many
-- cases you'll probably want to set this setting to
--
-- @
-- PrimitiveTypeLine|PrimitiveTypePoint
-- @
--
-- Together with the 'Triangulate' flag you can then be sure that
-- 'Face' :: 'numIndices' is always 3.
--
-- Note:
-- Take a look at the Data Structures page
-- (<http://assimp.sourceforge.net/lib_html/data.html>) for more information on
-- the layout and winding order of a face.
newtype Face = Face { 
    indices :: [CUInt] -- Holds indices defining the face
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

data VertexWeight = VertexWeight {
  -- | Index of the vertex which is influenced by the bone.
    vertexId :: CUInt
  -- | The strength of the influence in the range (0..1). The influence from
  -- all bones at one vertex amounts to 1.
  , weight   :: CFloat
  } deriving (Show)

instance Storable VertexWeight where
  sizeOf _ = #size aiVertexWeight
  alignment _ = #alignment aiVertexWeight
  peek p = VertexWeight <$> (#peek aiVertexWeight, mVertexId) p
                        <*> (#peek aiVertexWeight, mWeight) p
  poke = undefined
