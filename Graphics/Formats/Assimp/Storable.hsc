{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Formats.Assimp.Storable where

import Control.Monad
import Control.Applicative ((<$>), (<*>))

import Graphics.Formats.Assimp.Types

#include "../../assimp/include/assimp.h"        // Plain-C interface
#include "../../assimp/include/aiScene.h"       // Output data structure
#include "../../assimp/include/aiPostProcess.h" // Post processing flags
#include "./typedefs.h"

instance Storable AiPlane where
  sizeOf _ = (#size aiPlane)
  alignment _ = 4
  peek p = do
    a <- (#peek aiPlane, a) p
    b <- (#peek aiPlane, b) p
    c <- (#peek aiPlane, c) p
    d <- (#peek aiPlane, d) p
    return $ AiPlane a b c d
  poke p (AiPlane a b c d) = do
    (#poke aiPlane, a) p a
    (#poke aiPlane, b) p b
    (#poke aiPlane, c) p c
    (#poke aiPlane, d) p d

instance Storable AiRay where
  sizeOf _ = (#size aiRay)
  alignment _ = 4
  peek p = do
    pos <- (#peek aiRay, pos) p
    dir <- (#peek aiRay, dir) p
    return $ AiRay pos dir
  poke p (AiRay pos dir) = do
    (#poke aiRay, pos) p pos
    (#poke aiRay, dir) p dir

instance Storable AiColor3D where
  sizeOf _ = (#size aiColor3D)
  alignment _ = 4
  peek p = do
    r <- (#peek aiColor3D, r) p
    g <- (#peek aiColor3D, g) p
    b <- (#peek aiColor3D, b) p
    return $ AiColor3D r g b
  poke p (AiColor3D r g b) = do
    (#poke aicolor3D, r) p r
    (#poke aicolor3D, g) p g
    (#poke aicolor3D, b) p b

instance Storable AiColor4D where
  sizeOf _ = (#size aiColor4D)
  alignment _ = 4
  peek p = do
    r <- (#peek aiColor4D, r) p
    g <- (#peek aiColor4D, g) p
    b <- (#peek aiColor4D, b) p
    a <- (#peek aiColor4D, a) p
    return $ AiColor4D r g b a
  poke p (AiColor4D r g b a) = do
    (#poke aiColor4D, r) p r
    (#poke aiColor4D, g) p g
    (#poke aiColor4D, b) p b
    (#poke aiColor4D, a) p a

instance Storable AiMemoryInfo where
  sizeOf _ = (#size aiMemoryInfo)
  alignment _ = 4
  peek p = do
    text <- (#peek aiMemoryInfo, textures) p
    materials <- (#peek aiMemoryInfo, materials) p
    meshes <- (#peek aiMemoryInfo, meshes) p
    nodes <- (#peek aiMemoryInfo, nodes) p
    animations <- (#peek aiMemoryInfo, animations) p
    cameras <- (#peek aiMemoryInfo, cameras) p
    lights <- (#peek aiMemoryInfo, lights) p
    total <- (#peek aiMemoryInfo, total) p
    return $ AiMemoryInfo text materials meshes nodes animation cameras lights total
  poke p (AiMemoryInfo te ma me no an ca li to) = do
    (#poke aiMemoryInfo, text) p te
    (#poke aiMemoryInfo, materials) p ma
    (#poke aiMemoryInfo, meshes) p me
    (#poke aiMemoryInfo, nodes) p no
    (#poke aiMemoryInfo, animations) p an
    (#poke aiMemoryInfo, cameras) p ca
    (#poke aiMemoryInfo, lights) p li
    (#poke aiMemoryInfo, total) p to

aiStringToString = data'AiString
stringToAiString x = AiString (length x) x
instance Storable AiString where
  sizeOf _ = (#size aiString)
  alignment _ = 4 -- http://web.archiveorange.com/archive/v/V6skIGuT4JclKSXDM7Xj
  peek p = liftM2 AiString
    (liftM cIntConv ((#peek aiString, length) p))
    ((#peek aiString, data) p >>= peekCString)
  poke p (AiString len dat) = do
    (#poke aiString, length) p (cIntConv $ len)
    str <- newCString $ dat
    (#poke aiString, data) p str

instance Storable AiMatrix4x4 where
  sizeOf _ = (#size aiMatrix4x4)
  alignment _ = 4
  peek = undefined
  poke = undefined

instance Storable AiNode where
  sizeOf _ = (#size aiNode)
  alignment _ = 4
  peek p = do
    mName <- liftM aiStringToString $ (#peek aiNode, mName) p
    mTransformation <- (#peek aiNode, mTransformation) p
    mParent <- (#peek aiNode, mParent) p
    mNumChildren <- (#peek aiNode, mNumChildren) p
    mChildren <- peekArray mNumChildren ((#peek aiNode, mChildren) p) -- TODO
    mMeshes <- peekArray ((#peek aiNode, mNumMeshes) p) ((#peek aiNode, mMeshes))
    return $ AiNode mName mTransformation mParent mChildren mMeshes
  poke p (AiNode name trans par chil mes) = do
    (#poke aiNode, mName) $ stringToAiString $ mName'AiNode p
    (#poke aiNode, mTransformation) $ mTransformation'AiNode p
    (#poke aiNode, mParent) $ mParent'AiNode p 
    (#poke aiNode, mChildren) $ mChildren'AiNode p
    (#poke aiNode, mMeshes) $ mMeshes'AiNode p

instance Storable AiFace where
  sizeOf _ = (#size aiFace)
  alignment _ = 4
  peek p = do
    mNumIndices <- liftM cIntConv $ (#peek aiFace, mNumIndices) p
    mIndices <- (#peek aiFace, mIndices) p
    lst <- {-(liftM . liftM) cIntConv $-} peekArray mNumIndices mIndices
    return $ AiFace lst
  poke p (AiFace mIndices) = do
    (#poke aiFace, mNumIndices) p $ cIntConv $ length mIndices
    newArray mIndices >>= ((#poke aiFace, mIndices) p)

instance Storable AiVertexWeight where
  sizeOf _ = (#size aiVertexWeight)
  alignment _ = 4
  peek p = do
    mV <- (#peek aiVertexWeight, mVertexId) p
    mW <- (#peek aiVertexWeight, mWeight) p
    return $ AiVertexWeight mV mW
  poke p (AiVertexWeight mV mW) = do
    (#poke aiVertexWeight, mVertexId) p mV
    (#poke aiVertexWeight, mWeight) p mW

instance Storable AiBone where
  sizeOf _ = (#size aiBone)
  alignment _ = 4
  peek p = do
    mN <- liftM cIntConv $ (#peek aiBone, mName) p
    mNW <- (#peek aiBone, mNumWeights) p
    mW <- (#peek aiBone, mWeights) p
    lst <- peekArray mNW mW
    mO <- (#peek aiBone, mOffsetMatrix) p >>= peek
    return $ AiBone mN lst mO
  poke p (AiBone mN mW mO) = do
    (#poke aiBone, mName) p mN
    (#poke aiBone, mNumWeights) p $ cIntConv $ length mW
    newArray mW >>= ((#poke aiBone, mWeights) p)
    (#poke aiBone, mOffsetMatrix) p mO

instance Storable AiScene where
  sizeOf _ = (#size aiScene)
  alignment _ = 4
  peek = undefined
  --peek p = do
  --  let mFlags = 
  poke p x = undefined--do
  --  (#poke aiString, length) p (cIntConv $ length'AiString x)
  --  str <- newCString $ data'AiString x
  --  (#poke aiString, data) p str
