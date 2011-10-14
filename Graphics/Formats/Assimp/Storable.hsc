{-# OPTIONS_GHC -fno-warn-orphans #-}
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
-- Storable instances for all data types, we leave @poke@ undefined because
-- it is not ever used

module Graphics.Formats.Assimp.Storable where

import C2HS
import Prelude hiding (replicate)
import Foreign.Storable
import Control.Monad (liftM, liftM2, join)
import Control.Applicative ((<$>), (<*>))

import Graphics.Formats.Assimp.Types

#include "assimp.h"        // Plain-C interface
#include "aiScene.h"       // Output data structure
#include "./typedefs.h"

#include <stddef.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- Same as peekArray but checks for a null pointer
peekArray' :: Storable a => Int -> Ptr a -> IO [a]
peekArray' n ptr = if ptr == nullPtr
                   then return []
                   else peekArray n ptr

peekArrayPtr :: (Storable a) => Int -> (Ptr (Ptr a)) -> IO [a]
peekArrayPtr n p = peekArray' (fromIntegral n) p >>= mapM peek

instance Storable Plane3d where
  sizeOf _ = #size aiPlane
  alignment _ = #alignment aiPlane
  peek p = Plane3d <$> (#peek aiPlane, a) p
                   <*> (#peek aiPlane, b) p
                   <*> (#peek aiPlane, c) p
                   <*> (#peek aiPlane, d) p
  poke = undefined

instance Storable Ray where
  sizeOf _ = #size aiRay
  alignment _ = #alignment aiRay
  peek p = do
    (Vec3F pos) <- (#peek aiRay, pos) p
    (Vec3F dir) <- (#peek aiRay, dir) p
    return $ Ray pos dir
  poke = undefined

instance Storable MemoryInfo where
  sizeOf _ = #size aiMemoryInfo
  alignment _ = #alignment aiMemoryInfo
  peek p = do
    text       <- (#peek aiMemoryInfo, textures) p
    materials  <- (#peek aiMemoryInfo, materials) p
    meshes     <- (#peek aiMemoryInfo, meshes) p
    nodes      <- (#peek aiMemoryInfo, nodes) p
    animations <- (#peek aiMemoryInfo, animations) p
    cameras    <- (#peek aiMemoryInfo, cameras) p
    lights     <- (#peek aiMemoryInfo, lights) p
    total      <- (#peek aiMemoryInfo, total) p
    return $ MemoryInfo text materials meshes nodes animations cameras lights 
                        total
  poke = undefined

instance Storable AiString where
  sizeOf _ = #size aiString
  alignment _ = #alignment aiString
  peek p = do
    start <- ((#peek aiString, data) p) :: IO (Ptr CChar)
    if start == nullPtr
      then return $ AiString ""
      else do
        len <- (#peek aiString, length) p
        -- So the string is stored as an array, we need to pass a pointer to
        -- peekCStringLen, so we can't just (#peek aiString, data) because that
        -- would give us the value of the first word of the string instead of
        -- the pointer, so we have to create a pointer.
        str <- peekCStringLen (p `plusPtr` (#offset aiString, data), len)
        return $ AiString str
  poke = undefined

aiStringToString :: AiString -> String
aiStringToString (AiString s) = s

toEnumList :: Enum a => CUInt -> [a]
toEnumList ls =
  -- Find the places where the bits are set in ls, then convert them to b.
  let stage1 = map (ls .&.) $ map (2^) [0..(bitSize (undefined::CUInt))]
      stage2 = filter (>0) stage1
  in map (toEnum . fromIntegral) stage2

