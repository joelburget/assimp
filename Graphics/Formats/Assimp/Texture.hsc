{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Graphics.Formats.Assimp.Texture
-- Copyright   : (c) Joel Burget 2011-2012
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Corresponds to aiTexture.h

module Graphics.Formats.Assimp.Texture (
    Texel(..)
  , Texture(..)
  ) where

#include "texture.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Control.Applicative ((<$>), (<*>))

import Foreign.Storable
import Foreign.C
import Foreign.Marshal.Array

-- | Helper structure to represent a texel in a ARGB8888 format
--
-- Used by 'aiTexture'.
data Texel = Texel
  { texelB :: !CUChar
  , texelG :: !CUChar
  , texelR :: !CUChar
  , texelA :: !CUChar
  } deriving (Show)

instance Storable Texel where
  sizeOf _ = #size aiTexel
  alignment _ = #alignment aiTexel
  peek p = Texel
    <$> (#peek aiTexel, b) p
    <*> (#peek aiTexel, g) p
    <*> (#peek aiTexel, r) p
    <*> (#peek aiTexel, a) p
  poke = undefined

-- | Helper structure to describe an embedded texture
-- 
-- Normally textures are contained in external files but some file formats
-- embed them directly in the model file. There are two types of embedded
-- textures: 
--
-- 1. Uncompressed textures. The color data is given in an uncompressed format. 
--
-- 2. Compressed textures stored in a file format like png or jpg. The raw file
-- bytes are given so the application must utilize an image decoder (e.g.
-- DevIL) to get access to the actual color data.
data Texture = Texture { -- TODO(joel) - don't be a jerk, make this nicer
  -- | Width of the texture, in pixels
  --
  -- If 'height' is zero the texture is compressed in a format like JPEG. In
  -- this case 'width' specifies the size of the memory area pcData is pointing
  -- to, in bytes.
    width         :: CUInt
  -- | Height of the texture, in pixels
  --
  -- If this value is zero, pcData points to an compressed texture
  -- in any format (e.g. JPEG).
  , height        :: CUInt
  -- | A hint from the loader to make it easier for applications to determine
  -- the type of embedded compressed textures.
  --
  -- If 'height' != 0 this member is undefined. Otherwise it is set set to
  -- '\\0\\0\\0\\0' if the loader has no additional information about the
  -- texture file format used OR the file extension of the format without a
  -- trailing dot. If there are multiple file extensions for a format, the
  -- shortest extension is chosen (JPEG maps to 'jpg', not to 'jpeg'). E.g.
  -- 'dds\\0', 'pcx\\0', 'jpg\\0'. All characters are lower-case. The fourth
  -- character will always be '\\0'.
  , achFormatHint :: String
  -- | Data of the texture.
  --
  -- Points to an array of 'width' * 'height' aiTexel's.  The format of the
  -- texture data is always ARGB8888 to make the implementation for user of the
  -- library as easy as possible. If 'height' = 0 this is a pointer to a memory
  -- buffer of size 'width' containing the compressed texture data. Good luck,
  -- have fun!
  , pcData        :: [Texel]
  } deriving (Show)

instance Storable Texture where
  sizeOf _ = #size aiTexture
  alignment _ = #alignment aiTexture
  peek p = do
    mWidth <- (#peek aiTexture, mWidth) p
    mHeight <- (#peek aiTexture, mHeight) p
    -- Should achFormatHint be included?
    _achFormatHint <- (#peek aiTexture, achFormatHint) p >>= peekCString
    pcData' <- (#peek aiTexture, pcData) p
    _pcData <- if mHeight == 0
      then peekArray (fromIntegral mWidth) pcData'
      else peekArray (fromIntegral (mWidth * mHeight)) pcData'
    return $ Texture mWidth mHeight _achFormatHint _pcData
  poke = undefined
