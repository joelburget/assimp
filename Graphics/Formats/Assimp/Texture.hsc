{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module : Graphics.Formats.Assimp.Texture
-- Copyright : (c) Joel Burget 2011
-- License BSD3
--
-- Maintainer : Joel Burget <joelburget@gmail.com>
-- Stability : experimental
-- Portability : non-portable
--
-- Corresponds to aiTexture.h

module Graphics.Formats.Assimp.Texture (
    Texel(..)
  , Texture(..)
  ) where

#include "assimp.h"
#include "aiTexture.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Foreign.Storable
import Foreign.C
import Foreign.Marshal.Array

data Texel = Texel 
  { dummy'Texel :: Int
  } deriving (Show)

instance Storable Texel where
  sizeOf _ = #size aiTexel
  alignment _ = #alignment aiTexel
  peek _ = return $ Texel 0
  poke = undefined

data Texture = Texture 
  { width         :: CUInt
  , height        :: CUInt
  , achFormatHint :: String
  , pcData        :: [Texel]
  } deriving (Show)

instance Storable Texture where
  sizeOf _ = #size aiTexture
  alignment _ = #alignment aiTexture
  peek p = do
    mWidth <- (#peek aiTexture, mWidth) p
    mHeight <- (#peek aiTexture, mHeight) p
    -- Should achFormatHint be included?
    achFormatHint <- (#peek aiTexture, achFormatHint) p >>= peekCString
    pcData' <- (#peek aiTexture, pcData) p
    pcData <- if mHeight == 0
              then peekArray (fromIntegral mWidth) pcData'
              else peekArray (fromIntegral (mWidth * mHeight)) pcData'
    return $ Texture mWidth mHeight achFormatHint pcData
  poke = undefined
