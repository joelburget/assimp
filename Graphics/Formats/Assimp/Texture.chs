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
-- Corresponds to aiTexture.h

module Graphics.Formats.Assimp.Texture (
    Texel(..)
  , Texture(..)
  ) where

#include "assimp.h"
#include "typedefs.h"

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
{#pointer *aiTexture as TexturePtr -> Texture#}

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
