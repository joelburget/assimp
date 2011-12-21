{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module : Graphics.Formats.Assimp.Version
-- Copyright : (c) Joel Burget 2011
-- License BSD3
--
-- Maintainer : Joel Burget <joelburget@gmail.com>
-- Stability : experimental
-- Portability : non-portable
--
-- Corresponds to aiVersion.h

module Graphics.Formats.Assimp.Version (
    CompileFlags(..)
  -- * Version information
  , getCompileFlags
  , getLegalString
  , getVersionMinor
  , getVersionMajor
  , getVersionRevision
  , isExtensionSupported
  ) where

#include "assimp.h"
#include "aiVersion.h"
#include "typedefs.h"

import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Monad (liftM)

data CompileFlags = Shared
                  | StlPort
                  | Debug
                  | NoBoost
                  | SingleThreaded
                  
instance Enum CompileFlags where
  fromEnum Shared = 1
  fromEnum StlPort = 2
  fromEnum Debug = 4
  fromEnum NoBoost = 8
  fromEnum SingleThreaded = 16

  toEnum 1 = Shared
  toEnum 2 = StlPort
  toEnum 4 = Debug
  toEnum 8 = NoBoost
  toEnum 16 = SingleThreaded
  toEnum unmatched = error ("CompileFlags.toEnum: Cannot match " ++ show unmatched)

instance Show CompileFlags where
  show Shared         = "Shared"
  show StlPort        = "StlPort"
  show Debug          = "Debug"
  show NoBoost        = "NoBoost"
  show SingleThreaded = "SingleThreaded"

getCompileFlags :: IO (CompileFlags)
getCompileFlags = liftM convert aiGetCompileFlags
  where convert = toEnum . fromIntegral

getLegalString :: IO (String)
getLegalString = aiGetLegalString >>= peekCString

isExtensionSupported :: String -> IO Bool
isExtensionSupported ext =
  liftM toBool $ withCString ext aiIsExtensionSupported

foreign import ccall unsafe "aiGetCompileFlags" aiGetCompileFlags :: IO CUInt

foreign import ccall unsafe "aiGetLegalString" 
  aiGetLegalString :: IO (Ptr CChar)

foreign import ccall unsafe "aiGetVersionMinor" getVersionMinor :: IO CUInt

foreign import ccall unsafe "aiGetVersionMajor" getVersionMajor :: IO CUInt

foreign import ccall unsafe "aiGetVersionRevision"
  getVersionRevision :: IO CUInt

foreign import ccall unsafe "aiIsExtensionSupported"
  aiIsExtensionSupported :: Ptr CChar -> IO CInt
