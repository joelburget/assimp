{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.Formats.Assimp.Version
-- Copyright   : (c) Joel Burget 2011-2012
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
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

#include "defs.h"
#include "version.h"
#include "typedefs.h"

import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Monad (liftM)
import Data.Bits (Bits, testBit, bitSize)

-- | How assimp was compiled
data CompileFlags 
  = Shared         -- ^ As a shared object (DLL)
  | StlPort        -- ^ Compiled against STLport
  | Debug          -- ^ Debug build
  | NoBoost        -- ^ Compiled with ASSIMP_BUILD_BOOST_WORKAROUND defined
  | SingleThreaded -- ^ Compiled with ASSIMP_BUILD_SINGLETHREADED defined
                  
instance Enum CompileFlags where
  fromEnum Shared         = 1
  fromEnum StlPort        = 2
  fromEnum Debug          = 4
  fromEnum NoBoost        = 8
  fromEnum SingleThreaded = 16

  toEnum 1         = Shared
  toEnum 2         = StlPort
  toEnum 4         = Debug
  toEnum 8         = NoBoost
  toEnum 16        = SingleThreaded
  toEnum unmatched = error $
    "CompileFlags.toEnum: Cannot match " ++ show unmatched

instance Show CompileFlags where
  show Shared         = "Shared"
  show StlPort        = "StlPort"
  show Debug          = "Debug"
  show NoBoost        = "NoBoost"
  show SingleThreaded = "SingleThreaded"

-- | Returns assimp's compile flags
getCompileFlags :: IO [CompileFlags]
getCompileFlags = liftM bitsToList aiGetCompileFlags

setBits :: (Bits a) => a -> [Bool]
setBits i = map (testBit i) [0..(bitSize i - 1)]

bitsToList :: (Bits a, Enum b) => a -> [b]
bitsToList i = map (toEnum . fst)    -- get enums from powers of two
  $ filter (id . snd)                -- (power of two, True - it's in i)
  $ zip (map (2^) [0..]) (setBits i) -- (power of two, if it's in i)

-- | Returns a string with legal copyright and licensing information about
-- Assimp. The string may include multiple lines.
getLegalString :: IO (String)
getLegalString = aiGetLegalString >>= peekCString

-- | Returns whether a given file extension is supported by ASSIMP
isExtensionSupported :: String -- ^ Extension, for example ".3ds", ".md3" (must include dot)
                     -> IO Bool
isExtensionSupported ext =
  liftM toBool $ withCString ext aiIsExtensionSupported

foreign import ccall unsafe "aiGetCompileFlags" aiGetCompileFlags :: IO CUInt

foreign import ccall unsafe "aiGetLegalString" 
  aiGetLegalString :: IO (Ptr CChar)

-- | Returns the current minor version number of Assimp.
foreign import ccall unsafe "aiGetVersionMinor" getVersionMinor :: IO CUInt

-- | Returns the current major version number of Assimp.
foreign import ccall unsafe "aiGetVersionMajor" getVersionMajor :: IO CUInt

-- | Returns the repository revision of the Assimp runtime.
foreign import ccall unsafe "aiGetVersionRevision"
  getVersionRevision :: IO CUInt

foreign import ccall unsafe "aiIsExtensionSupported"
  aiIsExtensionSupported :: Ptr CChar -> IO CInt
