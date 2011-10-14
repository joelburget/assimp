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

import C2HS
import Unsafe.Coerce (unsafeCoerce)

{#enum define CompileFlags {ASSIMP_CFLAGS_SHARED         as Shared
                          , ASSIMP_CFLAGS_STLPORT        as StlPort
                          , ASSIMP_CFLAGS_DEBUG          as Debug
                          , ASSIMP_CFLAGS_NOBOOST        as NoBoost
                          , ASSIMP_CFLAGS_SINGLETHREADED as SingleThreaded
                          }#}

instance Show CompileFlags where
  show Shared         = "Shared"
  show StlPort        = "StlPort"
  show Debug          = "Debug"
  show NoBoost        = "NoBoost"
  show SingleThreaded = "SingleThreaded"

{#fun unsafe aiGetCompileFlags as getCompileFlags
  {} -> `CompileFlags' convert#}
  where convert = toEnum . cIntConv

{#fun unsafe aiGetLegalString as getLegalString
  {} -> `String'#}

{#fun unsafe aiGetVersionMinor as getVersionMinor
  {} -> `CUInt' unsafeCoerce#}

{#fun unsafe aiGetVersionMajor as getVersionMajor
  {} -> `CUInt' unsafeCoerce#}

{#fun unsafe aiGetVersionRevision as getVersionRevision
  {} -> `CUInt' unsafeCoerce#}

{#fun unsafe aiIsExtensionSupported as isExtensionSupported
  {`String'} -> `Bool'#}
