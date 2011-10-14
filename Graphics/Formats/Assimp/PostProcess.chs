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
-- Corresponds to aiPostProcess.h

module Graphics.Formats.Assimp.PostProcess (
    PostProcessSteps(..)
  ) where

#include "aiPostProcess.h" // Post processing flags

{#enum aiPostProcessSteps as PostProcessSteps {} with prefix="aiProcess_" deriving (Show, Eq)#}
