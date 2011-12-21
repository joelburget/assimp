{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module : Graphics.Formats.Assimp.PostProcess
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

data PostProcessSteps = CalcTangentSpace
                      | JoinIdenticalVertices
                      | MakeLeftHanded
                      | Triangulate
                      | RemoveComponent
                      | ProcessGenNormals
                      | GenSmoothNormals
                      | SplitLargeMeshes
                      | PreTransformVertices
                      | LimitBoneWeights
                      | ValidateDataStructure
                      | ImproveCacheLocality
                      | RemoveRedundantMaterials
                      | FixInfacingNormals
                      | SortByPType
                      | FindDegenerates
                      | FindInvalidData
                      | GenUVCoords
                      | TransformUVCoords
                      | FindInstances
                      | OptimizeMeshes
                      | OptimizeGraph
                      | ProcessFlipUVs
                      | FlipWindingOrder
                      deriving (Show, Eq)

instance Enum PostProcessSteps where
  fromEnum CalcTangentSpace         = 0x1
  fromEnum JoinIdenticalVertices    = 0x2
  fromEnum MakeLeftHanded           = 0x4
  fromEnum Triangulate              = 0x8
  fromEnum RemoveComponent          = 0x10
  fromEnum ProcessGenNormals        = 0x20
  fromEnum GenSmoothNormals         = 0x40
  fromEnum SplitLargeMeshes         = 0x80
  fromEnum PreTransformVertices     = 0x100
  fromEnum LimitBoneWeights         = 0x200
  fromEnum ValidateDataStructure    = 0x400
  fromEnum ImproveCacheLocality     = 0x800
  fromEnum RemoveRedundantMaterials = 0x1000
  fromEnum FixInfacingNormals       = 0x2000
  fromEnum SortByPType              = 0x8000
  fromEnum FindDegenerates          = 0x10000
  fromEnum FindInvalidData          = 0x20000
  fromEnum GenUVCoords              = 0x40000
  fromEnum TransformUVCoords        = 0x80000
  fromEnum FindInstances            = 0x100000
  fromEnum OptimizeMeshes           = 0x200000
  fromEnum OptimizeGraph            = 0x400000
  fromEnum ProcessFlipUVs           = 0x800000
  fromEnum FlipWindingOrder         = 0x1000000

  toEnum 0x1       = CalcTangentSpace
  toEnum 0x2       = JoinIdenticalVertices
  toEnum 0x4       = MakeLeftHanded
  toEnum 0x8       = Triangulate
  toEnum 0x10      = RemoveComponent
  toEnum 0x20      = ProcessGenNormals
  toEnum 0x40      = GenSmoothNormals
  toEnum 0x80      = SplitLargeMeshes
  toEnum 0x100     = PreTransformVertices
  toEnum 0x200     = LimitBoneWeights
  toEnum 0x400     = ValidateDataStructure
  toEnum 0x800     = ImproveCacheLocality
  toEnum 0x1000    = RemoveRedundantMaterials
  toEnum 0x2000    = FixInfacingNormals
  toEnum 0x8000    = SortByPType
  toEnum 0x10000   = FindDegenerates
  toEnum 0x20000   = FindInvalidData
  toEnum 0x40000   = GenUVCoords
  toEnum 0x80000   = TransformUVCoords
  toEnum 0x100000  = FindInstances
  toEnum 0x200000  = OptimizeMeshes
  toEnum 0x400000  = OptimizeGraph
  toEnum 0x800000  = ProcessFlipUVs
  toEnum 0x1000000 = FlipWindingOrder
  toEnum unmatched = error $ "PostProcessSteps.toEnum: Cannot match " ++ show unmatched
