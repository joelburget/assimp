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
                      | GenNormals
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
                      | FlipUVs
                      | FlipWindingOrder
                      deriving (Show, Eq)

instance Enum PostProcessSteps where
  fromEnum CalcTangentSpace         = #const aiProcess_CalcTangentSpace
  fromEnum JoinIdenticalVertices    = #const aiProcess_JoinIdenticalVertices
  fromEnum MakeLeftHanded           = #const aiProcess_MakeLeftHanded
  fromEnum Triangulate              = #const aiProcess_Triangulate
  fromEnum RemoveComponent          = #const aiProcess_RemoveComponent
  fromEnum GenNormals               = #const aiProcess_GenNormals
  fromEnum GenSmoothNormals         = #const aiProcess_GenSmoothNormals
  fromEnum SplitLargeMeshes         = #const aiProcess_SplitLargeMeshes
  fromEnum PreTransformVertices     = #const aiProcess_PreTransformVertices
  fromEnum LimitBoneWeights         = #const aiProcess_LimitBoneWeights
  fromEnum ValidateDataStructure    = #const aiProcess_ValidateDataStructure
  fromEnum ImproveCacheLocality     = #const aiProcess_ImproveCacheLocality
  fromEnum RemoveRedundantMaterials = #const aiProcess_RemoveRedundantMaterials
  fromEnum FixInfacingNormals       = #const aiProcess_FixInfacingNormals
  fromEnum SortByPType              = #const aiProcess_SortByPType
  fromEnum FindDegenerates          = #const aiProcess_FindDegenerates
  fromEnum FindInvalidData          = #const aiProcess_FindInvalidData
  fromEnum GenUVCoords              = #const aiProcess_GenUVCoords
  fromEnum TransformUVCoords        = #const aiProcess_TransformUVCoords
  fromEnum FindInstances            = #const aiProcess_FindInstances
  fromEnum OptimizeMeshes           = #const aiProcess_OptimizeMeshes
  fromEnum OptimizeGraph            = #const aiProcess_OptimizeGraph
  fromEnum FlipUVs                  = #const aiProcess_FlipUVs
  fromEnum FlipWindingOrder         = #const aiProcess_FlipWindingOrder

  toEnum (#const aiProcess_CalcTangentSpace)         = CalcTangentSpace
  toEnum (#const aiProcess_JoinIdenticalVertices)    = JoinIdenticalVertices
  toEnum (#const aiProcess_MakeLeftHanded)           = MakeLeftHanded
  toEnum (#const aiProcess_Triangulate)              = Triangulate
  toEnum (#const aiProcess_RemoveComponent)          = RemoveComponent
  toEnum (#const aiProcess_GenNormals)               = GenNormals
  toEnum (#const aiProcess_GenSmoothNormals)         = GenSmoothNormals
  toEnum (#const aiProcess_SplitLargeMeshes)         = SplitLargeMeshes
  toEnum (#const aiProcess_PreTransformVertices)     = PreTransformVertices
  toEnum (#const aiProcess_LimitBoneWeights)         = LimitBoneWeights
  toEnum (#const aiProcess_ValidateDataStructure)    = ValidateDataStructure
  toEnum (#const aiProcess_ImproveCacheLocality)     = ImproveCacheLocality
  toEnum (#const aiProcess_RemoveRedundantMaterials) = RemoveRedundantMaterials
  toEnum (#const aiProcess_FixInfacingNormals)       = FixInfacingNormals
  toEnum (#const aiProcess_SortByPType)              = SortByPType
  toEnum (#const aiProcess_FindDegenerates)          = FindDegenerates
  toEnum (#const aiProcess_FindInvalidData)          = FindInvalidData
  toEnum (#const aiProcess_GenUVCoords)              = GenUVCoords
  toEnum (#const aiProcess_TransformUVCoords)        = TransformUVCoords
  toEnum (#const aiProcess_FindInstances)            = FindInstances
  toEnum (#const aiProcess_OptimizeMeshes)           = OptimizeMeshes
  toEnum (#const aiProcess_OptimizeGraph)            = OptimizeGraph
  toEnum (#const aiProcess_FlipUVs)                  = FlipUVs
  toEnum (#const aiProcess_FlipWindingOrder)         = FlipWindingOrder
  toEnum unmatched = error $ "PostProcessSteps.toEnum: Cannot match " ++ show unmatched
