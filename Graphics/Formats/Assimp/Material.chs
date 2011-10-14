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
-- Corresponds to aiMaterial.h

module Graphics.Formats.Assimp.Material (
    ShadingMode(..)       -- ?
  , BlendMode(..)
  , TextureFlags(..)
  , TextureMapMode(..)
  , TextureMapping(..)
  , TextureOp(..)
  , TextureType(..)
  , PropertyTypeInfo(..)
  , MatKey(..)
  , MaterialProperty(..)
  , Material(..)
  ) where

#include "assimp.h"
#include "aiMaterial.h"
#include "typedefs.h"

{#enum aiShadingMode as ShadingMode           {} with prefix="aiShadingMode_" deriving (Show, Eq)#}
{#enum aiBlendMode as BlendMode               {} with prefix="aiBlendMode_" deriving (Show, Eq)#}
{#enum aiTextureFlags as TextureFlags         {} with prefix="aiTextureFlags_" deriving (Show, Eq)#}
{#enum aiTextureMapMode as TextureMapMode     {} with prefix="aiTextureMapMode_" deriving (Show, Eq)#}
{#enum aiTextureMapping as TextureMapping     {underscoreToCase} with prefix="aiTextureMapping_" deriving (Show, Eq)#}
{#enum aiTextureOp as TextureOp               {} with prefix="aiTextureOp_" deriving (Show, Eq)#}
{#enum aiTextureType as TextureType           {underscoreToCase} with prefix="aiTextureType_" deriving (Show, Eq)#}
{#enum aiPropertyTypeInfo as PropertyTypeInfo {underscoreToCase} deriving (Show, Eq)#}

data MatKey = KeyName
            | KeyTwoSided
            | KeyShadingModel
            | KeyEnableWireframe
            | KeyBlendFunc
            | KeyOpacity
            | KeyBumpScaling
            | KeyShininess
            | KeyReflectivity
            | KeyShininessStrength
            | KeyRefraction
            | KeyColorDiffuse
            | KeyColorAmbient
            | KeyColorSpecular
            | KeyColorEmissive
            | KeyColorTransparent
            | KeyColorReflective
            | KeyGlobalBackgroundImage
            | KeyTexture TextureType CUInt
            | KeyUvWSrc TextureType CUInt
            | KeyTexOp TextureType CUInt
            | KeyMapping TextureType CUInt
            | KeyTexBlend TextureType CUInt
            | KeyMappingModeU TextureType CUInt
            | KeyMappingModeV TextureType CUInt
            | KeyTexMapAxis TextureType CUInt
            | KeyUvTransform TextureType CUInt
            | KeyTexFlags TextureType CUInt

matKeyToTuple :: MatKey -> (String, CUInt, CUInt)
matKeyToTuple KeyName = ("?mat.name", 0, 0)
matKeyToTuple KeyTwoSided = ("$mat.twosided", 0, 0)
matKeyToTuple KeyShadingModel = ("$mat.shadingm", 0, 0)
matKeyToTuple KeyEnableWireframe = ("$mat.wireframe", 0, 0)
matKeyToTuple KeyBlendFunc = ("$mat.blend", 0, 0)
matKeyToTuple KeyOpacity = ("$mat.opacity", 0, 0)
matKeyToTuple KeyBumpScaling = ("$mat.bumpscaling", 0, 0)
matKeyToTuple KeyShininess = ("$mat.shininess", 0, 0)
matKeyToTuple KeyReflectivity = ("$mat.reflectivity", 0, 0)
matKeyToTuple KeyShininessStrength = ("$mat.shinpercent", 0, 0)
matKeyToTuple KeyRefraction = ("$mat.refracti", 0, 0)
matKeyToTuple KeyColorDiffuse = ("$clr.diffuse", 0, 0)
matKeyToTuple KeyColorAmbient = ("$clr.ambient", 0, 0)
matKeyToTuple KeyColorSpecular = ("$clr.specular", 0, 0)
matKeyToTuple KeyColorEmissive = ("$clr.emissive", 0, 0)
matKeyToTuple KeyColorTransparent = ("$clr.transparent", 0, 0)
matKeyToTuple KeyColorReflective = ("$clr.reflective", 0, 0)
matKeyToTuple KeyGlobalBackgroundImage = ("?bg.global", 0, 0)
matKeyToTuple (KeyTexture tType i) = ("$tex.file", fromEnum' tType, i)
matKeyToTuple (KeyUvWSrc tType i) = ("$tex.uvwsrc", fromEnum' tType, i)
matKeyToTuple (KeyTexOp tType i) = ("$tex.op", fromEnum' tType, i)
matKeyToTuple (KeyMapping tType i) = ("$tex.mapping", fromEnum' tType, i)
matKeyToTuple (KeyTexBlend tType i) = ("$tex.blend", fromEnum' tType, i)
matKeyToTuple (KeyMappingModeU tType i) = ("$tex.mapmodeu", fromEnum' tType, i)
matKeyToTuple (KeyMappingModeV tType i) = ("$tex.mapmodev", fromEnum' tType, i)
matKeyToTuple (KeyTexMapAxis tType i) = ("$tex.mapaxis", fromEnum' tType, i)
matKeyToTuple (KeyUvTransform tType i) = ("$tex.uvtrafo", fromEnum' tType, i)
matKeyToTuple (KeyTexFlags tType i) = ("$tex.flags", fromEnum' tType, i)

data MaterialProperty = MaterialProperty 
  { key      :: String
  , semantic :: TextureType
  , index    :: CUInt
  , mData    :: String
  } deriving (Show)
{#pointer *aiMaterialProperty as MaterialPropertyPtr -> MaterialProperty#}

data Material = Material 
  { properties :: [MaterialProperty]
  } deriving (Show)
{#pointer *aiMaterial as MaterialPtr -> Material#}

instance Storable MaterialProperty where
  sizeOf _ = #size aiMaterialProperty
  alignment _ = #alignment aiMaterialProperty
  peek p = do
    mKey <- liftM aiStringToString $ (#peek aiMaterialProperty, mKey) p
    mSemantic <- liftM (cToEnum :: CUInt -> TextureType) $ 
                   (#peek aiMaterialProperty, mSemantic) p
    mIndex <- (#peek aiMaterialProperty, mIndex) p
    -- mType <- liftM toEnum $ (#peek aiMaterialProperty, mType) p
    mData <- (#peek aiMaterialProperty, mData) p >>= peekCString
    -- return $ MaterialProperty mKey mSemantic mIndex mType mData
    return $ MaterialProperty mKey mSemantic mIndex mData
  poke = undefined

instance Storable Material where
  sizeOf _ = #size aiMaterial
  alignment _ = #alignment aiMaterial
  peek p = do
    mProperties <- join $ liftM2 peekArrayPtr
      (liftM fromIntegral ((#peek aiMaterial, mNumProperties) p :: IO CUInt))
      ((#peek aiMaterial, mProperties) p)
    return $ Material mProperties
  poke = undefined
