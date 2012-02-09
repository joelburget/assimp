{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.Formats.Assimp.Material
-- Copyright   : (c) Joel Burget 2011 - 2012
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Corresponds to aiMaterial.h

module Graphics.Formats.Assimp.Material (
    ShadingMode(..)       -- ?
  , BlendMode(..)
  , TextureFlag(..)
  , TextureMapMode(..)
  , TextureMapping(..)
  , TextureOp(..)
  , TextureType(..)
  , PropertyTypeInfo(..)
  , MatKey(..)
  , MaterialProperty(..)
  , Material(..)
  , UVTransform(..)
  , matKeyToTuple
  ) where

#include "assimp.h"
#include "aiMaterial.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Data.Vect (Vec2)
import Graphics.Formats.Assimp.Types
import Control.Monad (join)
import Control.Applicative ((<$>), (<*>), liftA, liftA2)

fromEnum' :: TextureType -> CUInt
fromEnum' = fromInteger . toInteger . fromEnum

-- | Defines all shading models supported by the library.
--
-- The list of shading modes has been taken from Blender. See Blender
-- documentation for more information. The API does not distinguish between
-- "specular" and "diffuse" shaders (thus the specular term for diffuse shading
-- models like Oren-Nayar remains undefined). 
--
-- Again, this value is just a hint. Assimp tries to select the shader whose
-- most common implementation matches the original rendering results of the 3D
-- modeller which wrote a particular model as closely as possible.
data ShadingMode 
  -- | Flat shading.
  --
  -- Shading is done on per-face base, diffuse only. Also known as 'faceted
  -- shading'.
  = Flat
  -- | Simple Gouraud shading.
  | Gouraud
  -- | Phong Shading.
  | Phong
  -- | Phong-Blinn Shading.
  | Blinn
  -- | Toon Shading per pixel.
  | Toon
  -- | Oren-Nayar Shading per pixel.
  --
  -- Extension to standard Lambertian shading, taking the roughness of the
  -- material into account
  | OrenNayar
  -- | Minnaert Shading per pixel.
  --
  -- Extension to standard Lambertian shading, taking the "darkness" of the
  -- material into account
  | Minnaert
  -- | Cook-Torrance Shading per pixel.
  --
  -- Special shader for metallic surfaces.
  | CookTorrance
  -- | No shading at all.
  --
  -- Constant light influence of 1.0.
  | NoShading
  -- | Fresnel shading.
  | Fresnel
  deriving (Show, Eq)

instance Enum ShadingMode where
  fromEnum Flat         = #const aiShadingMode_Flat
  fromEnum Gouraud      = #const aiShadingMode_Gouraud
  fromEnum Phong        = #const aiShadingMode_Phong
  fromEnum Blinn        = #const aiShadingMode_Blinn
  fromEnum Toon         = #const aiShadingMode_Toon
  fromEnum OrenNayar    = #const aiShadingMode_OrenNayar
  fromEnum Minnaert     = #const aiShadingMode_Minnaert
  fromEnum CookTorrance = #const aiShadingMode_CookTorrance
  fromEnum NoShading    = #const aiShadingMode_NoShading
  fromEnum Fresnel      = #const aiShadingMode_Fresnel

  toEnum (#const aiShadingMode_Flat)         = Flat
  toEnum (#const aiShadingMode_Gouraud)      = Gouraud
  toEnum (#const aiShadingMode_Phong)        = Phong
  toEnum (#const aiShadingMode_Blinn)        = Blinn
  toEnum (#const aiShadingMode_Toon)         = Toon
  toEnum (#const aiShadingMode_OrenNayar)    = OrenNayar
  toEnum (#const aiShadingMode_Minnaert)     = Minnaert
  toEnum (#const aiShadingMode_CookTorrance) = CookTorrance
  toEnum (#const aiShadingMode_NoShading)    = NoShading
  toEnum (#const aiShadingMode_Fresnel)      = Fresnel
  toEnum unmatched = error $ "ShadingMode.toEnum: Cannot match " ++ show unmatched

-- | Defines alpha-blend flags.
-- If you're familiar with OpenGL or D3D, these flags aren't new to you. They
-- define *how* the final color value of a pixel is computed, basing on the
-- previous color at that pixel and the new color value from the material. The
-- blend formula is:
--
-- @
-- SourceColor * SourceBlend + DestColor * DestBlend
-- @
--
-- where <DestColor> is the previous color in the framebuffer at this position
-- and <SourceColor> is the material colro before the transparency calculation.
-- This corresponds to the AI_MATKEY_BLEND_FUNC property.
data BlendMode 
  -- | Formula:
  --
  -- @
  -- SourceColor*SourceAlpha + DestColor*(1-SourceAlpha)
  -- @
  = Default
  -- | Additive blending.
  --
  -- Formula:
  -- 
  -- @
  -- SourceColor*1 + DestColor*1
  -- @
  | Additive
  deriving (Show, Eq)

instance Enum BlendMode where
  fromEnum Default  = #const aiBlendMode_Default
  fromEnum Additive = #const aiBlendMode_Additive

  toEnum (#const aiBlendMode_Default)  = Default
  toEnum (#const aiBlendMode_Additive) = Additive
  toEnum unmatched = error $ "BlendMode.toEnum: Cannot match " ++ show unmatched

data TextureFlag = Invert
                  | UseAlpha
                  | IgnoreAlpha
                  deriving (Show, Eq)

instance Enum TextureFlag where
  fromEnum Invert      = #const aiTextureFlags_Invert
  fromEnum UseAlpha    = #const aiTextureFlags_UseAlpha
  fromEnum IgnoreAlpha = #const aiTextureFlags_IgnoreAlpha

  toEnum (#const aiTextureFlags_Invert)      = Invert
  toEnum (#const aiTextureFlags_UseAlpha)    = UseAlpha
  toEnum (#const aiTextureFlags_IgnoreAlpha) = IgnoreAlpha
  toEnum unmatched = error $ "TextureFlag.toEnum: Cannot match " ++ show unmatched

data TextureMapMode = Wrap
                    | Clamp
                    | Decal
                    | Mirror
                    deriving (Show, Eq)

instance Enum TextureMapMode where
  fromEnum Wrap   = #const aiTextureMapMode_Wrap
  fromEnum Clamp  = #const aiTextureMapMode_Clamp
  fromEnum Decal  = #const aiTextureMapMode_Decal
  fromEnum Mirror = #const aiTextureMapMode_Mirror

  toEnum (#const aiTextureMapMode_Wrap)   = Wrap
  toEnum (#const aiTextureMapMode_Clamp)  = Clamp
  toEnum (#const aiTextureMapMode_Decal)  = Decal
  toEnum (#const aiTextureMapMode_Mirror) = Mirror
  toEnum unmatched = error $ "TextureMapMode.toEnum: Cannot match " ++ show unmatched

data TextureMapping = TmUv
                    | TmSphere
                    | TmCylinder
                    | TmBox
                    | TmPlane
                    | TmOther
                    deriving (Show, Eq)

instance Enum TextureMapping where
  fromEnum TmUv       = #const aiTextureMapping_UV
  fromEnum TmSphere   = #const aiTextureMapping_SPHERE
  fromEnum TmCylinder = #const aiTextureMapping_CYLINDER
  fromEnum TmBox      = #const aiTextureMapping_BOX
  fromEnum TmPlane    = #const aiTextureMapping_PLANE
  fromEnum TmOther    = #const aiTextureMapping_OTHER

  toEnum (#const aiTextureMapping_UV)       = TmUv
  toEnum (#const aiTextureMapping_SPHERE)   = TmSphere
  toEnum (#const aiTextureMapping_CYLINDER) = TmCylinder
  toEnum (#const aiTextureMapping_BOX)      = TmBox
  toEnum (#const aiTextureMapping_PLANE)    = TmPlane
  toEnum (#const aiTextureMapping_OTHER)    = TmOther
  toEnum unmatched = error $ "TextureMapping.toEnum: Cannot match " ++ show unmatched

data TextureOp = Multiply
               | Add
               | Subtract
               | Divide
               | SmoothAdd
               | SignedAdd
               deriving (Show, Eq)

instance Enum TextureOp where
  fromEnum Multiply  = #const aiTextureOp_Multiply
  fromEnum Add       = #const aiTextureOp_Add
  fromEnum Subtract  = #const aiTextureOp_Subtract
  fromEnum Divide    = #const aiTextureOp_Divide
  fromEnum SmoothAdd = #const aiTextureOp_SmoothAdd
  fromEnum SignedAdd = #const aiTextureOp_SignedAdd

  toEnum (#const aiTextureOp_Multiply)  = Multiply
  toEnum (#const aiTextureOp_Add)       = Add
  toEnum (#const aiTextureOp_Subtract)  = Subtract
  toEnum (#const aiTextureOp_Divide)    = Divide
  toEnum (#const aiTextureOp_SmoothAdd) = SmoothAdd
  toEnum (#const aiTextureOp_SignedAdd) = SignedAdd
  toEnum unmatched = error $ "TextureOp.toEnum: Cannot match " ++ show unmatched

data TextureType = None
                 | Diffuse
                 | Specular
                 | Ambient
                 | Emissive
                 | Height
                 | Normals
                 | Shininess
                 | Opacity
                 | Displacement
                 | Lightmap
                 | Reflection
                 | Unknown
                 deriving (Show, Eq)

instance Enum TextureType where
  fromEnum None         = #const aiTextureType_NONE
  fromEnum Diffuse      = #const aiTextureType_DIFFUSE
  fromEnum Specular     = #const aiTextureType_SPECULAR
  fromEnum Ambient      = #const aiTextureType_AMBIENT
  fromEnum Emissive     = #const aiTextureType_EMISSIVE
  fromEnum Height       = #const aiTextureType_HEIGHT
  fromEnum Normals      = #const aiTextureType_NORMALS
  fromEnum Shininess    = #const aiTextureType_SHININESS
  fromEnum Opacity      = #const aiTextureType_OPACITY
  fromEnum Displacement = #const aiTextureType_DISPLACEMENT
  fromEnum Lightmap     = #const aiTextureType_LIGHTMAP
  fromEnum Reflection   = #const aiTextureType_REFLECTION
  fromEnum Unknown      = #const aiTextureType_UNKNOWN

  toEnum (#const aiTextureType_NONE)         = None
  toEnum (#const aiTextureType_DIFFUSE)      = Diffuse
  toEnum (#const aiTextureType_SPECULAR)     = Specular
  toEnum (#const aiTextureType_AMBIENT)      = Ambient
  toEnum (#const aiTextureType_EMISSIVE)     = Emissive
  toEnum (#const aiTextureType_HEIGHT)       = Height
  toEnum (#const aiTextureType_NORMALS)      = Normals
  toEnum (#const aiTextureType_SHININESS)    = Shininess
  toEnum (#const aiTextureType_OPACITY)      = Opacity
  toEnum (#const aiTextureType_DISPLACEMENT) = Displacement
  toEnum (#const aiTextureType_LIGHTMAP)     = Lightmap
  toEnum (#const aiTextureType_REFLECTION)   = Reflection
  toEnum (#const aiTextureType_UNKNOWN)      = Unknown
  toEnum unmatched = error $ "TextureType.toEnum: Cannot match " ++ show unmatched

data PropertyTypeInfo = PtiFloat
                       | PtiString
                       | PtiInteger
                       | PtiBuffer
                       deriving (Show, Eq)

instance Enum PropertyTypeInfo where
  fromEnum PtiFloat   = #const aiPTI_Float
  fromEnum PtiString  = #const aiPTI_String
  fromEnum PtiInteger = #const aiPTI_Integer
  fromEnum PtiBuffer  = #const aiPTI_Buffer

  toEnum (#const aiPTI_Float)   = PtiFloat
  toEnum (#const aiPTI_String)  = PtiString
  toEnum (#const aiPTI_Integer) = PtiInteger
  toEnum (#const aiPTI_Buffer)  = PtiBuffer
  toEnum unmatched = error $ "PropertyTypeInfo.toEnum: Cannot match " ++ show unmatched

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
            deriving (Show, Eq)

matKeyToTuple :: MatKey -> (String, CUInt, CUInt)
matKeyToTuple KeyName                   = ("?mat.name",         0,               0)
matKeyToTuple KeyTwoSided               = ("$mat.twosided",     0,               0)
matKeyToTuple KeyShadingModel           = ("$mat.shadingm",     0,               0)
matKeyToTuple KeyEnableWireframe        = ("$mat.wireframe",    0,               0)
matKeyToTuple KeyBlendFunc              = ("$mat.blend",        0,               0)
matKeyToTuple KeyOpacity                = ("$mat.opacity",      0,               0)
matKeyToTuple KeyBumpScaling            = ("$mat.bumpscaling",  0,               0)
matKeyToTuple KeyShininess              = ("$mat.shininess",    0,               0)
matKeyToTuple KeyReflectivity           = ("$mat.reflectivity", 0,               0)
matKeyToTuple KeyShininessStrength      = ("$mat.shinpercent",  0,               0)
matKeyToTuple KeyRefraction             = ("$mat.refracti",     0,               0)
matKeyToTuple KeyColorDiffuse           = ("$clr.diffuse",      0,               0)
matKeyToTuple KeyColorAmbient           = ("$clr.ambient",      0,               0)
matKeyToTuple KeyColorSpecular          = ("$clr.specular",     0,               0)
matKeyToTuple KeyColorEmissive          = ("$clr.emissive",     0,               0)
matKeyToTuple KeyColorTransparent       = ("$clr.transparent",  0,               0)
matKeyToTuple KeyColorReflective        = ("$clr.reflective",   0,               0)
matKeyToTuple KeyGlobalBackgroundImage  = ("?bg.global",        0,               0)
matKeyToTuple (KeyTexture tType i)      = ("$tex.file",         fromEnum' tType, i)
matKeyToTuple (KeyUvWSrc tType i)       = ("$tex.uvwsrc",       fromEnum' tType, i)
matKeyToTuple (KeyTexOp tType i)        = ("$tex.op",           fromEnum' tType, i)
matKeyToTuple (KeyMapping tType i)      = ("$tex.mapping",      fromEnum' tType, i)
matKeyToTuple (KeyTexBlend tType i)     = ("$tex.blend",        fromEnum' tType, i)
matKeyToTuple (KeyMappingModeU tType i) = ("$tex.mapmodeu",     fromEnum' tType, i)
matKeyToTuple (KeyMappingModeV tType i) = ("$tex.mapmodev",     fromEnum' tType, i)
matKeyToTuple (KeyTexMapAxis tType i)   = ("$tex.mapaxis",      fromEnum' tType, i)
matKeyToTuple (KeyUvTransform tType i)  = ("$tex.uvtrafo",      fromEnum' tType, i)
matKeyToTuple (KeyTexFlags tType i)     = ("$tex.flags",        fromEnum' tType, i)

data MaterialProperty = MaterialProperty
  { key      :: String
  , semantic :: TextureType
  , index    :: CUInt
  , mData    :: String
  } deriving (Show)

data Material = Material
  { properties :: [MaterialProperty]
  } deriving (Show)

instance Storable MaterialProperty where
  sizeOf _ = #size aiMaterialProperty
  alignment _ = #alignment aiMaterialProperty
  peek p = MaterialProperty
    <$> liftA aiStringToString ((#peek aiMaterialProperty, mKey) p)
    <*> liftA (toEnum . fromIntegral :: CUInt -> TextureType)
          ((#peek aiMaterialProperty, mSemantic) p)
    <*> (#peek aiMaterialProperty, mIndex) p
    <*> ((#peek aiMaterialProperty, mData) p >>= peekCString)
  poke = undefined

instance Storable Material where
  sizeOf _ = #size aiMaterial
  alignment _ = #alignment aiMaterial
  peek p = Material <$>
    (join $ (liftA2 peekArrayPtr)
      (liftA fromIntegral ((#peek aiMaterial, mNumProperties) p :: IO CUInt))
      ((#peek aiMaterial, mProperties) p))
  poke = undefined

data UVTransform = UVTransform
  { translation :: Vec2
  , scaling     :: Vec2
  , rotation    :: Float
  } deriving (Show)
