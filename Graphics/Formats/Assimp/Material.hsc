{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module : Graphics.Formats.Assimp.Material
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
import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Vector (Vec2F)
import Control.Monad (join)
import Control.Applicative ((<$>), (<*>), liftA, liftA2)

fromEnum' :: TextureType -> CUInt
fromEnum' = fromInteger . toInteger . fromEnum

data ShadingMode = Flat
                 | Gouraud
                 | Phong
                 | Blinn
                 | Toon
                 | OrenNayar
                 | Minnaert
                 | CookTorrance
                 | NoShading
                 | Fresnel
                 deriving (Show, Eq)

instance Enum ShadingMode where
  fromEnum Flat         = 0x1
  fromEnum Gouraud      = 0x2
  fromEnum Phong        = 0x3
  fromEnum Blinn        = 0x4
  fromEnum Toon         = 0x5
  fromEnum OrenNayar    = 0x6
  fromEnum Minnaert     = 0x7
  fromEnum CookTorrance = 0x8
  fromEnum NoShading    = 0x9
  fromEnum Fresnel      = 0xa

  toEnum 0x1 = Flat
  toEnum 0x2 = Gouraud
  toEnum 0x3 = Phong
  toEnum 0x4 = Blinn
  toEnum 0x5 = Toon
  toEnum 0x6 = OrenNayar
  toEnum 0x7 = Minnaert
  toEnum 0x8 = CookTorrance
  toEnum 0x9 = NoShading
  toEnum 0xa = Fresnel
  toEnum unmatched = error $ "ShadingMode.toEnum: Cannot match " ++ show unmatched

data BlendMode = Default
               | Additive
               deriving (Show, Eq)

instance Enum BlendMode where
  fromEnum Default  = 0x0
  fromEnum Additive = 0x1

  toEnum 0x0 = Default
  toEnum 0x1 = Additive
  toEnum unmatched = error $ "BlendMode.toEnum: Cannot match " ++ show unmatched

data TextureFlags = Invert
                  | UseAlpha
                  | IgnoreAlpha
                  deriving (Show, Eq)

instance Enum TextureFlags where
  fromEnum Invert      = 0x1
  fromEnum UseAlpha    = 0x2
  fromEnum IgnoreAlpha = 0x4

  toEnum 0x1 = Invert
  toEnum 0x2 = UseAlpha
  toEnum 0x4 = IgnoreAlpha
  toEnum unmatched = error $ "TextureFlags.toEnum: Cannot match " ++ show unmatched

data TextureMapMode = Wrap
                    | Clamp
                    | Decal
                    | Mirror
                    deriving (Show, Eq)

instance Enum TextureMapMode where
  fromEnum Wrap   = 0x0
  fromEnum Clamp  = 0x1
  fromEnum Decal  = 0x3
  fromEnum Mirror = 0x2

  toEnum 0x0 = Wrap
  toEnum 0x1 = Clamp
  toEnum 0x3 = Decal
  toEnum 0x2 = Mirror
  toEnum unmatched = error $ "TextureMapMode.toEnum: Cannot match " ++ show unmatched

data TextureMapping = TmUv
                    | TmSphere
                    | TmCylinder
                    | TmBox
                    | TmPlane
                    | TmOther
                    deriving (Show, Eq)

instance Enum TextureMapping where
  fromEnum TmUv       = 0x0
  fromEnum TmSphere   = 0x1
  fromEnum TmCylinder = 0x2
  fromEnum TmBox      = 0x3
  fromEnum TmPlane    = 0x4
  fromEnum TmOther    = 0x5

  toEnum 0x0 = TmUv
  toEnum 0x1 = TmSphere
  toEnum 0x2 = TmCylinder
  toEnum 0x3 = TmBox
  toEnum 0x4 = TmPlane
  toEnum 0x5 = TmOther
  toEnum unmatched = error $ "TextureMapping.toEnum: Cannot match " ++ show unmatched

data TextureOp = Multiply
               | Add
               | Subtract
               | Divide
               | SmoothAdd
               | SignedAdd
               deriving (Show, Eq)

instance Enum TextureOp where
  fromEnum Multiply  = 0x0
  fromEnum Add       = 0x1
  fromEnum Subtract  = 0x2
  fromEnum Divide    = 0x3
  fromEnum SmoothAdd = 0x4
  fromEnum SignedAdd = 0x5

  toEnum 0x0 = Multiply
  toEnum 0x1 = Add
  toEnum 0x2 = Subtract
  toEnum 0x3 = Divide
  toEnum 0x4 = SmoothAdd
  toEnum 0x5 = SignedAdd
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
  fromEnum None         = 0x0
  fromEnum Diffuse      = 0x1
  fromEnum Specular     = 0x2
  fromEnum Ambient      = 0x3
  fromEnum Emissive     = 0x4
  fromEnum Height       = 0x5
  fromEnum Normals      = 0x6
  fromEnum Shininess    = 0x7
  fromEnum Opacity      = 0x8
  fromEnum Displacement = 0x9
  fromEnum Lightmap     = 0xa
  fromEnum Reflection   = 0xb
  fromEnum Unknown      = 0xc

  toEnum 0x0 = None
  toEnum 0x1 = Diffuse
  toEnum 0x2 = Specular
  toEnum 0x3 = Ambient
  toEnum 0x4 = Emissive
  toEnum 0x5 = Height
  toEnum 0x6 = Normals
  toEnum 0x7 = Shininess
  toEnum 0x8 = Opacity
  toEnum 0x9 = Displacement
  toEnum 0xa = Lightmap
  toEnum 0xb = Reflection
  toEnum 0xc = Unknown
  toEnum unmatched = error $ "TextureType.toEnum: Cannot match " ++ show unmatched

data PropertyTypeInfo = PtiFloat
                       | PtiString
                       | PtiInteger
                       | PtiBuffer
                       deriving (Show, Eq)

instance Enum PropertyTypeInfo where
  fromEnum PtiFloat   = 0x1
  fromEnum PtiString  = 0x3
  fromEnum PtiInteger = 0x4
  fromEnum PtiBuffer  = 0x5

  toEnum 0x1 = PtiFloat
  toEnum 0x3 = PtiString
  toEnum 0x4 = PtiInteger
  toEnum 0x5 = PtiBuffer
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
  { translation :: Vec2F
  , scaling     :: Vec2F
  , rotation    :: Float
  } deriving (Show)
