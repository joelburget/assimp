{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.Formats.Assimp.Material
-- Copyright   : (c) Joel Burget 2011-2012
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Corresponds to aiMaterial.h

module Graphics.Formats.Assimp.Material (
    ShadingMode (..)       -- ?
  , BlendMode (..)
  , TextureFlag (..)
  , TextureMapMode (..)
  , TextureMapping (..)
  , TextureOp (..)
  , TextureType (..)
  , PropertyTypeInfo (..)
  , MatKey (..)
  , MaterialProperty (..)
  , MaterialData (..)
  , Material (..)
  , UVTransform (..)
  , matKeyToTuple
  ) where

#include "material.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.String (peekCStringLen)
import Foreign.C.Types
import Foreign.Storable
import Data.Vect (Vec2)
import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Utils 
import Control.Monad (join)
import Control.Applicative ((<$>), (<*>))

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

-- | Defines some mixed flags for a particular texture.
--
-- Usually you'll instruct your cg artists how textures have to look like ...
-- and how they will be processed in your application. However, if you use
-- Assimp for completely generic loading purposes you might also need to
-- process these flags in order to display as many 'unknown' 3D models as
-- possible correctly.
--
-- This corresponds to the AI_MATKEY_TEXFLAGS property.
data TextureFlag
  -- | The texture's color values have to be inverted (componentwise 1-n).
  = Invert
  -- | Explicit request to the application to process the alpha channel of the
  -- texture.
  --
  -- Mutually exclusive with 'IgnoreAlpha'. These flags are set if the library
  -- can say for sure that the alpha channel is used/is not used. If the model
  -- format does not define this, it is left to the application to decide
  -- whether the texture alpha channel - if any - is evaluated or not.
  | UseAlpha
  -- | Explicit request to the application to ignore the alpha channel of the
  -- texture.
  --
  -- Mutually exclusive with 'UseAlpha'.
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

-- | Defines how UV coordinates outside the [0..1] range are handled.
--
-- Commonly refered to as 'wrapping mode'.
data TextureMapMode
  -- | A texture coordinate u|v is translated to u1|v1.
  = Wrap
  -- | Texture coordinates outside [0..1] are clamped to the nearest valid
  -- value.
  | Clamp
  -- | If the texture coordinates for a pixel are outside [0..1] the texture is
  -- not applied to that pixel.
  | Decal
  -- | A texture coordinate u|v becomes u1|v1 if (u-(u1))2 is zero and
  -- 1-(u1)|1-(v1) otherwise.
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

-- | Defines how the mapping coords for a texture are generated.
--
-- Real-time applications typically require full UV coordinates, so the use of
-- the GenUVCoords step is highly recommended. It generates proper UV channels
-- for non-UV mapped objects, as long as an accurate description how the
-- mapping should look like (e.g spherical) is given. See the AI_MATKEY_MAPPING
-- property for more details.
data TextureMapping
  -- | The mapping coordinates are taken from an UV channel.
  --
  -- The AI_MATKEY_UVWSRC key specifies from which UV channel the texture
  -- coordinates are to be taken from (remember, meshes can have more than one
  -- UV channel).
  = TmUv
  | TmSphere   -- ^ Spherical mapping.
  | TmCylinder -- ^ Cylindrical mapping.
  | TmBox      -- ^ Cubic mapping.
  | TmPlane    -- ^ Planar mapping.
  | TmOther    -- ^ Undefined mapping.
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

-- | Defines how the Nth texture of a specific type is combined with the result
-- of all previous layers.
--
-- Example (left: key, right: value):
--
-- @
--   DiffColor0     - gray
--   DiffTextureOp0 - aiTextureOpMultiply
--   DiffTexture0   - tex1.png
--   DiffTextureOp0 - aiTextureOpAdd
--   DiffTexture1   - tex2.png
-- @
--
-- Written as equation, the final diffuse term for a specific pixel would be:
--
-- @
--   diffFinal = DiffColor0 * sampleTex(DiffTexture0,UV0) +
--      sampleTex(DiffTexture1,UV0) * diffContrib;
-- @
--
-- where 'diffContrib' is the intensity of the incoming light for that pixel.
data TextureOp = Multiply  -- ^ T = T1 * T2
               | Add       -- ^ T = T1 + T2
               | Subtract  -- ^ T = T1 - T2
               | Divide    -- ^ T = T1 / T2
               | SmoothAdd -- ^ T = (T1 + T2) - (T1 * T2)
               | SignedAdd -- ^ T = T1 + (T2 - 0.5)
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

-- | Defines the purpose of a texture.
--
-- This is a very difficult topic. Different 3D packages support different
-- kinds of textures. For very common texture types, such as bumpmaps, the
-- rendering results depend on implementation details in the rendering
-- pipelines of these applications. Assimp loads all texture references from
-- the model file and tries to determine which of the predefined texture types
-- below is the best choice to match the original use of the texture as closely
-- as possible.
--
-- In content pipelines you'll usually define how textures have to be handled,
-- and the artists working on models have to conform to this specification,
-- regardless which 3D tool they're using.
data TextureType
  -- | Dummy value.
  --
  -- No texture, but the value to be used as 'texture semantic'
  -- (aiMaterialProperty::mSemantic) for all material properties *not* related
  -- to textures.
  = NoTexture
  -- | The texture is combined with the result of the diffuse lighting
  -- equation.
  | Diffuse
  -- | The texture is combined with the result of the specular lighting
  -- equation.
  | Specular
  -- | The texture is combined with the result of the ambient lighting
  -- equation.
  | Ambient
  -- | The texture is added to the result of the lighting calculation.
  --
  -- It isn't influenced by incoming light.
  | Emissive
  -- | The texture is a height map.
  --
  -- By convention, higher gray-scale values stand for higher elevations from
  -- the base height.
  | Height
  -- | The texture is a (tangent space) normal-map.
  --
  -- Again, there are several conventions for tangent-space normal maps. Assimp
  -- does (intentionally) not distinguish here.
  | Normals
  -- | The texture defines the glossiness of the material.
  --
  -- The glossiness is in fact the exponent of the specular (phong) lighting
  -- equation. Usually there is a conversion function defined to map the linear
  -- color values in the texture to a suitable exponent. Have fun.
  | Shininess
  -- | The texture defines per-pixel opacity.
  --
  -- Usually 'white' means opaque and 'black' means 'transparency'. Or quite
  -- the opposite. Have fun.
  | Opacity
  -- | Displacement texture.
  --
  -- The exact purpose and format is application-dependent. Higher color values
  -- stand for higher vertex displacements.
  | Displacement
  -- | Lightmap texture (aka Ambient Occlusion).
  --
  -- Both 'Lightmaps' and dedicated 'ambient occlusion maps' are covered by
  -- this material property. The texture contains a scaling value for the final
  -- color value of a pixel. Its intensity is not affected by incoming light.
  | Lightmap
  -- | Reflection texture.
  --
  -- Contains the color of a perfect mirror reflection. Rarely used, almost
  -- never for real-time applications.
  | Reflection
  -- | Unknown texture.
  --
  -- A texture reference that does not match any of the definitions above is
  -- considered to be 'unknown'. It is still imported, but is excluded from any
  -- further postprocessing.
  | Unknown
  deriving (Show, Eq)

instance Enum TextureType where
  fromEnum NoTexture    = #const aiTextureType_NONE
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

  toEnum (#const aiTextureType_NONE)         = NoTexture
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

-- | Retrieve a property from a material using 'get'
data MatKey =
  -- | The name of the material, if available.
  --
  -- Note: Ignored by 'RemoveRedundantMaterials'. Materials are considered
  -- equal even if their names are different.
  --
  -- Default: n/a
    KeyName
  -- | Specifies whether meshes using this material must be rendered without
  -- backface culling.
  --
  -- Default: 'False'
  | KeyTwoSided
  -- | One of the 'ShadingMode' enumerated values.
  --
  -- Defines the library shading model to use for (real time) rendering to
  -- approximate the original look of the material as closely as possible.
  --
  -- Default: 'Gouraud'
  | KeyShadingModel
  -- | Specifies whether wireframe rendering must be turned on for the
  -- material.
  --
  -- Default: 'False'
  | KeyEnableWireframe
  -- | One of the 'BlendMode' enumerated values.
  --
  -- Defines how the final color value in the screen buffer is computed from
  -- the given color at that position and the newly computed color from the
  -- material. Simply said, alpha blending settings.
  --
  -- Default: 'False'
  | KeyBlendFunc
  -- | Defines the opacity of the material in a range between 0..1.
  --
  -- Default: 1.0
  | KeyOpacity
  | KeyBumpScaling
  -- | Defines the shininess of a phong-shaded material.
  --
  -- This is actually the exponent of the phong specular equation.
  --
  -- Default: 0.0
  | KeyShininess
  | KeyReflectivity
  | KeyShininessStrength
  -- | Scales the specular color of the material.
  --
  -- Default: 1.0
  | KeyRefraction
  -- | Diffuse color of the material.
  --
  -- This is typically scaled by the amount of incoming diffuse light (e.g.
  -- using gouraud shading).
  --
  -- Default: Black
  | KeyColorDiffuse
  -- | Ambient color of the material.
  --
  -- This is typically scaled by the amount of ambient light.
  --
  -- Default: Black
  | KeyColorAmbient
  -- | Specular color of the material.
  --
  -- This is typically scaled by the amount of incoming specular light (e.g.
  -- using phong shading)
  --
  -- Default: Black
  | KeyColorSpecular
  -- | Emissive color of the material.
  --
  -- This is the amount of light emitted by the object. In real time
  -- applications it will usually not affect surrounding objects, but
  -- raytracing applications may wish to treat emissive objects as light
  -- sources.
  --
  -- Default: Black
  | KeyColorEmissive
  -- | Defines the transparent color of the material.
  --
  -- This is the color to be multiplied with the color of translucent light to
  -- construct the final 'destination color' for a particular position in the
  -- screen buffer.
  --
  -- Default: Black
  | KeyColorTransparent
  | KeyColorReflective
  | KeyGlobalBackgroundImage
  -- | Defines the path to the nth texture on the stack 't'
  --
  -- ... where 'n' is >= 0 and 't' is a 'TextureType'.
  --
  -- Default: n/a
  | KeyTexture TextureType CUInt
  -- | Defines the UV channel to be used as input mapping coordinates for
  -- sampling the nth texture on the stack 't'. All meshes assigned to this
  -- material share the same UV channel setup.
  --
  -- Default: n/a
  | KeyUvWSrc TextureType CUInt
  -- | One of the 'TextureOp' enumerated values.
  --
  -- Defines the arithmetic operation to be used to combine the n'th texture on
  -- the stack 't' with the n-1'th. 'KeyTexOp t 0' refers to the blend
  -- operation between the base color for this stack (e.g. 'Diffuse' for the
  -- diffuse stack) and the first texture.
  --
  -- Default: n/a
  | KeyTexOp TextureType CUInt
  -- | Defines how the input mapping coordinates for sampling the n'th texture
  -- on the stack 't' are computed. Usually explicit UV coordinates are
  -- provided, but some model file formats might also be using basic shapes,
  -- such as spheres or cylinders, to project textures onto meshes.
  --
  -- Default: n/a
  | KeyMapping TextureType CUInt
  -- | Defines the strength the n'th texture on the stack 't'.
  --
  -- All color components (rgb) are multipled with this factor *before* any
  -- further processing is done.
  --
  -- Default: n/a
  | KeyTexBlend TextureType CUInt
  -- | Any of the aiTextureMapMode enumerated values.
  --
  -- Defines the texture wrapping mode on the x axis for sampling the n'th
  -- texture on the stack 't'. 'Wrapping' occurs whenever UVs lie outside the
  -- 0..1 range.
  --
  -- Default: n/a
  | KeyMappingModeU TextureType CUInt
  -- | Wrap mode on the v axis.
  --
  -- See 'KeyMappingModeU'.
  --
  -- Default: n/a
  | KeyMappingModeV TextureType CUInt
  -- | Defines the base axis to to compute the mapping coordinates for the n'th
  -- texture on the stack 't' from.
  --
  -- This is not required for UV-mapped textures. For instance, if MAPPING(t,n)
  -- is 'TmSphere', U and V would map to longitude and latitude of
  -- a sphere around the given axis.  The axis is given in local mesh space.
  --
  -- Default: n/a
  | KeyTexMapAxis TextureType CUInt
  | KeyUvTransform TextureType CUInt
  -- | Defines miscellaneous flag for the n'th texture on the stack 't'.
  --
  -- This is a bitwise combination of the 'TextureFlags' enumerated values.
  | KeyTexFlags TextureType CUInt
  deriving (Show, Eq)

matKeyToTuple :: MatKey -> (String, CUInt, CUInt)
matKeyToTuple KeyName                   = ("?mat.name",                   0, 0)
matKeyToTuple KeyTwoSided               = ("$mat.twosided",               0, 0)
matKeyToTuple KeyShadingModel           = ("$mat.shadingm",               0, 0)
matKeyToTuple KeyEnableWireframe        = ("$mat.wireframe",              0, 0)
matKeyToTuple KeyBlendFunc              = ("$mat.blend",                  0, 0)
matKeyToTuple KeyOpacity                = ("$mat.opacity",                0, 0)
matKeyToTuple KeyBumpScaling            = ("$mat.bumpscaling",            0, 0)
matKeyToTuple KeyShininess              = ("$mat.shininess",              0, 0)
matKeyToTuple KeyReflectivity           = ("$mat.reflectivity",           0, 0)
matKeyToTuple KeyShininessStrength      = ("$mat.shinpercent",            0, 0)
matKeyToTuple KeyRefraction             = ("$mat.refracti",               0, 0)
matKeyToTuple KeyColorDiffuse           = ("$clr.diffuse",                0, 0)
matKeyToTuple KeyColorAmbient           = ("$clr.ambient",                0, 0)
matKeyToTuple KeyColorSpecular          = ("$clr.specular",               0, 0)
matKeyToTuple KeyColorEmissive          = ("$clr.emissive",               0, 0)
matKeyToTuple KeyColorTransparent       = ("$clr.transparent",            0, 0)
matKeyToTuple KeyColorReflective        = ("$clr.reflective",             0, 0)
matKeyToTuple KeyGlobalBackgroundImage  = ("?bg.global",                  0, 0)
matKeyToTuple (KeyTexture tType i)      = ("$tex.file",     fromEnum' tType, i)
matKeyToTuple (KeyUvWSrc tType i)       = ("$tex.uvwsrc",   fromEnum' tType, i)
matKeyToTuple (KeyTexOp tType i)        = ("$tex.op",       fromEnum' tType, i)
matKeyToTuple (KeyMapping tType i)      = ("$tex.mapping",  fromEnum' tType, i)
matKeyToTuple (KeyTexBlend tType i)     = ("$tex.blend",    fromEnum' tType, i)
matKeyToTuple (KeyMappingModeU tType i) = ("$tex.mapmodeu", fromEnum' tType, i)
matKeyToTuple (KeyMappingModeV tType i) = ("$tex.mapmodev", fromEnum' tType, i)
matKeyToTuple (KeyTexMapAxis tType i)   = ("$tex.mapaxis",  fromEnum' tType, i)
matKeyToTuple (KeyUvTransform tType i)  = ("$tex.uvtrafo",  fromEnum' tType, i)
matKeyToTuple (KeyTexFlags tType i)     = ("$tex.flags",    fromEnum' tType, i)

buildMatKey :: String -> TextureType -> CUInt -> MatKey
buildMatKey str tType i = case str of
  "?mat.name"         -> KeyName
  "$mat.twosided"     -> KeyTwoSided
  "$mat.shadingm"     -> KeyShadingModel
  "$mat.wireframe"    -> KeyEnableWireframe
  "$mat.blend"        -> KeyBlendFunc
  "$mat.opacity"      -> KeyOpacity
  "$mat.bumpscaling"  -> KeyBumpScaling
  "$mat.shininess"    -> KeyShininess
  "$mat.reflectivity" -> KeyReflectivity
  "$mat.shinpercent"  -> KeyShininessStrength
  "$mat.refracti"     -> KeyRefraction
  "$clr.diffuse"      -> KeyColorDiffuse
  "$clr.ambient"      -> KeyColorAmbient
  "$clr.specular"     -> KeyColorSpecular
  "$clr.emissive"     -> KeyColorEmissive
  "$clr.transparent"  -> KeyColorTransparent
  "$clr.reflective"   -> KeyColorReflective
  "?bg.global"        -> KeyGlobalBackgroundImage
  "$tex.file"         -> KeyTexture tType i
  "$tex.uvwsrc"       -> KeyUvWSrc tType i
  "$tex.op"           -> KeyTexOp tType i
  "$tex.mapping"      -> KeyMapping tType i
  "$tex.blend"        -> KeyTexBlend tType i
  "$tex.mapmodeu"     -> KeyMappingModeU tType i
  "$tex.mapmodev"     -> KeyMappingModeV tType i
  "$tex.mapaxis"      -> KeyTexMapAxis tType i
  "$tex.uvtrafo"      -> KeyUvTransform tType i
  "$tex.flags"        -> KeyTexFlags tType i
  unmatched           -> error $ "buildMatKey: Cannot match " ++ show unmatched

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
  toEnum unmatched = error $
    "PropertyTypeInfo.toEnum: Cannot match " ++ show unmatched

data MaterialProperty = MaterialProperty
  { key      :: MatKey
  --, semantic :: TextureType
  --, index    :: CUInt
  , mData    :: MaterialData
  } deriving (Show)

data MaterialData =
    MaterialFloat Float
  | MaterialString String
  | MaterialInt CUInt
  | MaterialBuffer CUInt (Ptr Char)
  deriving (Show)

newtype Material = Material {properties :: [MaterialProperty]} deriving (Show)

instance Storable MaterialProperty where
  sizeOf _ = #size aiMaterialProperty
  alignment _ = #alignment aiMaterialProperty
  peek p = do
    key'      <- aiStringToString <$> (#peek aiMaterialProperty, mKey) p
    semantic' <- (toEnum . fromIntegral :: CUInt -> TextureType) <$>
      (#peek aiMaterialProperty, mSemantic) p
    index'    <- (#peek aiMaterialProperty, mIndex) p
    mType'    <- (toEnum . fromIntegral :: CUInt -> PropertyTypeInfo) <$>
      (#peek aiMaterialProperty, mType) p
    pmData    <- (#peek aiMaterialProperty, mData) p
    len <- (#peek aiMaterialProperty, mDataLength) p :: IO CUInt
    logPrint (key', index', mType', pmData, len)
    mData'    <- let ptr = castPtr pmData in case mType' of
      PtiFloat   -> MaterialFloat                     <$> peek ptr
      PtiString  -> MaterialString <$> peekCStringLen (ptr, fromIntegral len)
      PtiInteger -> MaterialInt                       <$> peek ptr
      PtiBuffer  -> MaterialBuffer len <$> peek ptr
    logPrint mData'
    return $ MaterialProperty (buildMatKey key' semantic' index') mData'
  poke = undefined

instance Storable Material where
  sizeOf _ = #size aiMaterial
  alignment _ = #alignment aiMaterial
  peek p = do
      mNumProperties <- fromIntegral <$> ((#peek aiMaterial, mNumProperties) p :: IO CUInt)
      logPrint mNumProperties
      arrPtr <- (#peek aiMaterial, mProperties) p
      -- let props' = (castPtr arrPtr :: Ptr ())
      -- print props'
      props <- peekArrayPtr mNumProperties arrPtr
      return $ Material props
  -- peek p = Material <$>
  --   (join $ peekArrayPtr
  --     <$> (fromIntegral <$> ((#peek aiMaterial, mNumProperties) p :: IO CUInt))
  --     <*> (#peek aiMaterial, mProperties) p)
  poke = undefined

data UVTransform = UVTransform
  { translation :: Vec2
  , scaling     :: Vec2
  , rotation    :: Float
  } deriving (Show)
