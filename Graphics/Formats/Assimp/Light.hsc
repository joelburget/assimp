{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.Formats.Assimp.Light
-- Copyright   : (c) Joel Burget 2011 - 2012
-- License     : BSD3
--
-- Maintainer  : Joel Burget <joelburget@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Corresponds to aiLight.h

module Graphics.Formats.Assimp.Light (
    LightSourceType(..)
  , Light(..)
  ) where

#include "light.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Control.Applicative (liftA, (<$>), (<*>))
import Foreign.Storable
import Data.Vect.Float (Vec3(Vec3))
import Graphics.Formats.Assimp.Types

-- | All supported types of light sources
data LightSourceType 
  = LightSourceUndefined
  -- | A directional light source has a well-defined direction but is
  -- infinitely far away.
  --
  -- That's quite a good approximation for sun light.
  | LightSourceDirectional
  -- | A point light source has a well-defined position in space but no
  -- direction - it emits light in all directions.
  --
  -- A normal bulb is a point light.
  | LightSourcePoint
  -- | A spot light source emits light in a specific angle.
  --
  -- It has a position and a direction it is pointing to. A good example for a
  -- spot light is a light spot in sport arenas.
  | LightSourceSpot
  deriving (Show,Eq)

instance Enum LightSourceType where
  fromEnum LightSourceUndefined = 0
  fromEnum LightSourceDirectional = 1
  fromEnum LightSourcePoint = 2
  fromEnum LightSourceSpot = 3

  toEnum 0 = LightSourceUndefined
  toEnum 1 = LightSourceDirectional
  toEnum 2 = LightSourcePoint
  toEnum 3 = LightSourceSpot
  toEnum unmatched = error ("LightSourceType.toEnum: Cannot match " ++ show unmatched)

data Light = Light { 
  -- | The name of the light source
  -- There must be a node in the scenegraph with the same name. This node
  -- specifies the position of the light in the scene hierarchy and can be
  -- animated.
    lightName            :: String
  -- | The type of the light source
  --
  -- 'LightSourceUndefined' is not a valid value for this member.
  , mType                :: LightSourceType
  -- | Position of the light source in space
  -- Relative to the transformation of the node corresponding to the light.
  --
  -- The position is undefined for directional lights.
  , lightPosition        :: Vec3
  -- | Direction of the light source in space
  --
  -- Relative to the transformation of the node corresponding to the light.
  --
  -- The direction is undefined for point lights. The vector may be normalized,
  -- but it needn't.
  , direction            :: Vec3
  -- | Constant light attenuation factor
  --
  -- The intensity of the light source at a given distance 'd' from the light's
  -- position is
  --
  -- @
  -- Atten = 1/( att0 + att1 * d + att2 * d*d)
  -- @
  --
  -- This member corresponds to the att0 variable in the equation. Naturally
  -- undefined for directional lights.
  , attenuationConstant  :: Float
  -- | Linear light attenuation factor
  --
  -- The intensity of the light source at a given distance 'd' from the light's
  -- position is
  --
  -- @
  -- Atten = 1/( att0 + att1 * d + att2 * d*d)
  -- @
  --
  -- This member corresponds to the att1 variable in the equation. Naturally
  -- undefined for directional lights.
  , attenuationLinear    :: Float
  -- | Quadratic light attenuation factor
  --
  -- The intensity of the light source at a given distance 'd' from the light's
  -- position is
  --
  -- @
  -- Atten = 1/( att0 + att1 * d + att2 * d*d)
  -- @
  --
  -- This member corresponds to the att2 variable in the equation. Naturally
  -- undefined for directional lights.
  , attenuationQuadratic :: Float
  -- | Diffuse color of the light source
  --
  -- The diffuse light color is multiplied with the diffuse material color to
  -- obtain the final color that contributes to the diffuse shading term.
  , colorDiffuse         :: Color3F
  -- | Specular color of the light source
  --
  -- The specular light color is multiplied with the specular material color to
  -- obtain the final color that contributes to the specular shading term.
  , colorSpecular        :: Color3F
  -- | Ambient color of the light source
  --
  -- The ambient light color is multiplied with the ambient material color to
  -- obtain the final color that contributes to the ambient shading term. Most
  -- renderers will ignore this value it, is just a remaining of the
  -- fixed-function pipeline that is still supported by quite many file
  -- formats.
  , colorAmbient         :: Color3F
  -- | Inner angle of a spot light's cone
  --
  -- The spot light has maximum influence on objects inside this angle. The
  -- angle is given in radians. It is 2PI for point lights and undefined for
  -- directional lights.
  , angleInnerCone       :: Float
  -- | Outer angle of a spot light's light cone
  --
  -- The spot light does not affect objects outside this angle. The angle is
  -- given in radians. It is 2PI for point lights and undefined for directional
  -- lights. The outer angle must be greater than or equal to the inner angle.
  -- It is assumed that the application uses a smooth interpolation between the
  -- inner and the outer cone of the spot light.
  , angleOuterCone       :: Float
  } deriving (Show)

instance Position Light where
  position = lightPosition

instance Name Light where
  name = lightName

instance Storable Light where
  sizeOf _ = #size aiLight
  alignment _ = #alignment aiLight
  peek p = Light
    <$> liftA aiStringToString ((#peek aiLight, mName) p)
    <*> liftA toEnum ((#peek aiLight, mType) p)
    <*> (#peek aiLight, mPosition) p
    <*> (#peek aiLight, mDirection) p
    <*> (#peek aiLight, mAttenuationConstant) p
    <*> (#peek aiLight, mAttenuationLinear) p
    <*> (#peek aiLight, mAttenuationQuadratic) p
    <*> (#peek aiLight, mColorDiffuse) p
    <*> (#peek aiLight, mColorSpecular) p
    <*> (#peek aiLight, mColorAmbient) p
    <*> (#peek aiLight, mAngleInnerCone) p
    <*> (#peek aiLight, mAngleOuterCone) p
--  peek p = do
--    mName                    <- liftM aiStringToString $ (#peek aiLight, mName) p
--    mType                    <- liftM toEnum $ (#peek aiLight, mType) p
--    (Vec3F mPosition)        <- (#peek aiLight, mPosition) p
--    (Vec3F mDirection)       <- (#peek aiLight, mDirection) p
--    mAttenuationConstant     <- (#peek aiLight, mAttenuationConstant) p
--    mAttenuationLinear       <- (#peek aiLight, mAttenuationLinear) p
--    mAttenuationQuadratic    <- (#peek aiLight, mAttenuationQuadratic) p
--    (Color3F mColorDiffuse)  <- (#peek aiLight, mColorDiffuse) p
--    (Color3F mColorSpecular) <- (#peek aiLight, mColorSpecular) p
--    (Color3F mColorAmbient)  <- (#peek aiLight, mColorAmbient) p
--    mAngleInnerCone          <- (#peek aiLight, mAngleInnerCone) p
--    mAngleOuterCone          <- (#peek aiLight, mAngleOuterCone) p
--    return $ Light
--               mName mType mPosition mDirection mAttenuationConstant
--               mAttenuationLinear mAttenuationQuadratic mColorDiffuse
--               mColorSpecular mColorAmbient mAngleInnerCone mAngleOuterCone
  poke = undefined
