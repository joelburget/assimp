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
-- Corresponds to aiLight.h

module Graphics.Formats.Assimp.Light (
    LightSourceType(..)
  , Light(..)
  ) where

#include "assimp.h"
#include "typedefs.h"

data LightSourceType = LightSourceUndefined
                     | LightSourceDirectional
                     | LightSourcePoint
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

data Light = Light
  { lightName            :: String
  , mType                :: LightSourceType
  , lightPosition        :: Vec3
  , direction            :: Vec3
  , attenuationConstant  :: Float
  , attenuationLinear    :: Float
  , attenuationQuadratic :: Float
  , colorDiffuse         :: Vec3
  , colorSpecular        :: Vec3
  , colorAmbient         :: Vec3
  , angleInnerCone       :: Float
  , angleOuterCone       :: Float
  } deriving (Show)
{#pointer *aiLight as LightPtr -> Light#}

instance Position Light where
  position = lightPosition

instance Name Light where
  name = lightName

instance Storable Light where
  sizeOf _ = #size aiLight
  alignment _ = #alignment aiLight
  peek p = do
    mName                    <- liftM aiStringToString $ (#peek aiLight, mName) p
    mType                    <- liftM toEnum $ (#peek aiLight, mType) p
    (Vec3F mPosition)        <- (#peek aiLight, mPosition) p
    (Vec3F mDirection)       <- (#peek aiLight, mDirection) p
    mAttenuationConstant     <- (#peek aiLight, mAttenuationConstant) p
    mAttenuationLinear       <- (#peek aiLight, mAttenuationLinear) p
    mAttenuationQuadratic    <- (#peek aiLight, mAttenuationQuadratic) p
    (Color3F mColorDiffuse)  <- (#peek aiLight, mColorDiffuse) p
    (Color3F mColorSpecular) <- (#peek aiLight, mColorSpecular) p
    (Color3F mColorAmbient)  <- (#peek aiLight, mColorAmbient) p
    mAngleInnerCone          <- (#peek aiLight, mAngleInnerCone) p
    mAngleOuterCone          <- (#peek aiLight, mAngleOuterCone) p
    return $ Light
               mName mType mPosition mDirection mAttenuationConstant
               mAttenuationLinear mAttenuationQuadratic mColorDiffuse
               mColorSpecular mColorAmbient mAngleInnerCone mAngleOuterCone
  poke = undefined
