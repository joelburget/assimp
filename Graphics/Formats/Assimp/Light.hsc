{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module : Graphics.Formats.Assimp.Light
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

#include "aiLight.h"
#include "typedefs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Control.Applicative (liftA, (<$>), (<*>))
import Foreign.Storable
import Graphics.Formats.Assimp.Types
import Graphics.Formats.Assimp.Vector

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
  , lightPosition        :: Vec3F
  , direction            :: Vec3F
  , attenuationConstant  :: Float
  , attenuationLinear    :: Float
  , attenuationQuadratic :: Float
  , colorDiffuse         :: Color3F
  , colorSpecular        :: Color3F
  , colorAmbient         :: Color3F
  , angleInnerCone       :: Float
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
