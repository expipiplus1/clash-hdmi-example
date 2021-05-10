{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module TestPattern where

import           Clash.Class.Counter
import           Clash.Prelude
import           Data.Functor
import           DisplayMode
import           HDMI

testPattern
  :: forall px
   . HiddenClockResetEnable px
  => Signal px DisplayMode
  -> Signal px Pixel
testPattern dm =
  let frameCount :: Signal px (Index 180)
      frameCount = regEn 0 (dmNewFrame <$> dm) (countSucc <$> frameCount)
  in  frameCount <&> \case
        fc | fc < 60   -> Pixel 255 0 0
           | fc < 120  -> Pixel 0 255 0
           | otherwise -> Pixel 0 0 255
