{-# LANGUAGE LambdaCase #-}

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
      mod2 :: Signal px (Index 2)
      mod2 = register 0 (countSucc <$> mod2)
      mod3 :: Signal px (Index 3)
      mod3 = register 0 (countSucc <$> mod3)
      mod4 :: Signal px (Index 5)
      mod4 = register 0 (countSucc <$> mod4)
  in  bundle (frameCount, mod2, mod3, mod4) <&> \case
        (fc, m2, m3, m4)
          | fc < 60   -> let z = if m2 == 0 then 200 else 0 in Pixel 255 z z
          | fc < 120  -> let z = if m3 == 0 then 200 else 0 in Pixel z 255 z
          | otherwise -> let z = if m4 == 0 then 200 else 0 in Pixel z z 255
