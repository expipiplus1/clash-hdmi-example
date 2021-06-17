{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module HDMI where

import           Clash.Class.Counter
import           Clash.Prelude
import           DisplayMode
import qualified Prelude
import           TMDS

data Pixel = Pixel
  { pxR :: Unsigned 8
  , pxG :: Unsigned 8
  , pxB :: Unsigned 8
  }

hdmi
  :: ( HiddenClockResetEnable px
     , HiddenClockResetEnable tmds
     , DomainPeriod px ~ (5 * DomainPeriod tmds)
     )
  => Signal px DisplayMode
  -> Signal px Pixel
  -> Signal tmds (Vec 3 (Vec 2 Bit))
hdmi dm px =
  let tmdsWords = toTMDS dm px
      bits      = bundle . map serializeTMDS . unbundle $ tmdsWords
  in  bits

toTMDS
  :: HiddenClockResetEnable px
  => Signal px DisplayMode
  -> Signal px Pixel
  -> Signal px (Vec 3 TMDSWord)
toTMDS = liftA2 $ \DisplayMode {..} Pixel {..} ->
  let red   = if dmActive then Data (bitCoerce pxR) else Control 0b00
      green = if dmActive then Data (bitCoerce pxG) else Control 0b00
      blue  = if dmActive
        then Data (bitCoerce pxB)
        else Control (v2bv (boolToBit dmVSync :> boolToBit dmHSync :> Nil))
  in  red :> green :> blue :> Nil

serializeTMDS
  :: (HiddenClockResetEnable px, HiddenClockResetEnable tmds)
  => DomainPeriod px ~ (5 * DomainPeriod tmds)
  => Signal px TMDSWord
  -> Signal tmds (Vec 2 Bit)
serializeTMDS tmds =
  let encoded     = tmdsEncode (register (Data 0) tmds)
      encodedFast = register 0 $ unsafeSynchronizer (register 0 encoded)
  in  shiftOut . fmap pairs . fmap bv2v $ encodedFast

pairs :: KnownNat n => Vec (n*2) a -> Vec n (Vec 2 a)
pairs = unconcatI

-- >>> simulateN @System 9 shiftOut (Prelude.replicate 3 =<< [0b101 :: BitVector 3, 0b111, 0b000])
-- [1,0,1,1,1,1,0,0,0]
shiftOut
  :: forall n a dom
   . (HiddenClockResetEnable dom, KnownNat n, NFDataX a)
  => Signal dom (Vec (n+1) a)
  -> Signal dom a
shiftOut i =
  let
    c :: Signal dom (Index (n+1))
    c    = register 0 (countSucc <$> c)
    zero = register False ((== 0) <$> c)
    i'   = i
    s = mux zero i' ((error "shift" +>>) <$> register (pure (error "shift")) s)
  in
    last <$> s
