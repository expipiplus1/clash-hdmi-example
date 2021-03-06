{-# LANGUAGE AllowAmbiguousTypes #-}

module DisplayMode where

import           Clash.Class.Counter
import           Clash.Prelude

data DisplayMode = DisplayMode
  { dmActive   :: Bool
  , dmHSync    :: Bool
  , dmVSync    :: Bool
  , dmNewFrame :: Bool
  }

vgaTiming :: forall px . HiddenClockResetEnable px => Signal px DisplayMode
vgaTiming =
  let v :: Signal px (Index VSize)
      h :: Signal px (Index HSize)
      (v, h)   = unbundle counter

      vActive  = active @VActive <$> v
      vSync    = sync @VSyncStart @VSyncEnd <$> v

      hActive  = active @HActive <$> h
      hSync    = sync @HSyncStart @HSyncEnd <$> h

      newFrame = (<&&>) ((== 0) <$> v) ((== 0) <$> h)
  in  DisplayMode <$> (vActive <&&> hActive) <*> hSync <*> vSync <*> newFrame

sync
  :: forall syncStart syncEnd n
   . (KnownNat syncStart, KnownNat syncEnd, KnownNat n)
  => Index n
  -> Bool
sync = (>= natToNum @syncStart) <&&> (< natToNum @syncEnd)

active :: forall active n . (KnownNat active, KnownNat n) => Index n -> Bool
active = (< natToNum @active)

-- Sky04x
-- pixelClockMHz = 25.175
type HActive = 1280
type HSyncStart = 1390
type HSyncEnd = 1430
type HSize = 1650
type VActive = 720
type VSyncStart = 725
type VSyncEnd = 730
type VSize = 750

-- -- Sky04x
-- -- pixelClockMHz = 25.175
-- type HActive = 640
-- type HSyncStart = 656
-- type HSyncEnd = 752
-- type HSize = 800
-- type VActive = 480
-- type VSyncStart = 490
-- type VSyncEnd = 492
-- type VSize = 525

-- -- pixelClockMHz = 25.175
-- type HActive = 640
-- type HFrontPorch = 16
-- type HSync = 96
-- type HBackPorch = 96
-- type HSyncStart = HActive + HFrontPorch
-- type HSyncEnd = HSyncStart + HSync
-- type HSize = HActive + HFrontPorch + HSync + HBackPorch

-- type VActive = 480
-- type VFrontPorch = 11
-- type VSync = 2
-- type VBackPorch = 31
-- type VSyncStart = VActive + VFrontPorch
-- type VSyncEnd = VSyncStart + VSync
-- type VSize = VActive + VFrontPorch + VSync + VBackPorch

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

counter
  :: (Counter a, HiddenClockResetEnable dom, NFDataX a, Bounded a)
  => Signal dom a
counter = x where x = register minBound (countSucc <$> x)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
