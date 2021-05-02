{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Blink where

import           Clash.Prelude
import           Clash.Verification

{-# ANN topEntity
  (Synthesize
    { t_name   = "blink"
    , t_inputs = [PortName "clk_i"]
    , t_output = PortName "led_b"
    }) #-}
topEntity :: Clock System -> Signal System Bool
topEntity clk =
  let rst = resetGen @System
      en  = enableGen
  in  withClockResetEnable clk rst en go

go :: HiddenClockResetEnable dom => Signal dom Bool
go =
  let c = counter @2500000
      r = (<= 1000000) <$> c
  in  checkI "myProperty" AutoRenderAs (cover ((== 0) <$> c)) r

counter :: (KnownNat n, HiddenClockResetEnable dom) => Signal dom (Index n)
counter = x where x = register 0 (x + 1)

