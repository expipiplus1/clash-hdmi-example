{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Blink where

import           Clash.Prelude
import           Clash.Verification             ( RenderAs(YosysSVA)
                                                , assert
                                                , checkI
                                                , cover
                                                )

{-# ANN topEntity
  (Synthesize
    { t_name   = "blink"
    , t_inputs = [PortName "clk_i"]
    , t_output = PortProduct "" [PortName "led_r", PortName "led_g", PortName "led_b"]
    }) #-}
topEntity :: Clock System -> Signal System (Bool, Bool, Bool)
topEntity clk =
  let rst = resetGen @System
      en  = enableGen
  in  withClockResetEnable clk rst en go

go :: HiddenClockResetEnable dom => Signal dom (Bool, Bool, Bool)
go =
  let c   = counter @2500000
      r   = (< 1000000) <$> c
      g   = ((>= 1000000) <&&> (< 2000000)) <$> c
      b   = (>= 2000000) <$> c
      out = bundle (r, g, b)
      p   = isPrimary <$> out
  in  checkI "isPrimary" YosysSVA (assert p)
        . checkI "R" YosysSVA (cover r)
        . checkI "G" YosysSVA (cover g)
        . checkI "B" YosysSVA (cover b)
        $ out

isPrimary :: (Bool, Bool, Bool) -> Bool
isPrimary (r, g, b) = r `xor` g `xor` b

counter :: (KnownNat n, HiddenClockResetEnable dom) => Signal dom (Index n)
counter = x where x = register 0 (x + 1)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
