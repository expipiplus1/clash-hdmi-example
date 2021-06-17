{-# OPTIONS_GHC -fno-warn-orphans #-}

module HDMITop where

import           Clash.Prelude
import           Clash.Verification             ( RenderAs(YosysFormal)
                                                , assert
                                                , checkI
                                                , cover
                                                )
import           DisplayMode
import           HDMI
import           TestPattern

{-# ANN topEntity
  (Synthesize
    { t_name   = "hdmiClash"
    , t_inputs = [PortName "clk_i"]
    , t_output = PortProduct "" [PortName "r", PortName "g", PortName "b"]
    }) #-}
topEntity :: Clock PX -> Clock TMDS -> Signal TMDS (Vec 3 (Vec 2 Bit))
topEntity pxClk tmdsClk =
  withClockResetEnable tmdsClk (resetGen @TMDS) enableGen (go @PX @TMDS pxClk)

type TMDS = "TMDS"
type PX = "PX"

instance KnownDomain TMDS where
  type KnownConf TMDS
    = 'DomainConfiguration
        TMDS
        8000
        'Rising
        'Asynchronous
        'Defined
        'ActiveHigh
  knownDomain =
    SDomainConfiguration SSymbol SNat SRising SAsynchronous SDefined SActiveHigh

instance KnownDomain PX where
  type KnownConf PX
    = 'DomainConfiguration
        PX
        40000
        'Rising
        'Asynchronous
        'Defined
        'ActiveHigh
  knownDomain =
    SDomainConfiguration SSymbol SNat SRising SAsynchronous SDefined SActiveHigh


go
  :: forall px tmds
   . ( HiddenClockResetEnable tmds
     , KnownDomain px
     , DomainPeriod px ~ (5 * DomainPeriod tmds)
     )
  => Clock px
  -> Signal tmds (Vec 3 (Vec 2 Bit))
go pxClk =
  let (_, tmds) =
        withSpecificClockResetEnable pxClk resetGen enableGen
          $ let dm = vgaTiming @px
                px = testPattern @px dm
            in  (dm, hdmi dm px)
  in  tmds
