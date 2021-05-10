{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HDMITop where

import           Clash.Prelude
import           Clash.Verification             ( RenderAs(YosysSVA)
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
topEntity :: Clock PX -> Clock TMDS -> Signal TMDS (Vec 3 Bit)
topEntity pxClk tmdsClk =
  withClockResetEnable tmdsClk (resetGen @TMDS) enableGen (go @PX @TMDS pxClk)

type TMDS = "TMDS"
type PX = "PX"

instance KnownDomain TMDS where
  type KnownConf TMDS
    = 'DomainConfiguration
        TMDS
        4000
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
     , DomainPeriod px ~ (10 * DomainPeriod tmds)
     )
  => Clock px
  -> Signal tmds (Vec 3 Bit)
go pxClk =
  let (dm, tmds) =
        withSpecificClockResetEnable pxClk resetGen enableGen
          $ let dm = vgaTiming @px
                px = testPattern @px dm
            in  (dm, hdmi dm px)
  in  tmds
