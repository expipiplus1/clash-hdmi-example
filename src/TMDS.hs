{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module TMDS where

import           Clash.Prelude           hiding ( scanr1 )
import           Clash.Sized.Internal.BitVector ( popCountBV )
import           Data.Bool                      ( bool )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( )
import qualified Prelude

data TMDSWord
  = Data (BitVector 8)
  | Control (BitVector 2)
  deriving (Eq, Show, Generic, NFDataX)

tmdsEncode
  :: HiddenClockResetEnable dom
  => Signal dom TMDSWord
  -> Signal dom (BitVector 10)
tmdsEncode = mealy tmdsEncode1 0

-- prop> \d a -> (tmdsDecode . snd . tmdsEncode1 a . Data $ d) == Data d
-- +++ OK, passed 100 tests.
--
-- prop> \d a -> (tmdsDecode . snd . tmdsEncode1 a . Control $ d) == Control d
-- +++ OK, passed 100 tests.
--
-- prop> \d a -> let (a', c) = tmdsEncode1 a (Data d) in a' == a + 2 * bitCoerce (popCountBV c) - 10
-- +++ OK, passed 100 tests.
tmdsEncode1 :: Signed 4 -> TMDSWord -> (Signed 4, BitVector 10)
tmdsEncode1 acc = \case
  Control c -> (0, ) $ case c of
    0b00 -> 0b1101010100
    0b01 -> 0b0010101011
    0b10 -> 0b0101010100
    0b11 -> 0b1010101011
  Data d ->
    let pop        = popCountBV d

        (tag1, op) = if pop > 4 || pop == 4 && testBit d 1
          then (False, xnor)
          else (True, xor)

        stage1 :: BitVector 8
        stage1 = bitCoerce (scanr1 op (bitCoerce d))

        stage2 :: BitVector 8
        (tag2, stage2, acc') =
          let pop1 = popCountBV stage1
              popDiff :: Signed 4
              popDiff = 2 * (bitCoerce pop1 - 4)
              (invert, acc')
                | acc == 0 || pop1 == 4
                = (complement tag1, if tag1 then popDiff else negate popDiff)
                | acc > 0 && pop1 > 4 || acc < 0 && pop1 < 4
                = (True, bool 0 2 tag1 - popDiff)
                | otherwise
                = (False, bool (-2) 0 tag1 + popDiff)
          in  (invert, if invert then complement stage1 else stage1, acc')
    in  ( acc + acc'
        , bitCoerce (boolToBit tag2 :> boolToBit tag1 :> bitCoerce stage2)
        )

tmdsDecode :: BitVector 10 -> TMDSWord
tmdsDecode = \case
  0b1101010100 -> Control 0b00
  0b0010101011 -> Control 0b01
  0b0101010100 -> Control 0b10
  0b1010101011 -> Control 0b11
  (bitCoerce -> tag2 :> tag1 :> d) ->
    let d' = case tag2 of
          0 -> d
          1 -> bv2v . complement . v2bv $ d
        op = case tag1 of
          0 -> xnor
          1 -> xor
        s = tail d'
    in  Data . bitCoerce $ zipWith op s (init d') :< lsb d'

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

xnor :: Bit -> Bit -> Bit
xnor = complement .: xor

-- >>> scanr1 xnor (bv2v (0b0001 :: BitVector 4))
-- 0 :> 1 :> 0 :> 1 :> Nil
scanr1 f (xs :< x) = scanr f x xs

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)
