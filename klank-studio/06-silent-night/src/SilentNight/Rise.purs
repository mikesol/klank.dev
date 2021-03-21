module Klank.Studio.SilentNight.Rise where

import Prelude
import Control.Monad.Reader (ask, asks)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, maybe)
import Data.Typelevel.Num (d0, d1, d2, d3, d4, d5, D6)
import Data.Vec (Vec)
import Data.Vec as V
import FRP.Behavior.Audio (gain_', highpass_, playBuf_)
import Klank.Studio.SilentNight.Optics (AccumulatorGetter, AccumulatorSetter, BegTimeGetter, BegTimeSetter, EndTimeGetter, EndTimeSetter, amark, getBegTime, getEndTime, getInter, setBegTime, setEndTime, setInter)
import Klank.Studio.SilentNight.Types (AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (MusicM)
import Klank.Studio.SilentNight.Util (bb01, bindAndSlope, boundedEffect, calcSlope, pure2, standardEndTimeBleed, standardIntro, standardOutro)
import Type.Proxy (Proxy(..))

riseNormal = 12.0 :: Number

riseF :: Number -> Maybe Number -> Number -> Number -> Number
riseF startT didStop time freq = (maybe 0.0 (\n -> bindAndSlope n (0.0) (n + 2.0) 1000.0 time) didStop) + freq

rise' :: Number -> MusicM AudioListD2
rise' btm = do
  { time } <- ask
  v <- asks getRiseVector
  let
    one = riseF btm (join $ flip V.index d0 <$> v) time

    two = riseF btm (join $ flip V.index d1 <$> v) time

    three = riseF btm (join $ flip V.index d2 <$> v) time

    four = riseF btm (join $ flip V.index d3 <$> v) time

    five = riseF btm (join $ flip V.index d4 <$> v) time

    six = riseF btm (join $ flip V.index d5 <$> v) time

    riseEvl = (one <<< two <<< three <<< four <<< five <<< six) 500.0

    gnNow
      | time < btm + standardIntro = bindAndSlope btm 0.0 (btm + standardIntro) 1.0 time
      | time >= btm + standardIntro + riseNormal = bb01 $ calcSlope (btm + standardIntro + riseNormal) 1.0 (btm + standardIntro + riseNormal + standardOutro) 0.0 time
      | otherwise = 1.0
  pure2 (highpass_ "riseHPF" riseEvl 3.0 (gain_' ("riseGain") (gnNow) (playBuf_ "riseBuf" "rise" 1.0)))

rise :: MusicM AudioListD2
rise = boundedEffect "rise" getRiseBegTime getRiseEndTime rise'

riseEndTimeBleed = standardEndTimeBleed :: Number

riseLens = amark <<< prop (Proxy :: Proxy "rise")

getRiseBegTime = getBegTime riseLens :: BegTimeGetter

getRiseVector = getInter riseLens :: AccumulatorGetter (Vec D6 (Maybe Number))

getRiseEndTime = getEndTime riseLens :: EndTimeGetter

setRiseBegTime = setBegTime riseLens :: BegTimeSetter

setRiseVector = setInter riseLens :: AccumulatorSetter (Vec D6 (Maybe Number))

setRiseEndTime = setEndTime riseLens :: EndTimeSetter

riseXP = [ 1.0 / 12.0, 3.0 / 12.0, 5.0 / 12.0, 7.0 / 12.0, 9.0 / 12.0, 11.0 / 12.0 ] :: Array Number
