module Klank.Studio.SilentNight.Gear where

import Prelude
import Control.Monad.Reader (ask, asks)
import Data.Foldable (foldl, fold)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, maybe)
import Data.Traversable (sequence)
import Data.Typelevel.Num (D4, d0, d1, d2, d3)
import Data.Vec (Vec)
import Data.Array as A
import Data.Vec as V
import FRP.Behavior.Audio (gain_', loopBuf_, pannerMono_)
import Klank.Studio.SilentNight.Optics (AccumulatorGetter, AccumulatorSetter, BegTimeGetter, BegTimeSetter, EndTimeGetter, EndTimeSetter, amark, getBegTime, getEndTime, getInter, setBegTime, setEndTime, setInter)
import Klank.Studio.SilentNight.Types (AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (MusicM)
import Klank.Studio.SilentNight.Util (bindBetween, boundPlayer, boundedEffect, calcSlope, pure2, standardEndTimeBleed)
import Math (pow, sin, pi)
import Type.Proxy (Proxy(..))

data GearPos
  = GearOne
  | GearTwo
  | GearThree
  | GearFour

gearDir0 = true :: Boolean

gearDir1 = false :: Boolean

gearDir2 = true :: Boolean

gearDir3 = false :: Boolean

gearSpinF :: Number -> Number
gearSpinF t = 0.8 * t + (0.2 * (t `pow` 2.0)) -- t `pow` (1.3 + 0.3 * sin (0.5 * pi * t))

gearStay = 4.0 :: Number

gp2s :: GearPos -> String
gp2s GearOne = "gear1"

gp2s GearTwo = "gear2"

gp2s GearThree = "gear3"

gp2s GearFour = "gear4"

gp2r :: GearPos -> Number
gp2r GearOne = 0.9

gp2r GearTwo = 1.0

gp2r GearThree = 1.3

gp2r GearFour = 1.6

gp2pr :: GearPos -> Number
gp2pr GearOne = 0.2

gp2pr GearTwo = 0.3

gp2pr GearThree = 0.4

gp2pr GearFour = 0.5

gearMaxVol = 0.7 :: Number

gearSound :: Maybe Number -> GearPos -> Number -> MusicM AudioListD2
gearSound fadeOutT pos startT =
  let
    gn = gp2s pos
  in
    do
      { time } <- ask
      pure2 (pannerMono_ ("gainPM" <> gn) (sin $ (time - startT) * pi * gp2pr pos) (gain_' ("gain" <> gn) (maybe gearMaxVol (\fot -> bindBetween 0.0 gearMaxVol (calcSlope fot gearMaxVol (fot + 4.0) 0.0 time)) fadeOutT) (loopBuf_ ("buffer_" <> gn) "gearBowl" (gp2r pos) 4.0 15.0)))

gearsP :: Maybe Number -> GearPos -> Number -> MusicM AudioListD2
gearsP fadeOutT pos startT = boundPlayer startT 10000.0 (gearSound fadeOutT pos startT)

gears' :: Number -> MusicM AudioListD2
gears' btm = do
  v <- asks getGearsVector
  let
    fadeOutStarts = (foldl max 0.0) <$> (join (sequence <$> v))

    gp = gearsP fadeOutStarts

    one = (gp GearOne) <$> (join $ flip V.index d0 <$> v)

    two = (gp GearTwo) <$> (join $ flip V.index d1 <$> v)

    three = (gp GearThree) <$> (join $ flip V.index d2 <$> v)

    four = (gp GearFour) <$> (join $ flip V.index d3 <$> v)
  fold
    <$> sequence
        ( A.catMaybes
            [ one, two, three, four ]
        )

gears :: MusicM AudioListD2
gears = boundedEffect "gears" getGearsBegTime getGearsEndTime gears'

-- make a bit longer just in case for ringing
gearsEndTimeBleed = standardEndTimeBleed + 10.0 :: Number

gearsLens = amark <<< prop (Proxy :: Proxy "gears")

getGearsBegTime = getBegTime gearsLens :: BegTimeGetter

getGearsVector = getInter gearsLens :: AccumulatorGetter (Vec D4 (Maybe Number))

getGearsEndTime = getEndTime gearsLens :: EndTimeGetter

setGearsBegTime = setBegTime gearsLens :: BegTimeSetter

setGearsVector = setInter gearsLens :: AccumulatorSetter (Vec D4 (Maybe Number))

setGearsEndTime = setEndTime gearsLens :: EndTimeSetter
