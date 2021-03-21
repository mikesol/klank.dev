module Klank.Studio.SilentNight.Motion where

import Prelude
import Control.Monad.Reader (ask, asks)
import Data.Array as A
import Data.Either (Either, either)
import Data.Lens.Record (prop)
import Data.List ((:), List(..))
import Data.Maybe (fromMaybe)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..), snd)
import FRP.Behavior.Audio (gain_, gain_', loopBuf_)
import Graphics.Drawing (Point)
import Graphics.Painting (Painting, circle, fillColor, filled)
import Klank.Studio.SilentNight.Actionable (doingAction)
import Klank.Studio.SilentNight.Optics (AccumulatorSetter, BegTimeGetter, BegTimeSetter, EndTimeGetter, EndTimeSetter, amark, getBegTime, getEndTime, getInter, setBegTime, setEndTime, setInter)
import Klank.Studio.SilentNight.Types (AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (Activity(..), MusicM, PlayerEvent, SilentNightAccumulator, SilentNightPlayerT)
import Klank.Studio.SilentNight.Types.Canvas (MakeCanvasT)
import Klank.Studio.SilentNight.Util (bb01, boundedEffect, calcSlope, mouseOrBust, pure2, sqToRect, standardEndTimeBleed, standardIntro, standardOutro, whiteRGBA)
import Type.Proxy (Proxy(..))

motionNormal = 12.0 :: Number

motion' :: Number -> MusicM AudioListD2
motion' st = do
  { x, y } <- fromMaybe { x: 0.0, y: 0.0 } <$> asks getMotionPos
  { time } <- ask
  let
    introB = st + standardIntro

    vol
      | time < introB = bb01 $ calcSlope st 0.0 introB 1.0 time
      | stopPt <- introB + motionNormal
      , time >= stopPt = bb01 $ calcSlope stopPt 1.0 (stopPt + standardOutro) 0.0 time
      | otherwise = 1.0
  pure2
    $ gain_ ("motionswell") vol
        ( gain_' ("motionGain") (1.0 - x) (loopBuf_ ("motionBuffer") "square5" 1.0 0.0 0.0)
            :| gain_' ("motionWindGain") (1.0 - y) (loopBuf_ ("motionWindBuffer") "motion" 1.0 0.0 0.0)
            : Nil
        )

maxMotionVelocity = 17.0 :: Number -- really closer to 30

windGainMultiplier = 2.6 :: Number

minMotionVelocity = 0.0 :: Number

motion :: MusicM AudioListD2
motion = boundedEffect "motion" getMotionBegTime getMotionEndTime motion'

motionEndTimeBleed = standardEndTimeBleed :: Number

motionLens = amark <<< prop (Proxy :: Proxy "motion")

getMotionBegTime = getBegTime motionLens :: BegTimeGetter

getMotionPos = getInter motionLens

getMotionEndTime = getEndTime motionLens :: EndTimeGetter

setMotionBegTime = setBegTime motionLens :: BegTimeSetter

setMotionPos = setInter motionLens :: AccumulatorSetter Point

setMotionEndTime = setEndTime motionLens :: EndTimeSetter

needsToFollow :: forall a b. Number -> Number -> Number -> SilentNightAccumulator -> Either a b -> Boolean
needsToFollow xp yp cw acc =
  either
    ( \_ ->
        doingAction
          acc
          (sqToRect xp yp cw)
    )
    (const false)

needsToStopFollowing :: forall a b. SilentNightAccumulator -> Either a b -> Boolean
needsToStopFollowing acc =
  either
    (const false)
    (const $ not acc.inClick)

calibrateX :: SilentNightAccumulator -> Point -> Number
calibrateX acc v = (mouseOrBust acc.mousePosition).x - v.x

calibrateY :: SilentNightAccumulator -> Point -> Number
calibrateY acc v = (mouseOrBust acc.mousePosition).y - v.y

eix :: Number -> SilentNightAccumulator -> Either Point (Tuple Point Point) -> Number
eix w acc = either (\v -> w * v.x) (calibrateX acc <<< snd)

eiy :: Number -> SilentNightAccumulator -> Either Point (Tuple Point Point) -> Number
eiy h acc = either (\v -> h * v.y) (calibrateY acc <<< snd)

motionMaker :: PlayerEvent -> Number -> Number -> Number -> SilentNightAccumulator -> SilentNightPlayerT -> Painting -> Number -> MakeCanvasT
motionMaker newM xp yp cw acc i instr time =
  pure
    $ Tuple
        ( acc
            { activity =
              SilentNightPlayer
                ( i
                    { playerEvents = [ newM ] <> A.drop 1 i.playerEvents
                    }
                )
            }
        )
        ( instr
            <> filled
                ( fillColor
                    ( whiteRGBA
                        ( if time < i.eventStart + standardIntro + motionNormal then
                            1.0
                          else
                            calcSlope (i.eventStart + standardIntro + motionNormal) 1.0 (i.eventStart + standardIntro + motionNormal + standardOutro) 0.0 time
                        )
                    )
                )
                (circle xp yp cw)
        )
