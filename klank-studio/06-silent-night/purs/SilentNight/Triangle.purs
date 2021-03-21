module Klank.Studio.SilentNight.Triangle where

import Prelude
import Control.Monad.Reader (ask, asks)
import Data.Array as A
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D3, d0, d1, d2)
import Data.Vec (Vec)
import Data.Vec as V
import FRP.Behavior.Audio (defaultParam, gain_', playBufT_)
import Klank.Studio.SilentNight.Optics (AccumulatorGetter, AccumulatorSetter, BegTimeGetter, BegTimeSetter, EndTimeGetter, EndTimeSetter, AMark, amark, getBegTime, getEndTime, getInter, setBegTime, setEndTime, setInter)
import Klank.Studio.SilentNight.Types (AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (MusicM)
import Klank.Studio.SilentNight.Util (bb01, boundPlayer, boundedEffect, calcSlope, crotchet, musicalInfoToTime, pure2, roundUpTimeToNextMeasure, standardEndTimeBleed)
import Type.Proxy (Proxy(..))

data TrianglePos
  = TriangleTop
  | TriangleLeft
  | TriangleRight

derive instance genericTrianglePos :: Generic TrianglePos _

instance showTrianglePos :: Show TrianglePos where
  show s = genericShow s

triangleSound :: Number -> TrianglePos -> Maybe Number -> Number -> MusicM AudioListD2
triangleSound gn tp dimT stT = do
  { time } <- ask
  let
    nm = case tp of
      TriangleTop -> "sb43"
      TriangleLeft -> "sb38"
      TriangleRight -> "sb33"

    bo =
      defaultParam
        { param = 1.0
        , timeOffset = max 0.0 $ stT - time
        }
  boundPlayer stT 3.0
    ( pure2
        $ gain_' ("gain" <> nm <> show stT)
            ( case dimT of
                Nothing -> gn
                Just x -> bb01 (calcSlope x 1.0 (x + crotchet * 6.0) 0.0 time)
            )
            (playBufT_ ("buffer" <> nm <> show stT) nm bo)
    )

triangle' :: Number -> MusicM AudioListD2
triangle' btm = do
  { time, musicalInfo } <- ask
  v <- asks getTriangleVector
  let
    mmm1 = musicalInfo { measure = musicalInfo.measure `mod` 1 }

    mim = musicalInfo.measure

    ffm = roundUpTimeToNextMeasure btm

    top = join $ flip V.index d0 <$> v

    left = join $ flip V.index d1 <$> v

    right = join $ flip V.index d2 <$> v

    tsnd = triangleSound (bb01 (calcSlope (btm + (0.0)) 0.0 (btm + (2.0)) 1.0 time))

    curriedTop = tsnd TriangleTop top

    curriedLeft = tsnd TriangleLeft left

    curriedRight = tsnd TriangleRight right

    curriedFirst = curriedLeft

    curriedSecond = curriedTop

    curriedThird = curriedRight
  fold <$> sequence (join $ map (\i -> map (\(Tuple f n) -> f $ musicalInfoToTime { measure: ffm.measure + i, beat: n }) [ Tuple curriedFirst 0.0, Tuple curriedSecond 1.0, Tuple curriedThird 2.0 ]) (A.range 0 30))

triangle :: MusicM AudioListD2
triangle = boundedEffect "triangle" getTriangleBegTime getTriangleEndTime triangle'

triangleLens = amark <<< prop (Proxy :: Proxy "triangle")

triangleEndTimeBleed = standardEndTimeBleed :: Number

getTriangleBegTime = getBegTime triangleLens :: BegTimeGetter

getTriangleVector = getInter triangleLens :: AccumulatorGetter (Vec D3 (Maybe Number))

getTriangleEndTime = getEndTime triangleLens :: EndTimeGetter

setTriangleBegTime = setBegTime triangleLens :: BegTimeSetter

setTriangleVector = setInter triangleLens :: AccumulatorSetter (Vec D3 (Maybe Number))

setTriangleEndTime = setEndTime triangleLens :: EndTimeSetter
