module Klank.Studio.SilentNight.Shrink where

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
import Data.Typelevel.Num (D6, d0, d1, d2, d3, d4, d5)
import Data.Vec (Vec, (+>), empty)
import Data.Vec as V
import Graphics.Painting (circle, fillColor, filled)
import Klank.Studio.SilentNight.Optics (AccumulatorGetter, AccumulatorSetter, BegTimeGetter, BegTimeSetter, EndTimeGetter, EndTimeSetter, amark, getBegTime, getEndTime, getInter, setBegTime, setEndTime, setInter)
import Klank.Studio.SilentNight.Types (AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (Activity(..), MusicM, PlayerEvent(..), SilentNightAccumulator, SilentNightPlayerT)
import Klank.Studio.SilentNight.Types.Canvas (MakeCanvasT)
import Klank.Studio.SilentNight.Util (bb01, boundedEffect, calcSlope, kr, pure2, standardEndTimeBleed, standardIntro, standardOutro, wah, whiteRGBA)
import Math (abs, sin, pi)
import Type.Proxy (Proxy(..))

data ShrinkPos
  = ShrinkOne
  | ShrinkTwo
  | ShrinkThree
  | ShrinkFour
  | ShrinkFive
  | ShrinkSix

derive instance genericShrinkPos :: Generic ShrinkPos _

instance showShrinkPos :: Show ShrinkPos where
  show s = genericShow s

epsilon = 0.01 :: Number

eps :: Number -> Number -> Boolean
eps a b = abs (a - b) < epsilon

targetMaker :: forall a. Vec a Number -> Vec a (Number -> Number)
targetMaker v = map (\n i -> if (eps n i) then i else i + ((n - i) * kr * 1.4)) v

---- shrink vars
targetsOne = targetMaker (0.2 +> 0.3 +> 0.4 +> 0.1 +> 0.07 +> 0.17 +> empty) :: Vec D6 (Number -> Number)

targetsTwo = targetMaker (0.51 +> 0.13 +> 0.39 +> 0.26 +> 0.4 +> 0.04 +> empty) :: Vec D6 (Number -> Number)

targetsThree = targetMaker (0.04 +> 0.47 +> 0.06 +> 0.39 +> 0.1 +> 0.52 +> empty) :: Vec D6 (Number -> Number)

targetsFour = targetMaker (0.13 +> 0.08 +> 0.35 +> 0.43 +> 0.2 +> 0.28 +> empty) :: Vec D6 (Number -> Number)

targetsFive = targetMaker (0.18 +> 0.26 +> 0.5 +> 0.12 +> 0.54 +> 0.3 +> empty) :: Vec D6 (Number -> Number)

targetsSix = targetMaker (0.3 +> 0.05 +> 0.1 +> 0.6 +> 0.3 +> 0.22 +> empty) :: Vec D6 (Number -> Number)

targetsDefault = targetMaker (0.1 +> 0.15 +> 0.6 +> 0.3 +> 0.35 +> 0.05 +> empty) :: Vec D6 (Number -> Number)

shrinkOne = Tuple 0.5 0.6 :: Tuple Number Number

shrinkTwo = Tuple 0.7 0.2 :: Tuple Number Number

shrinkThree = Tuple 0.2 0.8 :: Tuple Number Number

shrinkFour = Tuple 0.3 0.4 :: Tuple Number Number

shrinkFive = Tuple 0.72 0.7 :: Tuple Number Number

shrinkSix = Tuple 0.9 0.8 :: Tuple Number Number

shrinkNormal = 18.0 :: Number

paintShrinks :: Number -> Number -> SilentNightAccumulator -> SilentNightPlayerT -> Vec D6 (Number -> Number) -> Vec D6 Number -> Number -> MakeCanvasT
paintShrinks w h acc i shrinkF curPos time =
  let
    newPos = V.zipWith (\f x -> f x) shrinkF curPos
  in
    pure
      $ ( Tuple
            ( (setShrinkVector newPos acc)
                { activity =
                  SilentNightPlayer
                    ( i
                        { playerEvents = [ Shrink newPos ] <> A.drop 1 i.playerEvents
                        }
                    )
                }
            )
            $ fold
                ( map
                    ( \(Tuple (Tuple xp yp) r) ->
                        filled
                          ( fillColor
                              ( whiteRGBA
                                  ( min 1.0
                                      ( calcSlope
                                          (i.eventStart + standardIntro + shrinkNormal)
                                          1.0
                                          (i.eventStart + standardIntro + shrinkNormal + standardOutro)
                                          0.0
                                          time
                                      )
                                  )
                              )
                          )
                          ( circle
                              (w * xp)
                              (h * yp)
                              (r * (min w h) / 2.0)
                          )
                    )
                    (V.zip (shrinkOne +> shrinkTwo +> shrinkThree +> shrinkFour +> shrinkFive +> shrinkSix +> empty) newPos)
                )
        )

shrink' :: Number -> MusicM AudioListD2
shrink' btm = do
  v' <- asks getShrinkVector
  case v' of
    Nothing -> mempty
    Just v -> do
      let
        one = shrinkP ShrinkOne btm (V.index v d0)

        two = shrinkP ShrinkTwo btm (V.index v d1)

        three = shrinkP ShrinkThree btm (V.index v d2)

        four = shrinkP ShrinkFour btm (V.index v d3)

        five = shrinkP ShrinkFive btm (V.index v d4)

        six = shrinkP ShrinkSix btm (V.index v d5)
      fold
        <$> sequence
            [ one, two, three, four, five, six ]

shrinkL = standardIntro + shrinkNormal + standardOutro :: Number

shrinkP :: ShrinkPos -> Number -> Number -> MusicM AudioListD2
shrinkP rp startT currentShrinkPlace = do
  { time } <- ask
  pure2
    $ wah (show rp) "smooth" startT (startT + shrinkL)
        ( case rp of
            ShrinkOne -> 100
            ShrinkTwo -> 110
            ShrinkThree -> 120
            ShrinkFour -> 130
            ShrinkFive -> 140
            ShrinkSix -> 150
        )
        ( pure
            $ case rp of
                ShrinkOne -> 75.0 -- Eb Bb C Eb F Bb C
                ShrinkTwo -> 82.0
                ShrinkThree -> 84.0
                ShrinkFour -> 87.0
                ShrinkFive -> 89.0
                ShrinkSix -> 91.0
        )
        ( \t ->
            let
              o
                -- elongate a bit
                | t < startT + standardIntro + 5.0 = bb01 (calcSlope startT 0.0 (startT + standardIntro + 5.0) 1.0 t)
                | t >= startT + standardIntro + shrinkNormal = bb01 (calcSlope (startT + standardIntro + shrinkNormal) 1.0 (startT + standardIntro + shrinkNormal + standardOutro) 0.0 t)
                | otherwise = 1.0
            in
              o * currentShrinkPlace * 0.38 -- shrink magic number
        )
        ( \t ->
            sin
              $ ( t
                    + case rp of
                        ShrinkOne -> 0.2
                        ShrinkTwo -> 0.4
                        ShrinkThree -> 0.5
                        ShrinkFour -> 0.6
                        ShrinkFive -> 0.8
                        ShrinkSix -> 0.9
                )
              * pi
              * ( case rp of
                    ShrinkOne -> 0.2
                    ShrinkTwo -> 0.1
                    ShrinkThree -> 0.3
                    ShrinkFour -> 0.4
                    ShrinkFive -> 0.5
                    ShrinkSix -> 0.6
                )
        )
        time

shrink :: MusicM AudioListD2
shrink = boundedEffect "shrink" getShrinkBegTime getShrinkEndTime shrink'

shrinkStart = 0.1 +> 0.15 +> 0.6 +> 0.3 +> 0.35 +> 0.05 +> empty :: Vec D6 Number

shrinkEndTimeBleed = standardEndTimeBleed :: Number

shrinkLens = amark <<< prop (Proxy :: Proxy "shrink")

getShrinkBegTime = getBegTime shrinkLens :: BegTimeGetter

getShrinkVector = getInter shrinkLens :: AccumulatorGetter (Vec D6 Number)

getShrinkEndTime = getEndTime shrinkLens :: EndTimeGetter

setShrinkBegTime = setBegTime shrinkLens :: BegTimeSetter

setShrinkVector = setInter shrinkLens :: AccumulatorSetter (Vec D6 Number)

setShrinkEndTime = setEndTime shrinkLens :: EndTimeSetter
