module Klank.Studio.SilentNight.Util where

import Prelude
import Color (Color, rgba)
import Control.Monad.Reader (ask)
import Data.Foldable (class Foldable, foldl)
import Data.Int (floor, toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.NonEmpty ((:|), NonEmpty)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple)
import Data.Typelevel.Num (D2)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, EngineInfo, defaultParam, evalPiecewise, gain_, pannerMono_, periodicOsc_, playBufT_)
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Point)
import Klank.Studio.SilentNight.Types.Accumulator (MusicM, AudioEnv)
import Klank.Studio.SilentNight.Types (MusicalInfo, AudioListD2)
import Math (cos, pow, sin, (%))
import Type.Klank.Dev (defaultEngineInfo)

pure2 :: forall m0 m1 a. Applicative m0 => Applicative m1 => a -> m0 (m1 a)
pure2 = pure <<< pure

metronomeClick :: Number -> Number -> MusicM AudioListD2
metronomeClick s gp = do
  let
    bo = beatGapToStartOffsetAsParam s gp
  pure2
    $ playBufT_ ("buffer" <> show s) "metronome-wb" bo

roundUpTimeToNextMeasure :: Number -> MusicalInfo
roundUpTimeToNextMeasure t =
  let
    asM = t * tempo / (3.0 * 60.0)

    fl = floor asM
  in
    { measure: fl + if toNumber fl == asM then 0 else 1, beat: 0.0 }

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

musicalInfoToTime :: MusicalInfo -> Number
musicalInfoToTime { measure, beat } = (toNumber measure * 3.0 + beat) * 60.0 / tempo

timeToMusicalInfo :: Number -> MusicalInfo
timeToMusicalInfo t =
  let
    beats = tempo * t / 60.0

    measure = floor (beats / 3.0)

    beat = beats - ((toNumber measure) * 3.0)
  in
    { measure, beat }

silentNightEngineInfo =
  defaultEngineInfo
    { msBetweenSamples = 40
    , msBetweenPings = 35
    } ::
    EngineInfo

tempo = 72.0 :: Number

measureDur = 180.0 / tempo :: Number

crotchet = 60.0 / tempo :: Number

quaver = 30.0 / tempo :: Number

semiquaver = 15.0 / tempo :: Number

preCodaInMeasures = 3.0 :: Number -- really two, but slower

introInMeasures = 4.0 :: Number

silentNightInMeasures = 26.0 :: Number -- includes 2m transition

silentNightInBeats = silentNightInMeasures * 3.0 :: Number

silentNightInBeatsAsTime = silentNightInBeats * 60.0 / tempo :: Number

pieceInMeasures = measureDur * (silentNightInMeasures + silentNightInMeasures + silentNightInMeasures) :: Number

kr = (toNumber silentNightEngineInfo.msBetweenSamples) / 1000.0 :: Number

krt = kr * tempo / 60.0 :: Number

nkrt = -1.0 * krt :: Number

codaStartsMI = { measure: 82, beat: 0.0 } :: MusicalInfo

codaStarts = musicalInfoToTime codaStartsMI :: Number

fadeIn = 2.0 :: Number

stay = 3.0 :: Number

fadeOut = 2.0 :: Number

dark = 0.5 :: Number

darknessFadeIn = 0.0 :: Number

silentNightFadeIn = darknessFadeIn + fadeIn :: Number

silentNightStay = silentNightFadeIn + fadeIn :: Number

silentNightFadeOut = silentNightStay + stay :: Number

silentNightDark = silentNightFadeOut + fadeOut :: Number

instructionFadeIn = silentNightDark + dark :: Number

instructionStay = instructionFadeIn + fadeIn :: Number

instructionFadeOut = instructionStay + stay :: Number

instructionDark = instructionFadeOut + fadeOut :: Number

standardIntro = 1.4 :: Number

standardOutro = 4.0 :: Number

standardPress = 0.5 :: Number

whiteRGBA = rgba 255 255 255 :: Number -> Color

circleDivisor = 18.0 :: Number

bindAndSlope :: Number -> Number -> Number -> Number -> Number -> Number
bindAndSlope x0 y0 x1 y1 x = bindBetween y0 y1 $ calcSlope x0 y0 x1 y1 x

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b

conv440 :: Number -> Number
conv440 i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

pythag :: Number -> Number -> Number
pythag x y = ((x `pow` 2.0) + (y `pow` 2.0)) `pow` 0.5

conv1 :: Number -> Number
conv1 i = 1.0 * (2.0 `pow` ((i - 0.0) / 12.0))

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

loopT :: forall a. Number -> (Number -> a) -> (Number -> a)
loopT t = lcmap (_ % t)

foldOverTime :: forall a b f. Foldable f => Applicative f => Monoid (f b) => (Number -> a -> b) -> (a -> Number) -> f a -> f b
foldOverTime trans getn = _.acc <<< foldl (\{ acc, clen } a -> { acc: acc <> (pure $ trans clen a), clen: clen + getn a }) { acc: mempty, clen: 0.0 }

boundPlayer :: Number -> Number -> MusicM AudioListD2 -> MusicM AudioListD2
boundPlayer st len a = do
  { time } <- ask
  if time + kr >= st && time < (st + len) then a else mempty

skewedTriangle01 :: Number -> Number -> Number -> Number
skewedTriangle01 os len = lcmap (_ % len) go
  where
  go time
    | time < (len * os) = (time / (len * os))
    | otherwise = (len - time) / (len * (1.0 - os))

triangle01 :: Number -> Number -> Number
triangle01 = skewedTriangle01 0.5

toNel :: forall s. Semiring s => List s -> NonEmpty List s
toNel Nil = zero :| Nil

toNel (h : t) = h :| t

wah :: String -> String -> Number -> Number -> Int -> List Number -> (Number -> Number) -> (Number -> Number) -> Number -> AudioUnit D2
wah tag pwave bgt edt nwahs pitches gnF panF time = let len = edt - bgt in pannerMono_ (tag <> "WahPanner") (panF time) (gain_ (tag <> "WahGain") (if time >= edt then 0.0 else (gnF time * (triangle01 (len / (toNumber nwahs)) time) / (toNumber $ L.length pitches))) (toNel (L.mapWithIndex (\i p -> periodicOsc_ (tag <> show i) pwave (conv440 p)) pitches)))

bindBetween :: Number -> Number -> Number -> Number
bindBetween mn mx n = max mn (min mx n)

bb01 :: Number -> Number
bb01 = bindBetween 0.0 1.0

measureMinus :: MusicalInfo -> Int -> MusicalInfo
measureMinus { measure, beat } i = { measure: measure - i, beat }

hitsTargetInSecond :: Number -> Number
hitsTargetInSecond tg = tg `pow` kr

targetForOneHalf = hitsTargetInSecond 0.5 :: Number

standardEndTimeBleed = 5.0 :: Number

sinp :: Number -> Number -> Number
sinp v p = (v * (((sin (p)) * 0.5 + 0.5) * 0.7 + 0.15))

cosp :: Number -> Number -> Number
cosp v p = (v * (((cos (p)) * 0.5 + 0.5) * 0.7 + 0.15))

sqToRect :: Number -> Number -> Number -> Rectangle
sqToRect x y r = { x: x - r, y: y - r, width: 2.0 * r, height: 2.0 * r }

mouseOrBust :: Maybe Point -> Point
mouseOrBust = fromMaybe { x: 0.0, y: 0.0 }

boundedEffect :: String -> (AudioEnv -> Maybe Number) -> (AudioEnv -> Maybe Number) -> (Number -> MusicM AudioListD2) -> MusicM AudioListD2
boundedEffect tag begt endt a = do
  audEnv <- ask
  let
    bt = begt audEnv
  maybe mempty
    ( \x -> case endt audEnv of
        Nothing -> a x
        Just et
          | audEnv.time < et -> a x
          | otherwise -> mempty
    )
    bt

miGap :: MusicalInfo -> MusicalInfo -> Maybe Number
miGap target atNow =
  let
    targetB = toNumber target.measure * 3.0 + target.beat

    atNowB = toNumber atNow.measure * 3.0 + atNow.beat

    diffB = atNowB - targetB
  in
    if diffB <= nkrt then Nothing else Just diffB

miGapB :: MusicalInfo -> MusicalInfo -> Boolean
miGapB target now = isJust $ miGap target now

infixl 4 miGap as |<

infixl 4 miGapB as ||<

beatToTime :: Number -> Number
beatToTime b = (b * 60.0) / tempo

beatGapToStartOffset :: Number -> Number
beatGapToStartOffset n = max 0.0 $ kr - beatToTime n

beatGapToStartOffsetAsParam :: Number -> Number -> AudioParameter
beatGapToStartOffsetAsParam param n =
  defaultParam
    { param = param
    , timeOffset = beatGapToStartOffset n
    }
