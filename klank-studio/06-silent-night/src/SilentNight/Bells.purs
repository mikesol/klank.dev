module Klank.Studio.SilentNight.Bells where

import Prelude
import Data.List (List(..), fold, (:))
import Math ((%))
import Control.Monad.Reader (ask, asks)
import Data.Array as A
import Data.Int (toNumber)
import Data.Lens.Record (prop)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import FRP.Behavior.Audio (playBuf_)
import Graphics.Painting (Painting, circle, filled, fillColor)
import Klank.Studio.SilentNight.Actionable (doAction)
import Klank.Studio.SilentNight.Optics (AccumulatorGetter, AccumulatorSetter, BegTimeGetter, BegTimeSetter, EndTimeGetter, EndTimeSetter, amark, getBegTime, getEndTime, getInter, setBegTime, setEndTime, setInter)
import Klank.Studio.SilentNight.Types (AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (Activity(..), MusicM, PlayerEvent(..), SilentNightAccumulator, SilentNightPlayerT)
import Klank.Studio.SilentNight.Types.Canvas (MakeCanvasT, MakeCanvas)
import Klank.Studio.SilentNight.Util (boundPlayer, boundedEffect, calcSlope, pure2, sqToRect, standardEndTimeBleed, standardIntro, standardOutro, timeToMusicalInfo, whiteRGBA)
import Type.Proxy (Proxy(..))

bellsInternal = standardIntro + bellsNormal :: Number

bellsFaded = bellsInternal + standardOutro :: Number

bellsNormal = 18.0 :: Number

bellsRecurser :: MakeCanvas -> SilentNightPlayerT -> Number -> Number -> SilentNightAccumulator -> Number -> Number -> List (List Number) -> MakeCanvasT
bellsRecurser makeCanvas i w h acc startT time l = go 0 Nil (if acc.inClick then l else Nil)
  where
  op = max 0.0 (min 1.0 (calcSlope (i.eventStart + bellsInternal) 1.0 (i.eventStart + bellsFaded) 0.0 time))

  notNow Nil = true

  notNow (a : b) = a /= time

  go z hd Nil =
    pure
      $ Tuple (setBellsList l acc)
          ( makeBells w h
              ( map
                  ( \tl ->
                      Tuple
                        ( case tl of
                            Nil -> 1.0
                            (a : b) -> if time - a < 0.25 then (calcSlope a 1.0 (a + 0.25) 1.13 time) else if time - a < 0.5 then (calcSlope (a + 0.25) 1.13 (a + 0.5) 1.0 time) else 1.0
                        )
                        op
                  )
                  (A.fromFoldable l)
              )
          )

  go z hd (a : b) =
    if notNow a
      && doAction
          acc
          (sqToRect (bellX z * w) (bellY z * h) ((min w h) / 24.0)) then
      makeCanvas
        ( acc
            { activity =
              SilentNightPlayer
                ( i
                    { playerEvents = [ Bells (hd <> (pure $ time : a) <> b) ] <> A.drop 1 i.playerEvents
                    }
                )
            }
        )
        time
    else
      go (z + 1) (hd <> pure a) b

bellX :: Int -> Number
bellX i = ((toNumber (i `mod` 6)) * 2.0 + 1.0) / 12.0

bellY :: Int -> Number
bellY i = ((toNumber (i `div` 6)) * 2.0 + 1.0) / 8.0

-- sizeMult opacity
makeBells :: Number -> Number -> Array (Tuple Number Number) -> Painting
makeBells w h a =
  fold
    ( A.mapWithIndex
        ( \i (Tuple sizeM op) ->
            filled (fillColor (whiteRGBA op))
              (circle (bellX i * w) (bellY i * h) (sizeM * (min w h) / 24.0))
        )
        a
    )

bells' :: Number -> MusicM AudioListD2
bells' begT = do
  { time, mainStarts } <- ask
  case mainStarts of
    Nothing -> mempty
    Just ms -> do
      bellsL <- fromMaybe baseBells <$> asks getBellsList
      let
        placeInPiece = timeToMusicalInfo (time - ms)

        twoBeatsAhead
          | placeInPiece.beat >= 1.0 = { measure: placeInPiece.measure + 1, beat: (placeInPiece.beat + 2.0) % 3.0 }
          | otherwise = { measure: placeInPiece.measure, beat: placeInPiece.beat + 2.0 }
      bellAudio bellsL

singleBell' :: Number -> Int -> MusicM AudioListD2
singleBell' onset bi = pure2 (playBuf_ (show bi <> show onset) "snow" (1.0 + (toNumber bi) / 8.0))

singleBell :: Int -> List Number -> List (MusicM AudioListD2)
singleBell i a = map (\inc -> boundPlayer inc 2.0 (singleBell' inc i)) a

bellAudio :: List (List Number) -> MusicM AudioListD2
bellAudio l = fold <$> sequence (join $ go 0 l)
  where
  go :: Int -> List (List Number) -> List (List (MusicM AudioListD2))
  go i Nil = mempty

  go i (a : b) = singleBell i a : go (i + 1) b

bells :: MusicM AudioListD2
bells = boundedEffect "bells" getBellsBegTime getBellsEndTime bells'

bellsEndTimeBleed = standardEndTimeBleed :: Number

bellsLens = amark <<< prop (Proxy :: Proxy "bells")

getBellsBegTime = getBegTime bellsLens :: BegTimeGetter

getBellsList = getInter bellsLens :: AccumulatorGetter (List (List Number))

getBellsEndTime = getEndTime bellsLens :: EndTimeGetter

setBellsBegTime = setBegTime bellsLens :: BegTimeSetter

setBellsList = setInter bellsLens :: AccumulatorSetter (List (List Number))

setBellsEndTime = setEndTime bellsLens :: EndTimeSetter

baseBells = L.fromFoldable $ A.replicate 24 Nil :: List (List Number)
