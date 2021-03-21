module Klank.Studio.E2020 where

import Prelude
import Color (rgb)
import Data.Array (index, length, range)
import Data.Foldable (fold)
import Data.Int (floor, toNumber)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AV(..), AudioParameter, AudioUnit, CanvasInfo(..), evalPiecewise, gainT_, gain_', panner_, playBufWithOffset_, runInBrowser, speaker)
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Klank.Dev.Util (makeBuffersKeepingCache)
import Klank.Weblib.Studio as Studio
import Math (pow, sin, (%))
import Type.Klank.Dev (Klank, defaultEngineInfo, klank)

tempo = 90.0 :: Number

q = 60.0 / tempo :: Number

_8 = 60.0 / (2.0 * tempo) :: Number

_16 = 60.0 / (4.0 * tempo) :: Number

_32 = 60.0 / (8.0 * tempo) :: Number

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

boundPlayer :: forall a. Number -> (Number -> List a) -> Number -> List a
boundPlayer len a time = if (time) + kr >= 0.0 && time < (len) then a time else Nil

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

ep :: Int -> String -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
ep i' tag len rate os =
  let
    i = toNumber i'
  in
    boundPlayer len
      ( \t ->
          pure
            ( gainT_
                (tag <> "gain")
                (epwf [ Tuple 0.0 1.0, Tuple (len - 0.03) 1.0, Tuple len 0.0 ] t)
                ( (panner_ "p_cnn" (sin i) (playBufWithOffset_ (tag <> "buf_e") "election" 1.0 os))
                    :| ( ( if ( i' `mod` 16 /= 2
                              && i' `mod` 16
                              /= 5
                              && i' `mod` 16
                              /= 9
                              && i' `mod` 16
                              /= 14
                          ) then
                            (pure $ gain_' (tag <> "gt_e") (0.7 - 0.125 * 0.7 * (i % 8.0)) (playBufWithOffset_ (tag <> "buf_p") "nice" rate 0.0))
                          else
                            Nil
                        )
                          <> ( if ( true
                              ) then
                                (pure $ gain_' (tag <> "gt_z") 0.2 (playBufWithOffset_ (tag <> "buf_z") ("b" <> show (i' `mod` 8)) rate 0.0))
                              else
                                Nil
                            )
                      )
                )
            )
      )

xfy :: Int -> Int
xfy i = floor (8.0 * (0.5 + 0.5 * (sin $ toNumber i)))

rm :: Int -> Int -> Number -> Number -> Painting
rm c r w h =
  let
    rw = w / (toNumber c)

    rh = h / (toNumber r)
  in
    fold (join (map (\i -> let ix = xfy i in map (\j -> let jx = xfy j in filled (fillColor (if (((j * r) + i) `mod` 2) == 0 then (rgb (255 - 10 * (ix + jx)) (ix * 8) (jx * 4)) else (rgb (jx * 3) (ix * 9) (255 - 10 * (ix + jx))))) (rectangle (toNumber i * rw) (toNumber j * rh) (rw) (rh))) (range 0 (r - 1))) (range 0 (c - 1))))

flen = 189.0 :: Number

scene ::
  Unit ->
  CanvasInfo ->
  Number ->
  Behavior (AV D2 Unit)
scene _ ci time =
  pure
    $ AV
        { audio:
            Just
              ( speaker
                  ( zero
                      :| ( fold
                            ( map (\f -> f time)
                                ( (fac 0.0)
                                    <> (fac (q * 48.0))
                                )
                            )
                        )
                  )
              )
        , visual:
            Just
              { painting:
                  \_ ->
                    (fold <<< fold)
                      ( map (\f -> f time)
                          ( (fax ci 0.0)
                              <> (fax ci (q * 48.0))
                          )
                      )
              , words: Nil
              }
        , accumulator: unit
        }

st :: Array Number
st = [ 0.26, 0.88, 2.38, 3.12, 3.80, 4.82, 5.68, 7.14 ]

lst = length st :: Int

fac :: Number -> Array (Number -> List (AudioUnit D2))
fac offset =
  ( map
      ( \i ->
          let
            n = toNumber i
          in
            atT (offset + n * q) $ ep i ("a" <> show i) (q + 0.04) (1.0 + 0.1 * sin n) (fromMaybe 0.0 $ index st (i `mod` lst))
      )
      (range 0 15)
  )
    <> ( map
          ( \i ->
              let
                n = toNumber i
              in
                atT (offset + (q * 16.0) + n * _8) $ ep i ("a" <> show i) (_8 + 0.04) (1.0 + 0.1 * sin n) (fromMaybe 0.0 $ index st (i `mod` lst))
          )
          (range 0 31)
      )
    <> ( map
          ( \i ->
              let
                n = toNumber i
              in
                atT (offset + (q * 32.0) + n * _16) $ ep i ("a" <> show i) (_16 + 0.04) (1.0 + 0.1 * sin n) (fromMaybe 0.0 $ index st (i `mod` lst))
          )
          (range 0 31)
      )
    <> ( map
          ( \i ->
              let
                n = toNumber i
              in
                atT (offset + (q * 40.0) + n * _32) $ ep i ("a" <> show i) (_32 * 1.5 + 0.04) (1.0 + 0.1 * sin n) (fromMaybe 0.0 $ index st (i `mod` lst))
          )
          (range 0 63)
      )

shuf = [ 1, 3, 5, 6, 7, 8, 9, 10 ] :: Array Int

lshuf = length shuf :: Int

shufx = [ 3, 5, 3, 4, 3, 5, 3, 4, 5, 4, 3 ] :: Array Int

lshufx = length shufx :: Int

fromShuf :: Int -> Int
fromShuf i = fromMaybe 3 (shuf `index` (i `mod` lshuf))

fromShufx :: Int -> Int
fromShufx i = fromMaybe 3 (shufx `index` (i `mod` lshufx))

fax :: CanvasInfo -> Number -> Array (Number -> List Painting)
fax (CanvasInfo ci) offset =
  ( map
      ( \i ->
          let
            n = toNumber i
          in
            atT (offset + n * q) $ boundPlayer q (\_ -> pure $ (rm (fromShuf i) (fromShuf (16 - i)) ci.w ci.h))
      )
      (range 0 48)
  )
    <> ( map
          ( \i ->
              let
                n = toNumber i
              in
                atT (offset + (q * 16.0) + n * _8) $ boundPlayer q (\_ -> pure $ (rm (fromShufx i) (fromShufx (16 - i)) ci.w ci.h))
          )
          (range 0 63)
      )

playMe :: Klank
playMe =
  klank
    { run = runInBrowser scene
    , buffers =
      makeBuffersKeepingCache 20
        [ Tuple "election" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/electionShort.ogg"
        , Tuple "b0" "https://freesound.org/data/previews/48/48527_100432-lq.mp3"
        , Tuple "b1" "https://freesound.org/data/previews/332/332643_5261755-lq.mp3"
        , Tuple "b2" "https://freesound.org/data/previews/422/422272_6672220-lq.mp3"
        , Tuple "b3" "https://freesound.org/data/previews/116/116830_2085593-lq.mp3"
        , Tuple "b4" "https://freesound.org/data/previews/128/128535_2345419-lq.mp3"
        , Tuple "b5" "https://freesound.org/data/previews/185/185079_3342732-lq.mp3"
        , Tuple "b6" "https://freesound.org/data/previews/121/121072_2193266-lq.mp3"
        , Tuple "b7" "https://freesound.org/data/previews/23/23563_150583-lq.mp3"
        , Tuple "b8" "https://freesound.org/data/previews/464/464227_1431924-lq.mp3"
        , Tuple "b9" "https://freesound.org/data/previews/26/26352_116671-lq.mp3"
        , Tuple "b10" "https://freesound.org/data/previews/0/907_838-lq.mp3"
        , Tuple "b11" "https://freesound.org/data/previews/321/321790_4701691-lq.mp3"
        , Tuple "nice" "https://freesound.org/data/previews/91/91781_1491361-lq.mp3"
        ]
    }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (Studio.component (pure playMe)) unit body
