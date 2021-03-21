module Klank.Studio.SilentNight.Large where

import Prelude
import Control.Monad.Reader (ask, asks)
import Data.Foldable (fold)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (sequence)
import Data.Typelevel.Num (D2)
import FRP.Behavior.Audio (AudioUnit, gain_', loopBuf_, notch_)
import Klank.Studio.SilentNight.Optics (AccumulatorGetter, AccumulatorSetter, BegTimeGetter, BegTimeSetter, EndTimeGetter, EndTimeSetter, amark, getBegTime, getEndTime, getInter, setBegTime, setEndTime, setInter)
import Klank.Studio.SilentNight.Types (AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (MusicM)
import Klank.Studio.SilentNight.Util (bb01, boundedEffect, calcSlope, pure2, standardEndTimeBleed)
import Type.Proxy (Proxy(..))

largeCrossing = 25.0 :: Number

data LargeTrack
  = LgSanta
  | LgBirds
  | LgChimes
  | LgSynth
  | LgCrowd

lt2s :: LargeTrack -> String
lt2s LgBirds = "large-birds"

lt2s LgCrowd = "large-market"

lt2s LgChimes = "large-chimes"

lt2s LgSynth = "large-synth"

lt2s LgSanta = "large-santa"

lg2f :: Int -> LargeTrack -> AudioUnit D2 -> AudioUnit D2
lg2f i LgCrowd
  | i == 0 = identity
  | i == 1 = (notch_ "lg-notch-1" 300.0 1.0)
  | i == 2 = (notch_ "lg-notch-2" 900.0 1.0) <<< lg2f 1 LgCrowd
  | i == 3 = (notch_ "lg-notch-3" 1400.0 1.0) <<< lg2f 2 LgCrowd
  | i == 4 = (notch_ "lg-notch-4" 2000.0 1.0) <<< lg2f 3 LgCrowd
  | otherwise = (notch_ "lg-notch-5" 2500.0 1.0) <<< lg2f 4 LgCrowd

lg2f _ _ = identity

largeSingleton :: Int -> Number -> LargeTrack -> Maybe Number -> MusicM AudioListD2
largeSingleton nfilt st ltrack ed =
  let
    ln = lt2s ltrack

    ft = lg2f nfilt ltrack

    ept = st + largeCrossing
  in
    do
      { time } <- ask
      pure2 (ft (gain_' ("gain_large_" <> ln) (1.0 * (bb01 $ calcSlope (ept - 5.0) 1.0 ept 0.0 time) * (bb01 $ calcSlope st 0.0 (st + 4.0) 1.0 time) * (maybe 1.0 (\ee -> bb01 $ calcSlope ee 1.0 (ee + 4.0) 0.0 time) ed)) (loopBuf_ ("loop_large_" <> ln) ln 1.0 0.0 0.0)))

makeLarges :: Int -> Number -> Maybe Number -> Maybe Number -> Maybe Number -> Maybe Number -> MusicM AudioListD2
makeLarges nfilt st birdsOut santaOut chimesOut synthOut =
  let
    lfunc = largeSingleton nfilt st
  in
    fold
      <$> sequence [ lfunc LgSanta santaOut, lfunc LgBirds birdsOut, lfunc LgChimes chimesOut, lfunc LgSynth synthOut, lfunc LgCrowd Nothing ]

--  large start
largeListF :: Number -> List Number -> MusicM AudioListD2
largeListF st Nil = makeLarges 0 st Nothing Nothing Nothing Nothing

largeListF st (a : Nil) = makeLarges 0 st (Just a) Nothing Nothing Nothing

largeListF st (b : a : Nil) = makeLarges 0 st (Just a) (Just b) Nothing Nothing

largeListF st (c : b : a : Nil) = makeLarges 0 st (Just a) (Just b) (Just c) Nothing

largeListF st (d : c : b : a : Nil) = makeLarges 0 st (Just a) (Just b) (Just c) (Just d)

largeListF st (e : d : c : b : a : Nil) = makeLarges 1 st (Just a) (Just b) (Just c) (Just d)

largeListF st (f : e : d : c : b : a : Nil) = makeLarges 2 st (Just a) (Just b) (Just c) (Just d)

largeListF st (g : f : e : d : c : b : a : Nil) = makeLarges 3 st (Just a) (Just b) (Just c) (Just d)

largeListF st (h : g : f : e : d : c : b : a : Nil) = makeLarges 4 st (Just a) (Just b) (Just c) (Just d)

-- ignore anything larger than 7 filters
-- even this is probably too big...
largeListF st (foo : bar) = largeListF st bar

large' :: Number -> MusicM AudioListD2
large' begT = do
  ll <- fromMaybe Nil <$> asks getLargeList
  largeListF begT ll

large :: MusicM AudioListD2
large = boundedEffect "large" getLargeBegTime getLargeEndTime large'

largeEndTimeBleed = standardEndTimeBleed :: Number

largeLens = amark <<< prop (Proxy :: Proxy "large")

getLargeBegTime = getBegTime largeLens :: BegTimeGetter

getLargeList = getInter largeLens :: AccumulatorGetter (List Number)

getLargeEndTime = getEndTime largeLens :: EndTimeGetter

setLargeBegTime = setBegTime largeLens :: BegTimeSetter

setLargeList = setInter largeLens :: AccumulatorSetter (List Number)

setLargeEndTime = setEndTime largeLens :: EndTimeSetter
