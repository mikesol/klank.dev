module Klank.Studio.SilentNight.Optics where

import Prelude
import Data.Lens (_1, _2, _Just, over, preview, set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Klank.Studio.SilentNight.Types.Accumulator (AudioEnv, SilentNightAccumulator, Marker)
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons)

type AMark sym
  = forall prof rrs rrt rint s t os ot.
    Cons sym s rrs os =>
    Cons sym t rrt ot =>
    Strong prof =>
    prof s t ->
    prof
      { audioMarkers :: (Record os)
      | rint
      }
      { audioMarkers :: (Record ot)
      | rint
      }

amark ::
  forall p ami amo rest.
  Strong p =>
  p ami amo ->
  p
    { audioMarkers :: ami
    | rest
    }
    { audioMarkers :: amo
    | rest
    }
amark = prop (Proxy :: Proxy "audioMarkers")

type AccumulatorSetter a
  = a -> SilentNightAccumulator -> SilentNightAccumulator

type EndTimeSetter
  = AccumulatorSetter Number

type BegTimeSetter
  = AccumulatorSetter Number

type AccumulatorGetter a
  = AudioEnv -> (Maybe a)

type BegTimeGetter
  = AccumulatorGetter (Number)

type EndTimeGetter
  = AccumulatorGetter (Number)

_Just_2_1 = _Just <<< _2 :: forall p inter. Choice p => Strong p => p (Maybe (Tuple inter (Maybe Number))) (Maybe (Tuple inter (Maybe Number))) -> p (Marker inter) (Marker inter)

_Just_2_2 = _Just <<< _2 <<< _Just <<< _2 :: forall p inter. Choice p => Strong p => p (Maybe Number) (Maybe Number) -> p (Marker inter) (Marker inter)

maybeTupleMod :: forall i x. i -> Maybe (Tuple i (Maybe x)) -> Maybe (Tuple i (Maybe x))
maybeTupleMod i Nothing = Just $ Tuple i Nothing

maybeTupleMod i (Just (Tuple _ x)) = Just $ Tuple i x

--getBegTime :: forall inter r1. (Getter' { audioMarkers :: AudioMarkers | r1 } (Marker inter)) -> SilentNightAccumulator -> Maybe Number
getBegTime interLens = preview (interLens <<< _Just <<< _1)

getInter interLens = preview (interLens <<< _Just <<< _2 <<< _Just <<< _1)

--getEndTime :: forall inter r1. (Getter' { audioMarkers :: AudioMarkers | r1 } (Marker inter)) -> SilentNightAccumulator -> Maybe Number
getEndTime interLens = preview (interLens <<< _Just <<< _2 <<< _Just <<< _2 <<< _Just)

--setBegTime :: forall inter r1. (Setter' { audioMarkers :: AudioMarkers | r1 } (Marker inter)) -> Number -> SilentNightAccumulator -> SilentNightAccumulator
setBegTime interLens = over interLens <<< maybeTupleMod

--setInter :: forall inter r1. (Setter' { audioMarkers :: AudioMarkers | r1 } (Marker inter)) -> inter -> SilentNightAccumulator -> SilentNightAccumulator
setInter interLens = over (interLens <<< _Just_2_1) <<< maybeTupleMod

---setEndTime :: forall inter r1. (Setter' { audioMarkers :: AudioMarkers | r1 } (Marker inter)) -> Number -> SilentNightAccumulator -> SilentNightAccumulator
setEndTime endTimeLens = set (endTimeLens <<< _Just_2_2) <<< Just
