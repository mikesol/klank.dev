module Klank.Studio.SilentNight.Heart where

import Prelude
import Control.Monad.Reader (asks)
import Data.Array (range)
import Data.Foldable (fold)
import Data.Int (toNumber)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Typelevel.Num (class Pos)
import FRP.Behavior.Audio (AudioUnit, gain_', lowpass_, playBuf_)
import Klank.Studio.SilentNight.Optics (AccumulatorGetter, AccumulatorSetter, BegTimeGetter, BegTimeSetter, EndTimeGetter, EndTimeSetter, amark, getBegTime, getEndTime, getInter, setBegTime, setEndTime, setInter)
import Klank.Studio.SilentNight.Types (AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (MusicM)
import Klank.Studio.SilentNight.Util (boundPlayer, boundedEffect, pure2, standardEndTimeBleed, standardOutro)
import Math ((%), sin, pi)
import Type.Proxy (Proxy(..))

heartbeat :: Number -> Number -> Number -> Number -> Number -> Number -> Number
heartbeat st b0 s0 b1 s1 t = go (t % pd)
  where
  pd = st + b0 + s0 + b1 + s1

  go i
    | i < st = 0.0
    | i < b0 + st = sin ((i - st) * pi / b0)
    | i < s0 + b0 + st = 0.0
    | i < b1 + s0 + b0 + st = 0.7 * sin ((i - b0 - s0 - st) * pi / b1)
    | otherwise = 0.0

heartNormal = 10.0 :: Number

heartHelper :: forall om im ch i. Applicative om => Applicative im => Pos ch => Show i => i -> String -> om (im (AudioUnit ch))
heartHelper i bf = pure2 $ gain_' (bf <> "gain" <> show i) 0.3 (lowpass_ (bf <> "lowpass" <> show i) 150.0 2.0 (playBuf_ (bf <> "buf" <> show i) bf 1.0))

heart' :: Number -> MusicM AudioListD2
heart' btm = do
  v <- asks getHeartStartTime
  case v of
    Nothing -> mempty
    Just t ->
      boundPlayer t (heartNormal + standardOutro)
        ( fold
            <$> sequence
                ( join
                    ( map
                        ( \i ->
                            [ boundPlayer (t + 0.2 + (toNumber i) * 1.45) 2.0 (heartHelper i "hb1"), boundPlayer (t + 0.7 + (toNumber i) * 1.45) 2.0 (heartHelper i "hb2")
                            ]
                        )
                        (range 0 30)
                    )
                )
        )

heart :: MusicM AudioListD2
heart = boundedEffect "heart" getHeartBegTime getHeartEndTime heart'

heartEndTimeBleed = standardEndTimeBleed :: Number

heartLens = amark <<< prop (Proxy :: Proxy "heart")

getHeartBegTime = getBegTime heartLens :: BegTimeGetter

getHeartStartTime = getInter heartLens :: AccumulatorGetter Number

getHeartEndTime = getEndTime heartLens :: EndTimeGetter

setHeartBegTime = setBegTime heartLens :: BegTimeSetter

setHeartStartTime = setInter heartLens :: AccumulatorSetter Number

setHeartEndTime = setEndTime heartLens :: EndTimeSetter
