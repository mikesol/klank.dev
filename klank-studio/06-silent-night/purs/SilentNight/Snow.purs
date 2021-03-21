module Klank.Studio.SilentNight.Snow where

import Prelude
import Control.Monad.Reader (asks)
import Data.Array as A
import Data.Foldable (fold)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import FRP.Behavior.Audio (gain_', playBuf_)
import Graphics.Painting (circle, fillColor, filled)
import Klank.Studio.SilentNight.Actionable (doAction)
import Klank.Studio.SilentNight.Optics (AccumulatorGetter, AccumulatorSetter, BegTimeGetter, BegTimeSetter, EndTimeGetter, EndTimeSetter, amark, getBegTime, getEndTime, getInter, setBegTime, setEndTime, setInter)
import Klank.Studio.SilentNight.Types (AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (Activity(..), MusicM, PlayerEvent(..), SilentNightAccumulator, SilentNightPlayerT)
import Klank.Studio.SilentNight.Types.Canvas (MakeCanvas, MakeCanvasT)
import Klank.Studio.SilentNight.Util (boundPlayer, boundedEffect, calcSlope, conv1, pure2, sqToRect, standardEndTimeBleed, whiteRGBA)
import Type.Proxy (Proxy(..))

data SnowI
  = SnowI Number Number Number

-- print(",".join(["SnowI "+str(random.random())+" "+str(random.random())+" "+str(random.random()) for x in range(42)]))
snows = [ SnowI 0.1763482237244488 0.7843392558067448 0.8736775739009528, SnowI 0.28200324903345875 0.7616174886387548 0.9886386189118499, SnowI 0.5333998008487347 0.9930176360953382 0.4358035440368596, SnowI 0.6215201584313228 0.2579016465924222 0.9752643594074341, SnowI 0.866939186383011 0.8360805146799725 0.7958765209012253, SnowI 0.8316783439260418 0.37836656895524046 0.5791729169409586, SnowI 0.2733107241502992 0.3612653659521434 0.19231210814850852, SnowI 0.31027228153383757 0.13944530644247088 0.7550167721602923, SnowI 0.9998447705284339 0.6730718131664458 0.8426964150644125, SnowI 0.17929816108198704 0.790475448268706 0.0003747545515384587, SnowI 0.5146676245826576 0.9116200472758966 0.8531328483755889, SnowI 0.16350352611390262 0.035050938943344545 0.12000898554296346, SnowI 0.02235639770750919 0.2637379491356546 0.38653640592838456, SnowI 0.09638415930577127 0.5450761098431707 0.04520437463832483, SnowI 0.4892276041330552 0.3173833927642091 0.9635762922386145, SnowI 0.5579747466273458 0.4261821763227277 0.034829543965688825, SnowI 0.1383348346924944 0.8730898886001669 0.5821842719623467, SnowI 0.5417335843317096 0.5227917372193671 0.5841349868208985, SnowI 0.561091946480468 0.7226139875075132 0.3254593174473801, SnowI 0.18440849817668648 0.8842935817289816 0.23906878196524894, SnowI 0.8395874260044586 0.28184640063208755 0.14333272039170541, SnowI 0.6156612759042932 0.713960173022045 0.7480729332633452, SnowI 0.38173188767605626 0.7387808291538753 0.6278447076280427, SnowI 0.24589553416745413 0.5290708205531433 0.32196549155354526, SnowI 0.35871499328693446 0.7548104803495969 0.20947469621974146, SnowI 0.7788111970805636 0.5267905775598622 0.6587485955119049, SnowI 0.6335840127131971 0.22575848163364354 0.8774238179179625, SnowI 0.4775763671281781 0.23139336402021182 0.4714066657711262, SnowI 0.6117193254284164 0.5404145696505988 0.9423220409781308, SnowI 0.8393112405852664 0.5386134230858991 0.34356584701617143, SnowI 0.7273029953298517 0.9157883450982325 0.5161433407992876, SnowI 0.7523225195383978 0.9706116721435495 0.8485145712220996, SnowI 0.25565438866795875 0.4858444945461321 0.02409997710228562, SnowI 0.2482461461505997 0.06998419990506399 0.8023056575942396, SnowI 0.6447100246955894 0.23461445726865426 0.7437139284297501, SnowI 0.4037462586982459 0.8347403986979446 0.6252308698800807, SnowI 0.5777309693538369 0.7523862184546504 0.7963104491899832, SnowI 0.10765091503325408 0.9055179728124041 0.5411994836080661, SnowI 0.009800138222332277 0.2174946835643694 0.9471799584836776, SnowI 0.5718328748696805 0.1991607030882191 0.6324687034592706, SnowI 0.009331228956806159 0.8127670538006947 0.10168511813889736, SnowI 0.971185774284385 0.6019894835093992 0.5234232707786459 ] :: Array SnowI

snowL = A.length snows :: Int

snowList = L.fromFoldable snows :: List SnowI

snowYp :: Number -> Number -> Number -> Number
snowYp h nowT v = h * (min 1.1 $ calcSlope 0.0 (-0.1) (snowMin + (v * snowDiff)) (1.1) nowT)

snowXp :: Number -> Number -> Number
snowXp w v = w * v

snowRad :: Number -> Number -> Number -> Number
snowRad w h v = (min w h) * 0.05 * v

snowMin = 6.0 :: Number

snowMax = 24.0 :: Number

snowDiff = snowMax - snowMin :: Number

snowRecurser :: MakeCanvas -> SilentNightPlayerT -> Number -> Number -> SilentNightAccumulator -> Number -> Number -> List (Maybe Number) -> MakeCanvasT
snowRecurser makeCanvas i w h acc startT time l = go 0 Nil (if acc.inClick then l else Nil)
  where
  go z hd Nil =
    pure
      $ Tuple (setSnowList l (setSnowBegTime i.eventStart acc))
          ( fold
              ( map
                  ( \(Tuple (SnowI xrd yrd rrd) mn) ->
                      if mn == Nothing then
                        filled (fillColor (whiteRGBA 1.0))
                          (circle (snowXp w xrd) (snowYp h (time - startT) yrd) (snowRad w h rrd))
                      else
                        mempty
                  )
                  (A.zipWith Tuple snows (A.fromFoldable l))
              )
          )

  go z hd (a : b) =
    if a == Nothing
      && maybe false
          ( \(SnowI xrd yrd rrd) ->
              doAction
                acc
                (sqToRect (snowXp w xrd) (snowYp h (time - startT) yrd) (snowRad w h rrd))
          )
          (A.index snows z) then
      makeCanvas
        ( acc
            { activity =
              SilentNightPlayer
                ( i
                    { playerEvents = [ Snow (hd <> (pure $ Just time) <> b) ] <> A.drop 1 i.playerEvents
                    }
                )
            }
        )
        time
    else
      go (z + 1) (hd <> pure a) b

snowSingleton :: Int -> Number -> MusicM AudioListD2
snowSingleton i st = boundPlayer st 4.0 (pure2 (gain_' ("snowGain_" <> show i) (fromMaybe 0.3 (A.index [ 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8 ] (i `mod` 7))) (playBuf_ ("snowBuf_" <> show i) "snow" (conv1 (fromMaybe 1.0 $ A.index [ 1.0, 6.0, 8.0, 10.0, 13.0, 18.0, 20.0, 22.0 ] (i `mod` 8))))))

bFoldL :: forall a. (Int -> a -> Boolean) -> List a -> Boolean
bFoldL f l = go 0 l
  where
  go _ Nil = false

  go x (a : b) = if f x a then true else go (x + 1) b

snowAudio :: List (Maybe Number) -> MusicM AudioListD2
snowAudio l = fold <$> sequence (go 0 l)
  where
  go :: Int -> List (Maybe Number) -> List (MusicM AudioListD2)
  go i Nil = Nil

  go i (Nothing : b) = go (i + 1) b

  go i (Just a : b) = snowSingleton i a : go (i + 1) b

snow' :: Number -> MusicM AudioListD2
snow' begT = (fromMaybe baseSnows <$> asks getSnowList) >>= snowAudio

snow :: MusicM AudioListD2
snow = boundedEffect "snow" getSnowBegTime getSnowEndTime snow'

snowEndTimeBleed = standardEndTimeBleed :: Number

snowLens = amark <<< prop (Proxy :: Proxy "snow")

getSnowBegTime = getBegTime snowLens :: BegTimeGetter

getSnowList = getInter snowLens :: AccumulatorGetter (List (Maybe Number))

getSnowEndTime = getEndTime snowLens :: EndTimeGetter

setSnowBegTime = setBegTime snowLens :: BegTimeSetter

setSnowList = setInter snowLens :: AccumulatorSetter (List (Maybe Number))

setSnowEndTime = setEndTime snowLens :: EndTimeSetter

baseSnows = L.fromFoldable $ A.replicate snowL Nothing :: List (Maybe Number)
