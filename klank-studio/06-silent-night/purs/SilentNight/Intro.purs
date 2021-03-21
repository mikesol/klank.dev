module Klank.Studio.SilentNight.Intro where

import Prelude
import Control.Monad.Reader (ask)
import Data.Foldable (fold)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), uncurry)
import Data.Typelevel.Num (D2)
import FRP.Behavior.Audio (AudioUnit, gain_, gain_', playBufT_, playBuf_)
import Graphics.Painting (Painting, circle, fillColor, filled)
import Klank.Studio.SilentNight.Actionable (doAction)
import Klank.Studio.SilentNight.Types (MusicalInfo, AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (Activity(..), SilentNightAccumulator, MusicM)
import Klank.Studio.SilentNight.Types.Canvas (MakeCanvasT, MakeCanvas)
import Klank.Studio.SilentNight.Types.Intro (ChoiceEvent(..), HarmChooserStep(..), IntroLoop(..), Verse(..), VerseChoice(..))
import Klank.Studio.SilentNight.Util (beatGapToStartOffsetAsParam, bindBetween, boundPlayer, calcSlope, circleDivisor, crotchet, dark, fadeIn, fadeOut, kr, pure2, stay, toNel, whiteRGBA, (|<), (||<))
import Math (pow)

il2s :: IntroLoop -> String
il2s IntroLoopA = "IntroLoopA"

il2s IntroLoopB = "IntroLoopB"

il2s IntroLoopC = "IntroLoopC"

il2s IntroLoopD = "IntroLoopD"

il2s IntroLoopE = "IntroLoopE"

startM0 = mmi 0 0.0 :: MusicalInfo

startM0_1 = mmi 0 1.0 :: MusicalInfo

startM0_2 = mmi 0 2.0 :: MusicalInfo

startM1 = mmi 1 0.0 :: MusicalInfo

startM4 = mmi 4 0.0 :: MusicalInfo

startM8 = mmi 8 0.0 :: MusicalInfo

startM12 = mmi 12 0.0 :: MusicalInfo

startM16 = mmi 16 0.0 :: MusicalInfo

startM20 = mmi 20 0.0 :: MusicalInfo

mmi :: Int -> Number -> MusicalInfo
mmi measure beat = { measure, beat }

nCurve :: Number -> Number
nCurve = pnCurve 1.0

pnCurve :: Number -> Number -> Number
pnCurve p n
  | n < 0.5 = ((2.0 * n) `pow` p) / 2.0
  | otherwise = ((((n - 0.5) * 2.0) `pow` (1.0 / p)) / 2.0) + 0.5

circles :: Number -> Number -> Number -> (VerseChoice -> Number) -> Number -> Painting
circles fH w h opq traj = fold (map (\i' -> let i = (toNumber <<< versionToInt) i' in filled (fillColor (whiteRGBA (opq i'))) (circle ((2.0 * i * traj + 1.0) * w / 16.0) h (makeCircleDim (min w fH) i))) verseChoices)

circles' :: Number -> Number -> Number -> (Number -> Number) -> Painting
circles' fH w h traj = fold (map (\i' -> let i = (toNumber <<< versionToInt) i' in filled (fillColor (whiteRGBA 1.0)) (circle ((2.0 * i + 1.0) * w / 16.0) h (makeCircleDim (min w fH) i * (traj i)))) verseChoices)

circleFanner :: Number -> Number -> Number -> Number -> Number -> Painting
circleFanner fH w h startsAt time =
  let
    nTime = time - startsAt
  in
    circles' fH w h
      ( \i ->
          let
            st = (pnCurve 0.7 $ i / 10.0) * circleIntro

            ed = (pnCurve 0.7 $ (i + 2.0) / 10.0) * circleIntro

            x
              | nTime < st = 0.0
              | nTime >= ed = 1.0
              | otherwise = pnCurve 1.2 $ min 1.0 (max 0.0 (calcSlope st 0.0 ed 1.0 nTime))
          in
            x
      )

circleChoice :: Number -> Number -> Number -> VerseChoice -> Number -> Number -> Painting
circleChoice fH w h vc startsAt time = circles fH w h (\v -> if v == vc then 1.0 else max 0.3 (1.0 - (time - startsAt) / (0.7 * circleFade))) 1.0

firstRow :: Number -> Number
firstRow h = h / 6.0

secondRow :: Number -> Number
secondRow h = 3.0 * h / 6.0

thirdRow :: Number -> Number
thirdRow h = 5.0 * h / 6.0

circleChosen :: Number -> Number -> Number -> VerseChoice -> Painting
circleChosen fH w h vc = circles fH w h (\v -> if v == vc then 1.0 else 0.3) 1.0

circleOutroStart :: Number -> Number -> Verse -> VerseChoice -> Tuple Number Number
circleOutroStart w h Verse1 VersionOne = Tuple (1.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionTwo = Tuple (3.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionThree = Tuple (5.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionFour = Tuple (7.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionFive = Tuple (9.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionSix = Tuple (11.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionSeven = Tuple (13.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionEight = Tuple (15.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse2 VersionOne = Tuple (1.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionTwo = Tuple (3.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionThree = Tuple (5.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionFour = Tuple (7.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionFive = Tuple (9.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionSix = Tuple (11.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionSeven = Tuple (13.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionEight = Tuple (15.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse3 VersionOne = Tuple (1.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionTwo = Tuple (3.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionThree = Tuple (5.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionFour = Tuple (7.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionFive = Tuple (9.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionSix = Tuple (11.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionSeven = Tuple (13.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionEight = Tuple (15.0 * w / 16.0) (thirdRow h)

circleOutroEnd :: Number -> Number -> Verse -> VerseChoice -> Tuple Number Number
circleOutroEnd w h Verse1 VersionOne = Tuple (w * 0.29) (h * 0.91)

circleOutroEnd w h Verse1 VersionTwo = Tuple (w * 0.45) (h * 0.23)

circleOutroEnd w h Verse1 VersionThree = Tuple (w * 0.02) (h * 0.45)

circleOutroEnd w h Verse1 VersionFour = Tuple (w * 0.23) (h * 0.34)

circleOutroEnd w h Verse1 VersionFive = Tuple (w * 0.96) (h * 0.77)

circleOutroEnd w h Verse1 VersionSix = Tuple (w * 0.72) (h * 0.51)

circleOutroEnd w h Verse1 VersionSeven = Tuple (w * 0.01) (h * 0.13)

circleOutroEnd w h Verse1 VersionEight = Tuple (w * 0.60) (h * 0.03)

circleOutroEnd w h Verse2 VersionOne = Tuple (w * 0.18) (h * 0.76)

circleOutroEnd w h Verse2 VersionTwo = Tuple (w * 0.14) (h * 0.54)

circleOutroEnd w h Verse2 VersionThree = Tuple (w * 0.85) (h * 0.38)

circleOutroEnd w h Verse2 VersionFour = Tuple (w * 0.36) (h * 0.65)

circleOutroEnd w h Verse2 VersionFive = Tuple (w * 0.72) (h * 0.41)

circleOutroEnd w h Verse2 VersionSix = Tuple (w * 0.3) (h * 0.03)

circleOutroEnd w h Verse2 VersionSeven = Tuple (w * 0.9) (h * 0.57)

circleOutroEnd w h Verse2 VersionEight = Tuple (w * 0.05) (h * 0.98)

circleOutroEnd w h Verse3 VersionOne = Tuple (w * 0.21) (h * 0.82)

circleOutroEnd w h Verse3 VersionTwo = Tuple (w * 0.36) (h * 0.55)

circleOutroEnd w h Verse3 VersionThree = Tuple (w * 0.45) (h * 0.45)

circleOutroEnd w h Verse3 VersionFour = Tuple (w * 0.08) (h * 0.13)

circleOutroEnd w h Verse3 VersionFive = Tuple (w * 0.72) (h * 0.86)

circleOutroEnd w h Verse3 VersionSix = Tuple (w * 0.63) (h * 0.34)

circleOutroEnd w h Verse3 VersionSeven = Tuple (w * 0.83) (h * 0.75)

circleOutroEnd w h Verse3 VersionEight = Tuple (w * 0.02) (h * 0.99)

circleOutro :: Verse -> VerseChoice -> Boolean -> Number -> Number -> Number -> Number -> Painting
circleOutro vs vc chosen w h startsAt time =
  let
    opq' = if chosen then 1.0 else 0.3

    opq = max 0.0 $ opq' - (opq' * (time - startsAt) / circleFlyAway)

    i = (toNumber <<< versionToInt) vc

    (Tuple x0 y0) = circleOutroStart w h vs vc

    (Tuple x1 y1) = circleOutroEnd w h vs vc

    x = calcSlope 0.0 x0 1.0 x1 (pnCurve 0.8 $ (time - startsAt) / circleFlyAway)

    y = calcSlope 0.0 y0 1.0 y1 (pnCurve 0.8 $ (time - startsAt) / circleFlyAway)
  in
    filled (fillColor (whiteRGBA opq)) (circle x y (makeCircleDim (min w h) i))

darknessFadeIn = 0.0 :: Number

silentNightFadeIn = darknessFadeIn + fadeIn :: Number

silentNightStay = silentNightFadeIn + fadeIn :: Number

silentNightFadeOut = silentNightStay + stay :: Number

silentNightDark = silentNightFadeOut + fadeOut :: Number

instructionFadeIn = silentNightDark + dark :: Number

instructionStay = instructionFadeIn + fadeIn :: Number

instructionFadeOut = instructionStay + stay :: Number

instructionDark = instructionFadeOut + fadeOut :: Number

circleIntro = 3.0 :: Number

circleFade = 0.5 :: Number

circleFlyAway = 5.0 :: Number

introOpacity :: Number -> Number
introOpacity time
  | time < silentNightFadeIn = calcSlope darknessFadeIn 0.0 silentNightFadeIn 0.0 time
  | time < silentNightStay = calcSlope silentNightFadeIn 0.0 silentNightStay 1.0 time
  | time < silentNightFadeOut = calcSlope silentNightStay 1.0 silentNightFadeOut 1.0 time
  | time < silentNightDark = calcSlope silentNightFadeOut 1.0 silentNightDark 0.0 time
  | time < instructionFadeIn = calcSlope silentNightDark 0.0 instructionFadeIn 0.0 time
  | time < instructionStay = calcSlope instructionFadeIn 0.0 instructionStay 1.0 time
  | time < instructionFadeOut = calcSlope instructionStay 1.0 instructionFadeOut 1.0 time
  | time < instructionDark = calcSlope instructionFadeOut 1.0 instructionDark 0.0 time
  | otherwise = 0.0

verseChoices =
  [ VersionOne
  , VersionTwo
  , VersionThree
  , VersionFour
  , VersionFive
  , VersionSix
  , VersionSeven
  , VersionEight
  ] ::
    Array VerseChoice

versionToInt :: VerseChoice -> Int
versionToInt VersionOne = 0

versionToInt VersionTwo = 1

versionToInt VersionThree = 2

versionToInt VersionFour = 3

versionToInt VersionFive = 4

versionToInt VersionSix = 5

versionToInt VersionSeven = 6

versionToInt VersionEight = 7

chooseVerseOne :: MakeCanvas -> VerseChoice -> SilentNightAccumulator -> Number -> MakeCanvasT
chooseVerseOne makeCanvas vc acc time =
  makeCanvas
    ( acc
        { verseStarts =
          acc.verseStarts
            { one = Just $ Tuple time vc
            }
        , activity = HarmChooser { step: Row2Animation { startsAt: time, verseOne: vc } }
        }
    )
    time

chooseVerseTwo :: MakeCanvas -> VerseChoice -> VerseChoice -> SilentNightAccumulator -> Number -> MakeCanvasT
chooseVerseTwo makeCanvas v1 vc acc time =
  makeCanvas
    ( acc
        { verseStarts =
          acc.verseStarts
            { two = Just $ Tuple time vc
            }
        , activity = HarmChooser { step: Row3Animation { startsAt: time, verseOne: v1, verseTwo: vc } }
        }
    )
    time

chooseVerseThree :: MakeCanvas -> VerseChoice -> VerseChoice -> VerseChoice -> SilentNightAccumulator -> Number -> MakeCanvasT
chooseVerseThree makeCanvas v1 v2 vc acc time =
  makeCanvas
    ( acc
        { introEnds = Just time
        , verseStarts =
          acc.verseStarts
            { three = Just $ Tuple time vc
            }
        , activity = HarmChooser { step: FadeOutAnimation { startsAt: time, verseOne: v1, verseTwo: v2, verseThree: vc } }
        }
    )
    time

makeCircleDim :: Number -> Number -> Number
makeCircleDim w i = calcSlope 0.0 (w / circleDivisor) 7.0 (w / (circleDivisor * 2.0)) i

doVAction :: SilentNightAccumulator -> Number -> Number -> Number -> VerseChoice -> Boolean
doVAction acc w fullH h vc =
  let
    i = (toNumber <<< versionToInt) vc

    dim = makeCircleDim (min w fullH) i

    twoDim = 2.0 * dim

    out =
      doAction acc
        { x: ((2.0 * i + 1.0) * w / 16.0) - dim
        , y: h - dim
        , width: twoDim
        , height: twoDim
        }
  in
    out

verseVersionChooser :: (VerseChoice -> SilentNightAccumulator -> Number -> MakeCanvasT) -> Number -> Number -> Number -> Painting -> SilentNightAccumulator -> Number -> MakeCanvasT
verseVersionChooser vc w fullH h d acc time
  | doVAction acc w fullH h VersionOne = vc VersionOne acc time
  | doVAction acc w fullH h VersionTwo = vc VersionTwo acc time
  | doVAction acc w fullH h VersionThree = vc VersionThree acc time
  | doVAction acc w fullH h VersionFour = vc VersionFour acc time
  | doVAction acc w fullH h VersionFive = vc VersionFive acc time
  | doVAction acc w fullH h VersionSix = vc VersionSix acc time
  | doVAction acc w fullH h VersionSeven = vc VersionSeven acc time
  | doVAction acc w fullH h VersionEight = vc VersionEight acc time
  | otherwise = pure $ Tuple acc (d <> circles fullH w h (const 1.0) 1.0)

introLoopSingleton :: IntroLoop -> Number -> MusicM (AudioUnit D2)
introLoopSingleton il gp = let ils = il2s il in pure $ playBufT_ ("buf" <> ils) ils (beatGapToStartOffsetAsParam 1.0 gp)

introLoopPlayer :: MusicM AudioListD2
introLoopPlayer = do
  { musicalInfo, mainStarts, time } <- ask
  let
    mmm40 = musicalInfo { measure = musicalInfo.measure `mod` 20 }

    gbms = maybe Just (\t -> if t - time < kr then const Nothing else Just) mainStarts
  let
    o
      | musicalInfo ||< startM4 =
        sequence
          $ introLoopSingleton IntroLoopA 0.0
          : Nil
      | Just gap <- startM20 |< mmm40 =
        (sequence <<< L.catMaybes)
          $ gbms (introLoopSingleton IntroLoopA gap)
          : pure (introLoopSingleton IntroLoopE 0.0)
          : Nil
      | mmm40 ||< startM4 =
        (sequence <<< L.catMaybes)
          $ gbms (introLoopSingleton IntroLoopA 0.0)
          : pure (introLoopSingleton IntroLoopE 0.0)
          : Nil
      | Just gap <- startM4 |< mmm40
      , mmm40 ||< startM8 =
        (sequence <<< L.catMaybes)
          $ gbms (introLoopSingleton IntroLoopB gap)
          : pure (introLoopSingleton IntroLoopA 0.0)
          : Nil
      | Just gap <- startM8 |< mmm40
      , mmm40 ||< startM12 =
        (sequence <<< L.catMaybes)
          $ gbms (introLoopSingleton IntroLoopC gap)
          : pure (introLoopSingleton IntroLoopB 0.0)
          : Nil
      | Just gap <- startM12 |< mmm40
      , mmm40 ||< startM16 =
        (sequence <<< L.catMaybes)
          $ gbms (introLoopSingleton IntroLoopD gap)
          : pure (introLoopSingleton IntroLoopC 0.0)
          : Nil
      | Just gap <- startM16 |< mmm40 =
        (sequence <<< L.catMaybes)
          $ gbms (introLoopSingleton IntroLoopE gap)
          : pure (introLoopSingleton IntroLoopD 0.0)
          : Nil
      | otherwise = mempty
  o

introLoop :: MusicM AudioListD2
introLoop = do
  { time } <- ask
  ilp <- introLoopPlayer
  pure2 $ gain_ "introLoopFader" (bindBetween 0.0 0.7 $ calcSlope (2.0 * crotchet) 0.0 (10.0 * crotchet) 0.7 time) (toNel ilp)

choicePlayer :: String -> Number -> MusicM (AudioUnit D2)
choicePlayer tag pshift = pure (gain_' ("gainChoice" <> tag) 0.6 (playBuf_ ("bufChoice" <> tag) "choiceBell" pshift))

makeChoiceEvent :: ChoiceEvent -> (Maybe (Tuple Number VerseChoice)) -> MusicM AudioListD2
makeChoiceEvent _ Nothing = mempty

makeChoiceEvent ce (Just (Tuple st _)) = case ce of
  ChoiceEventA -> boundPlayer st 6.0 (pure <$> choicePlayer "ceA" 0.9)
  ChoiceEventB -> boundPlayer st 6.0 (pure <$> choicePlayer "ceB" 0.6723)
  ChoiceEventC -> boundPlayer st 6.0 (pure <$> choicePlayer "ceC" 1.01)

choiceEvents :: MusicM AudioListD2
choiceEvents = do
  { verseStarts: { one, two, three } } <- ask
  fold
    <$> sequence
        ( map (uncurry makeChoiceEvent)
            [ Tuple ChoiceEventA one
            , Tuple ChoiceEventB two
            , Tuple ChoiceEventC three
            ]
        )

introBG :: MusicM AudioListD2
introBG = do
  { time, mainStarts } <- ask
  if maybe false (\x -> time > x + 5.0) mainStarts then
    mempty
  else
    fold
      <$> sequence
          [ introLoop
          , choiceEvents
          ]
