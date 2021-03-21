module Klank.Studio.SilentNight.Canvas where

import Prelude
import Color (rgb, rgba)
import Control.Monad.Reader (ask)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (fold, foldl)
import Data.Lens (_2, over)
import Data.List ((:))
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe, maybe')
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (d0, d1, d2, d3, d4, d5)
import Data.Vec ((+>), empty)
import Data.Vec as V
import Graphics.Painting (arc, circle, rectangle, fillColor, filled, lineWidth, outlineColor, outlined, text)
import Graphics.Painting.Font (bold, customFont, font, italic, sansSerif)
import Klank.Studio.SilentNight.Actionable (doAction, doingAction)
import Klank.Studio.SilentNight.Bells (bellsEndTimeBleed, bellsNormal, bellsRecurser, makeBells, setBellsBegTime, setBellsEndTime)
import Klank.Studio.SilentNight.Gear (gearDir0, gearDir1, gearDir2, gearDir3, gearSpinF, gearStay, gearsEndTimeBleed, setGearsBegTime, setGearsEndTime, setGearsVector)
import Klank.Studio.SilentNight.Heart (heartEndTimeBleed, heartNormal, heartbeat, setHeartBegTime, setHeartEndTime, setHeartStartTime)
import Klank.Studio.SilentNight.Intro (chooseVerseOne, chooseVerseThree, chooseVerseTwo, circleChoice, circleChosen, circleFanner, circleFlyAway, circleIntro, circleOutro, firstRow, introOpacity, pnCurve, secondRow, silentNightFadeIn, thirdRow, verseChoices, verseVersionChooser)
import Klank.Studio.SilentNight.Large (largeCrossing, largeEndTimeBleed, setLargeBegTime, setLargeEndTime, setLargeList)
import Klank.Studio.SilentNight.Motion (calibrateX, calibrateY, eix, eiy, motionEndTimeBleed, motionMaker, motionNormal, needsToFollow, needsToStopFollowing, setMotionBegTime, setMotionEndTime, setMotionPos)
import Klank.Studio.SilentNight.Rise (riseEndTimeBleed, riseNormal, riseXP, setRiseBegTime, setRiseEndTime, setRiseVector)
import Klank.Studio.SilentNight.Shrink (paintShrinks, setShrinkBegTime, setShrinkEndTime, shrinkEndTimeBleed, shrinkFive, shrinkFour, shrinkNormal, shrinkOne, shrinkSix, shrinkStart, shrinkThree, shrinkTwo, targetsDefault, targetsFive, targetsFour, targetsOne, targetsSix, targetsThree, targetsTwo)
import Klank.Studio.SilentNight.Snow (SnowI(..), bFoldL, setSnowEndTime, snowEndTimeBleed, snowRecurser, snowYp, snows)
import Klank.Studio.SilentNight.Square (setSquareBegTime, setSquareEndTime, setSquareVector, squareEndTimeBleed, squareTravel)
import Klank.Studio.SilentNight.Stars (starFs)
import Klank.Studio.SilentNight.Triangle (setTriangleBegTime, setTriangleEndTime, setTriangleVector, triangleEndTimeBleed)
import Klank.Studio.SilentNight.Types.Accumulator (Activity(..), PlayerEvent(..), SilentNightAccumulator, SilentNightPlayerT)
import Klank.Studio.SilentNight.Types.Canvas (MakeCanvasT, MakeCanvas)
import Klank.Studio.SilentNight.Types.Intro (HarmChooserStep(..), Verse(..))
import Klank.Studio.SilentNight.Util (calcSlope, cosp, instructionDark, mouseOrBust, pythag, quaver, silentNightDark, sinp, sqToRect, standardIntro, standardOutro, standardPress, timeToMusicalInfo, whiteRGBA)
import Math (pi, pow)

pressEffect :: Number -> Number -> Number -> Number
pressEffect cw press time
  | time < press / 2.0 = cw + (time * cw * 0.1 / press)
  | time < press = cw + ((press - time) * cw * 0.1 / press)
  | otherwise = cw

nextObj :: forall a. (a -> PlayerEvent) -> SilentNightAccumulator -> SilentNightPlayerT -> (Number -> a) -> Number -> MakeCanvasT
nextObj pef acc i tf time =
  makeCanvas
    ( acc
        { activity =
          SilentNightPlayer
            ( i
                { playerEvents = [ pef (tf time) ] <> A.drop 1 i.playerEvents
                }
            )
        }
    )
    time

newCanvas :: SilentNightPlayerT -> SilentNightAccumulator -> Number -> MakeCanvasT
newCanvas i acc time =
  makeCanvas
    ( acc
        { activity =
          SilentNightPlayer
            ( i
                { playerEvents = A.drop 1 i.playerEvents
                , eventStart = time
                }
            )
        }
    )
    time

isLarge :: SilentNightAccumulator -> Boolean
isLarge { activity: SilentNightPlayer { playerEvents } } =
  maybe false
    ( \v -> case v of
        Large _ -> true
        _ -> false
    )
    $ A.head playerEvents

isLarge _ = false

makeCanvas :: MakeCanvas
makeCanvas acc time = do
  { w, h } <- ask
  let
    bg = filled (fillColor (rgb 0 0 0)) (rectangle 0.0 0.0 w h)

    introFade = if time > silentNightFadeIn then mempty else filled (fillColor (rgba 255 255 255 (calcSlope 0.0 1.0 silentNightFadeIn 0.0 time))) (rectangle 0.0 0.0 w h)

    starscape = fold (map (\f -> f w h time) starFs)
  map
    (over _2 (\i -> if isLarge acc then bg <> i <> starscape <> introFade else bg <> starscape <> i <> introFade))
    (go w h)
  where
  dAcc = doAction acc

  go :: Number -> Number -> MakeCanvasT
  go w h = case acc.activity of
    Intro ->
      if time > instructionDark then
        makeCanvas (acc { activity = HarmChooser { step: Row1Animation { startsAt: time } } }) time
      else
        pure
          $ Tuple acc
              ( text
                  ( font (if time > silentNightDark then sansSerif else customFont "Parisienne")
                      (if time > silentNightDark then 36 else 60)
                      (if time > silentNightDark then mempty else bold)
                  )
                  (w / 2.0 - if time > silentNightDark then 230.0 else 160.0)
                  (h / 2.0)
                  (fillColor (whiteRGBA (introOpacity time)))
                  if time > silentNightDark then "Click on or press the circles" else "Silent Night"
              )
    HarmChooser { step } -> case step of
      Row1Animation i ->
        if time > i.startsAt + circleIntro then
          makeCanvas (acc { activity = HarmChooser { step: Row1Choose } }) time
        else
          pure $ Tuple acc (circleFanner h w (firstRow h) i.startsAt time)
      Row1Choose -> verseVersionChooser (chooseVerseOne makeCanvas) w h (firstRow h) mempty acc time
      Row2Animation i ->
        if time > i.startsAt + circleIntro then
          makeCanvas (acc { activity = HarmChooser { step: Row2Choose { verseOne: i.verseOne } } }) time
        else
          pure $ Tuple acc (circleFanner h w (secondRow h) i.startsAt time <> circleChoice h w (firstRow h) i.verseOne i.startsAt time)
      Row2Choose i -> verseVersionChooser (chooseVerseTwo makeCanvas i.verseOne) w h (secondRow h) (circleChosen h w (firstRow h) i.verseOne) acc time
      Row3Animation i ->
        if time > i.startsAt + circleIntro then
          makeCanvas (acc { activity = HarmChooser { step: Row3Choose { verseOne: i.verseOne, verseTwo: i.verseTwo } } }) time
        else
          pure $ Tuple acc (circleFanner h w (thirdRow h) i.startsAt time <> circleChoice h w (secondRow h) i.verseTwo i.startsAt time <> circleChosen h w (firstRow h) i.verseOne)
      Row3Choose i -> verseVersionChooser (chooseVerseThree makeCanvas i.verseOne i.verseTwo) w h (thirdRow h) (circleChosen h w (firstRow h) i.verseOne <> circleChosen h w (secondRow h) i.verseTwo) acc time
      FadeOutAnimation i -> do
        { evts } <- ask
        if time > i.startsAt + circleFlyAway then
          makeCanvas
            ( acc
                { activity =
                  SilentNightPlayer
                    { verse: Verse1
                    , verseOne: i.verseOne
                    , verseTwo: i.verseTwo
                    , verseThree: i.verseThree
                    , playerEvents: evts
                    , eventStart: time
                    }
                }
            )
            time
        else
          pure
            $ Tuple acc
                ( fold
                    ( map (\vc -> circleOutro Verse1 vc (vc == i.verseOne) w h i.startsAt time) verseChoices
                        <> map (\vc -> circleOutro Verse2 vc (vc == i.verseTwo) w h i.startsAt time) verseChoices
                        <> map (\vc -> circleOutro Verse3 vc (vc == i.verseThree) w h i.startsAt time) verseChoices
                    )
                )
    SilentNightPlayer i -> case A.head i.playerEvents of
      Nothing -> pure $ Tuple acc mempty
      (Just evt) -> case evt of
        NoEvent dur ->
          if time > i.eventStart + dur then
            newCanvas i acc time
          else
            pure $ Tuple acc mempty
        Heart v ->
          let
            cw = (min w h) / 9.0

            nextHeart = nextObj Heart

            o
              | Just s <- v
              , shouldEnd <- standardOutro + s + heartNormal
              , time > shouldEnd = newCanvas i (setHeartEndTime (shouldEnd + heartEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ ( Tuple (setHeartBegTime i.eventStart acc)
                        $ filled
                            ( fillColor
                                (whiteRGBA ((time - i.eventStart) / standardIntro))
                            )
                            ( circle
                                (w / 2.0)
                                (h / 2.0)
                                cw
                            )
                    )
              | v
                  == Nothing
                  && dAcc
                      (sqToRect (w / 2.0) (h / 2.0) cw) = nextHeart acc i Just time
              | otherwise =
                pure
                  $ ( Tuple (maybe acc (flip setHeartStartTime acc) v)
                        $ ( filled
                              ( fillColor
                                  ( whiteRGBA
                                      ( case v of
                                          Nothing -> 1.0
                                          Just n' -> max 0.0 (min 1.0 (calcSlope (n' + heartNormal) 1.0 (standardOutro + n' + heartNormal) 0.0 time))
                                      )
                                  )
                              )
                              ( circle
                                  (w / 2.0)
                                  (h / 2.0)
                                  (cw * (1.0 + 0.08 * heartbeat 0.2 0.3 0.2 0.25 0.5 (maybe 0.0 (time - _) v)))
                              )
                          )
                    )
          in
            o
        Triangle v ->
          let
            sqv = sequence v

            top = V.index v d0

            left = V.index v d1

            right = V.index v d2

            cw = (min w h) / 16.0

            twoCw = 2.0 * cw

            nextTriangle = nextObj Triangle

            o
              | Just s <- sqv
              , shouldEnd <- standardOutro + foldl max 0.0 s
              , time > shouldEnd = newCanvas i (setTriangleEndTime (shouldEnd + triangleEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ ( Tuple (setTriangleBegTime i.eventStart acc)
                        $ fold
                            ( map
                                ( \pd ->
                                    let
                                      pdpi = pd * pi
                                    in
                                      filled
                                        ( fillColor
                                            (whiteRGBA ((time - i.eventStart) / standardIntro))
                                        )
                                        ( circle
                                            (sinp w pdpi)
                                            (cosp h pdpi)
                                            cw
                                        )
                                )
                                [ 0.0, 2.0 / 3.0, 4.0 / 3.0 ]
                            )
                    )
              | top
                  == Nothing
                  && dAcc
                      (sqToRect (sinp w (0.0 * pi)) (cosp h (0.0 * pi)) cw) = nextTriangle acc i (\t -> ((Just t) +> left +> right +> empty)) time
              | left
                  == Nothing
                  && dAcc
                      (sqToRect (sinp w (2.0 * pi / 3.0)) (cosp h (2.0 * pi / 3.0)) cw) = nextTriangle acc i (\t -> (top +> (Just t) +> right +> empty)) time
              | right
                  == Nothing
                  && dAcc
                      (sqToRect (sinp w (4.0 * pi / 3.0)) (cosp h (4.0 * pi / 3.0)) cw) = nextTriangle acc i (\t -> (top +> left +> (Just t) +> empty)) time
              | otherwise =
                pure
                  $ ( Tuple (setTriangleVector v acc)
                        $ fold
                            ( map
                                ( \(Tuple pd (Tuple bt n)) ->
                                    let
                                      pdpi = (pd + maybe 0.0 (\s -> (time - foldl max 0.0 s) `pow` 1.6) sqv) * pi

                                      mif = timeToMusicalInfo (time - quaver)

                                      r
                                        | mif.beat - bt < 0.3 = cw
                                        | mif.beat - bt < 0.5 = cw * (1.0 + calcSlope 0.3 0.0 0.5 0.3 (mif.beat - bt))
                                        | mif.beat - bt < 1.0 = cw * (1.0 + calcSlope 0.5 0.3 1.0 0.0 (mif.beat - bt))
                                        | otherwise = cw
                                    in
                                      filled
                                        ( fillColor
                                            ( whiteRGBA
                                                ( case n of
                                                    Nothing -> 1.0
                                                    Just n' -> maybe' (\_ -> max 0.4 (1.0 - (0.6 * (time - n') / standardPress))) (\s -> let mx = foldl max 0.0 s in calcSlope mx (if n' == mx then 1.0 else 0.4) (mx + standardOutro) 0.0 time) sqv
                                                )
                                            )
                                        )
                                        ( circle
                                            (sinp w pdpi)
                                            (cosp h pdpi)
                                            r
                                        )
                                )
                                [ Tuple 0.0 (Tuple 0.0 top)
                                , Tuple (2.0 / 3.0) (Tuple 2.0 left)
                                , Tuple (4.0 / 3.0) (Tuple 1.0 right)
                                ]
                            )
                    )
          in
            o
        Square v ->
          let
            sqv = sequence v

            c1 = 0.2

            c2 = 0.8

            topLeft = V.index v d0

            topRight = V.index v d1

            bottomLeft = V.index v d2

            bottomRight = V.index v d3

            cw = (min w h) / 17.0

            twoCw = 2.0 * cw

            nextSquare = nextObj Square

            o
              | Just s <- sqv
              , shouldEnd <- standardOutro + squareTravel + foldl max 0.0 s
              , time > shouldEnd = newCanvas i (setSquareEndTime (shouldEnd + squareEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ ( Tuple (setSquareBegTime i.eventStart acc)
                        $ fold
                            ( map
                                ( \{ x, y } ->
                                    filled (fillColor (whiteRGBA ((time - i.eventStart) / standardIntro)))
                                      (circle (x * w) (y * h) cw)
                                )
                                [ { x: c1, y: c1 }, { x: c2, y: c1 }, { x: c1, y: c2 }, { x: c2, y: c2 } ]
                            )
                    )
              | topLeft
                  == Nothing
                  && dAcc
                      (sqToRect (c1 * w) (c1 * h) cw) = nextSquare acc i (\t -> ((Just t) +> topRight +> bottomLeft +> bottomRight +> empty)) time
              | topRight
                  == Nothing
                  && dAcc
                      (sqToRect (c2 * w) (c1 * h) cw) = nextSquare acc i (\t -> (topLeft +> (Just t) +> bottomLeft +> bottomRight +> empty)) time
              | bottomLeft
                  == Nothing
                  && dAcc
                      (sqToRect (c1 * w) (c2 * h) cw) = nextSquare acc i (\t -> (topLeft +> topRight +> (Just t) +> bottomRight +> empty)) time
              | bottomRight
                  == Nothing
                  && dAcc
                      (sqToRect (c2 * w) (c2 * h) cw) = nextSquare acc i (\t -> (topLeft +> topRight +> bottomLeft +> (Just t) +> empty)) time
              | otherwise =
                pure
                  $ ( Tuple (setSquareVector v acc)
                        $ fold
                            ( map
                                ( \(Tuple { x0, y0, x1, y1 } n) ->
                                    let
                                      sqvT = maybe 0.0 (\s -> (time - foldl max 0.0 s)) sqv

                                      r = case n of
                                        Nothing -> cw
                                        Just n' -> pressEffect cw standardPress (time - n')
                                    in
                                      filled
                                        ( fillColor
                                            ( whiteRGBA
                                                ( case n of
                                                    Nothing -> 1.0
                                                    Just n' -> maybe' (\_ -> max 0.4 (1.0 - (0.6 * (time - n') / standardPress))) (\s -> let mx = foldl max 0.0 s in calcSlope mx (if n' == mx then 1.0 else 0.4) (mx + standardOutro) 0.0 time) sqv
                                                )
                                            )
                                        )
                                        ( circle
                                            ( case n of
                                                Nothing -> x0 * w
                                                Just n' -> w * (if (sqvT < squareTravel) then (calcSlope n' x0 (n' + squareTravel) 0.5 (min (n' + squareTravel) time)) else (calcSlope squareTravel (0.5) (standardOutro + squareTravel) x1 sqvT))
                                            )
                                            ( case n of
                                                Nothing -> y0 * h
                                                Just n' -> h * (if (sqvT < squareTravel) then (calcSlope n' y0 (n' + squareTravel) 0.5 (min (n' + squareTravel) time)) else (calcSlope squareTravel (0.5) (standardOutro + squareTravel) y1 sqvT))
                                            )
                                            r
                                        )
                                )
                                [ Tuple { x0: c1, y0: c1, x1: 1.1, y1: 1.1 } topLeft
                                , Tuple { x0: c2, y0: c1, x1: -0.1, y1: 1.1 } topRight
                                , Tuple { x0: c1, y0: c2, x1: 1.1, y1: -0.1 } bottomLeft
                                , Tuple { x0: c2, y0: c2, x1: -0.1, y1: -0.1 } bottomRight
                                ]
                            )
                    )
          in
            o
        Gears v ->
          let
            sqv = sequence v

            gear0 = V.index v d0

            gear1 = V.index v d1

            gear2 = V.index v d2

            gear3 = V.index v d3

            cw = (min w h) / 17.0

            cw0 = cw

            cw1 = cw0 + (w * 0.05)

            cw2 = cw1 + (w * 0.05)

            cw3 = cw2 + (w * 0.05)

            cg0 = 0.0

            cg1 = 0.3 * pi

            cg2 = 0.9 * pi

            cg3 = 1.8 * pi

            nextGear = nextObj Gears

            dArc cc =
              acc.initiatedClick
                && ( maybe false
                      ( \{ x, y } ->
                          if x == 0.0 && y == 0.0 then
                            false
                          else
                            ( let
                                hyp = pythag (x - w / 2.0) (y - h / 2.0)

                                lb = cc - (w * 0.025)

                                ub = cc + (w * 0.025)
                              in
                                hyp >= lb && hyp <= ub
                            )
                      )
                      acc.mousePosition
                  )

            gear2arc opq mcw gp =
              outlined (outlineColor (whiteRGBA opq) <> lineWidth (w * 0.05))
                (arc (w / 2.0) (h / 2.0) gp (gp + (calcSlope 0.0 1.0 w 2.0 mcw * pi)) mcw)

            o
              | Just s <- sqv
              , shouldEnd <- standardOutro + gearStay + foldl max 0.0 s
              , time > shouldEnd = newCanvas i (setGearsEndTime (shouldEnd + gearsEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ ( Tuple (setGearsBegTime i.eventStart acc)
                        $ fold
                            ( map
                                (\(Tuple mcw gs) -> gear2arc ((time - i.eventStart) / standardIntro) mcw gs)
                                [ Tuple cw0 cg0, Tuple cw1 cg1, Tuple cw2 cg2, Tuple cw3 cg3 ]
                            )
                    )
              | gear0
                  == Nothing
                  && dArc cw0 = nextGear acc i (\t -> ((Just t) +> gear1 +> gear2 +> gear3 +> empty)) time
              | gear1
                  == Nothing
                  && dArc cw1 = nextGear acc i (\t -> (gear0 +> (Just t) +> gear2 +> gear3 +> empty)) time
              | gear2
                  == Nothing
                  && dArc cw2 = nextGear acc i (\t -> (gear0 +> gear1 +> (Just t) +> gear3 +> empty)) time
              | gear3
                  == Nothing
                  && dArc cw3 = nextGear acc i (\t -> (gear0 +> gear1 +> gear2 +> (Just t) +> empty)) time
              | otherwise =
                pure
                  $ ( Tuple (setGearsVector v acc)
                        $ fold
                            ( map
                                ( \(Tuple (Tuple mcw gp) (Tuple gd n)) ->
                                    let
                                      nowT = maybe 0.0 (\s -> (time - foldl max 0.0 s)) sqv
                                    in
                                      gear2arc (maybe 1.0 (\s -> let mx = foldl max 0.0 s in (max 0.0 (min 1.0 (calcSlope (mx + gearStay) 1.0 (mx + gearStay + standardOutro) 0.0 time)))) sqv) mcw
                                        ( case n of
                                            Nothing -> gp
                                            Just n' -> (if gd then (+) else (-)) gp (gearSpinF (time - n'))
                                        )
                                )
                                [ Tuple (Tuple cw0 cg0) (Tuple gearDir0 gear0)
                                , Tuple (Tuple cw1 cg1) (Tuple gearDir1 gear1)
                                , Tuple (Tuple cw2 cg2) (Tuple gearDir2 gear2)
                                , Tuple (Tuple cw3 cg3) (Tuple gearDir3 gear3)
                                ]
                            )
                    )
          in
            o
        Motion prevT lr ->
          let
            cw = (min w h) / 16.0

            xp = eix w acc lr

            yp = eiy h acc lr

            textNormal = 1.0

            smp = \(Tuple { x: x0, y: y0 } { x: x1, y: y1 }) ->
              setMotionPos { x: (x1 / w), y: (y1 / h) }

            instr
              | time < i.eventStart + standardIntro + textNormal + standardOutro =
                let
                  opacity
                    | time < i.eventStart + standardIntro = calcSlope i.eventStart 0.0 (i.eventStart + standardIntro) 1.0 time
                    | time < i.eventStart + standardIntro + textNormal = 1.0
                    | time < i.eventStart + standardIntro + textNormal + standardOutro = calcSlope (i.eventStart + standardIntro + textNormal) 1.0 (i.eventStart + standardIntro + textNormal + standardOutro) 0.0 time
                    | otherwise = 0.0
                in
                  text
                    (font sansSerif 16 italic)
                    (w * 0.31)
                    (h * 0.31)
                    (fillColor (whiteRGBA opacity))
                    "(move the circle)"
              | otherwise = mempty

            o
              | shouldEnd <- i.eventStart + standardIntro + motionNormal + standardOutro
              , time > shouldEnd = newCanvas i (setMotionEndTime (shouldEnd + motionEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ Tuple (setMotionBegTime i.eventStart acc)
                      ( instr
                          <> filled
                              (fillColor (whiteRGBA (min 1.0 $ (time - i.eventStart) / standardIntro)))
                              (circle xp yp cw)
                      )
              | otherwise = case lr of
                Left _ ->
                  if needsToFollow xp yp cw acc lr then
                    let
                      oss = { x: (mouseOrBust acc.mousePosition).x - xp, y: (mouseOrBust acc.mousePosition).y - yp }

                      newX = calibrateX acc oss

                      newY = calibrateY acc oss

                      npt = { x: newX, y: newY }

                      ppt = { x: xp, y: yp }

                      newPt = Right (Tuple npt oss)
                    in
                      motionMaker (Motion (Just ppt) newPt) newX newY cw (smp (Tuple ppt npt) acc) i instr time
                  else
                    let npt = { x: xp, y: yp } in motionMaker (Motion prevT lr) xp yp cw (smp (Tuple (fromMaybe npt prevT) npt) acc) i instr time
                Right (Tuple old offset) ->
                  if needsToStopFollowing acc lr then
                    let
                      newPt = (Left { x: xp / w, y: yp / h })

                      nxp = (eix w acc newPt)

                      nyp = (eiy h acc newPt)
                    in
                      motionMaker (Motion (Just old) newPt) nxp nyp cw (smp (Tuple old { x: nxp, y: nyp }) acc) i instr time
                  else
                    let npt = { x: xp, y: yp } in motionMaker (Motion (Just old) (Right (Tuple npt offset))) xp yp cw (smp (Tuple old npt) acc) i instr time
          in
            o
        Rise v ->
          let
            one = V.index v d0

            two = V.index v d1

            three = V.index v d2

            four = V.index v d3

            five = V.index v d4

            six = V.index v d5

            cw = (min w h) / 21.0

            nextRise = nextObj Rise

            twoCw = 2.0 * cw

            tillIntroEnd = i.eventStart + standardIntro

            tillNormal = tillIntroEnd + riseNormal

            normalizedTime = pnCurve 1.2 $ (time - tillIntroEnd) / riseNormal

            heightNow = h * (calcSlope 0.0 0.9 1.0 0.1 (min normalizedTime 1.0))

            o
              | shouldEnd <- tillNormal + standardOutro
              , time > shouldEnd = newCanvas i (setRiseEndTime (shouldEnd + riseEndTimeBleed) acc) time
              | time < tillIntroEnd =
                pure
                  $ Tuple (setRiseBegTime i.eventStart acc)
                      ( fold
                          ( map
                              ( \xp ->
                                  ( filled
                                      (fillColor (whiteRGBA (min 1.0 $ (time - i.eventStart) / standardIntro)))
                                      (circle (w * xp) (h * 0.9) cw)
                                  )
                              )
                              riseXP
                          )
                      )
              | isNothing one
                  && dAcc
                      (sqToRect (1.0 * w / 12.0) (heightNow) cw) = nextRise acc i (\t -> (Just $ Tuple heightNow time) +> two +> three +> four +> five +> six +> empty) time
              | isNothing two
                  && dAcc
                      (sqToRect (3.0 * w / 12.0) (heightNow) cw) = nextRise acc i (\t -> one +> (Just $ Tuple heightNow time) +> three +> four +> five +> six +> empty) time
              | isNothing three
                  && dAcc
                      (sqToRect (5.0 * w / 12.0) (heightNow) cw) = nextRise acc i (\t -> one +> two +> (Just $ Tuple heightNow time) +> four +> five +> six +> empty) time
              | isNothing four
                  && dAcc
                      (sqToRect (7.0 * w / 12.0) (heightNow) cw) = nextRise acc i (\t -> one +> two +> three +> (Just $ Tuple heightNow time) +> five +> six +> empty) time
              | isNothing five
                  && dAcc
                      (sqToRect (9.0 * w / 12.0) (heightNow) cw) = nextRise acc i (\t -> one +> two +> three +> four +> (Just $ Tuple heightNow time) +> six +> empty) time
              | isNothing six
                  && dAcc
                      (sqToRect (11.0 * w / 12.0) (heightNow) cw) = nextRise acc i (\t -> one +> two +> three +> four +> five +> (Just $ Tuple heightNow time) +> empty) time
              | otherwise =
                pure
                  $ Tuple (setRiseVector (map (map snd) v) acc)
                      ( fold
                          ( map
                              ( \(Tuple xp pgd) ->
                                  ( filled
                                      (fillColor (whiteRGBA (if time > tillNormal then calcSlope tillNormal 1.0 (tillNormal + standardOutro) 0.0 time else 1.0)))
                                      (circle (w * xp) (maybe heightNow fst pgd) cw)
                                  )
                              )
                              (A.zip riseXP [ one, two, three, four, five, six ])
                          )
                      )
          in
            o
        Shrink v ->
          let
            shrinkToRect (Tuple xp yp) rr = sqToRect (xp * w) (yp * h) (rr * (min w h) / 2.0)

            one = V.index v d0

            two = V.index v d1

            three = V.index v d2

            four = V.index v d3

            five = V.index v d4

            six = V.index v d5

            o
              -- todo: 1.5 magic number, change - allows for shrink
              | shouldEnd <- i.eventStart + standardIntro + shrinkNormal + standardOutro
              , time > shouldEnd = newCanvas i (setShrinkEndTime (shouldEnd + shrinkEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ ( Tuple (setShrinkBegTime i.eventStart acc)
                        $ fold
                            ( map
                                ( \(Tuple (Tuple xp yp) rr) ->
                                    filled (fillColor (whiteRGBA ((time - i.eventStart) / standardIntro)))
                                      (circle (xp * w) (yp * h) (rr * (min w h) / 2.0))
                                )
                                (V.zip (shrinkOne +> shrinkTwo +> shrinkThree +> shrinkFour +> shrinkFive +> shrinkSix +> empty) shrinkStart)
                            )
                    )
              | doingAction acc
                  (shrinkToRect shrinkOne (V.index v d0)) = paintShrinks w h acc i targetsOne v time
              | doingAction acc
                  (shrinkToRect shrinkTwo (V.index v d1)) = paintShrinks w h acc i targetsTwo v time
              | doingAction acc
                  (shrinkToRect shrinkThree (V.index v d2)) = paintShrinks w h acc i targetsThree v time
              | doingAction acc
                  (shrinkToRect shrinkFour (V.index v d3)) = paintShrinks w h acc i targetsFour v time
              | doingAction acc
                  (shrinkToRect shrinkFive (V.index v d4)) = paintShrinks w h acc i targetsFive v time
              | doingAction acc
                  (shrinkToRect shrinkSix (V.index v d5)) = paintShrinks w h acc i targetsSix v time
              | otherwise = paintShrinks w h acc i targetsDefault v time
          in
            o
        Snow a ->
          let
            stTime = i.eventStart

            nowT = time - stTime

            terminus = h * 1.1

            yp = snowYp h nowT

            o
              | not
                  ( bFoldL
                      ( \v x -> case x of
                          Nothing -> yp (maybe 100.0 (\(SnowI _ b _) -> b) (A.index snows v)) < terminus
                          (Just _) -> false
                      )
                      a
                  ) = newCanvas i (setSnowEndTime (time + snowEndTimeBleed) acc) time
              | otherwise = snowRecurser makeCanvas i w h acc stTime time a
          in
            o
        Bells a ->
          let
            stTime = i.eventStart

            nowT = time - stTime

            o
              | shouldEnd <- i.eventStart + standardIntro + bellsNormal + standardOutro
              , time > shouldEnd = newCanvas i (setBellsEndTime (shouldEnd + bellsEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ Tuple (setBellsBegTime (i.eventStart) acc)
                      ( makeBells w h (A.replicate 24 (Tuple 1.0 (min 1.0 $ (time - i.eventStart) / standardIntro)))
                      )
              | otherwise = bellsRecurser makeCanvas i w h acc stTime time a
          in
            o
        Large v ->
          let
            cw = h / 1.2

            cst = 0.0 - cw - 10.0

            ced = w + cw + 1.0

            spn = ced - cst

            cpos = calcSlope 0.0 cst 1.0 ced (pnCurve 1.2 $ (time - i.eventStart) / largeCrossing)

            newV = maybe v (\mp -> if acc.initiatedClick then (Tuple mp time) : v else v) acc.mousePosition

            o
              | shouldEnd <- i.eventStart + largeCrossing + standardOutro
              , time > shouldEnd = newCanvas i (setLargeEndTime (shouldEnd + largeEndTimeBleed) acc) time
              | otherwise =
                pure
                  $ Tuple
                      ( (setLargeList (map snd newV) (setLargeBegTime i.eventStart acc))
                          { activity =
                            SilentNightPlayer
                              ( i
                                  { playerEvents = [ Large newV ] <> A.drop 1 i.playerEvents
                                  }
                              )
                          }
                      )
                      (filled (fillColor $ rgb 255 255 255) (circle cpos (h / 2.0) cw) <> fold (map (\(Tuple { x, y } tm) -> filled (fillColor $ rgb 0 0 0) (circle (calcSlope tm x (tm + largeCrossing) (x + spn) time) y ((min w h) * (min 0.3 $ 0.03 * (time - tm))))) newV))
          in
            o
