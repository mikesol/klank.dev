module Klank.Studio.SilentNight where

import Prelude
import Control.Monad.Reader (runReader)
import Data.Array as A
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Int (floor, toNumber)
import Data.List (List(..))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2, D6)
import Data.Vec (Vec, empty, fill, (+>))
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Now (now)
import Effect.Random (random)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AV(..), CanvasInfo(..), defaultExporter, gain_, makePeriodicWave, runInBrowser_, speaker')
import Foreign.Object as O
import Graphics.Painting.Font (FontOptions, bold, italic)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Klank.Dev.Util (makeBuffersKeepingCache)
import Klank.Studio.SilentNight.AudioFiles (audioFiles)
import Klank.Studio.SilentNight.Bells (baseBells, bells)
import Klank.Studio.SilentNight.Canvas (makeCanvas)
import Klank.Studio.SilentNight.Gear (gears)
import Klank.Studio.SilentNight.Heart (heart)
import Klank.Studio.SilentNight.Interactions (Interactions, getInteractivity, interactionLog)
import Klank.Studio.SilentNight.Intro (introBG)
import Klank.Studio.SilentNight.Large (large)
import Klank.Studio.SilentNight.Motion (motion)
import Klank.Studio.SilentNight.Rise (rise)
import Klank.Studio.SilentNight.Shrink (shrink)
import Klank.Studio.SilentNight.Snow (baseSnows, snow, snowL)
import Klank.Studio.SilentNight.Square (square)
import Klank.Studio.SilentNight.Triangle (triangle)
import Klank.Studio.SilentNight.Types (AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (Activity(..), MusicM, PlayerEvent(..), SilentNightAccumulator, defaultAudioMarkers)
import Klank.Studio.SilentNight.Types.Intro (HarmChooserStep(..), Verse(..), VerseChoice(..))
import Klank.Studio.SilentNight.Util (crotchet, pieceInMeasures, silentNightEngineInfo, tempo, timeToMusicalInfo, toNel)
import Klank.Studio.SilentNight.Verse (verses)
import Klank.Weblib.Studio as Studio
import Math ((%))
import Type.Klank.Dev (Klank'')

silentNight :: MusicM AudioListD2
silentNight =
  fold
    <$> sequence
        [ introBG
        , verses
        , triangle
        , square
        , motion
        , snow
        , gears
        , rise
        , large
        , shrink
        , bells
        , heart
        ]

boldItalic :: FontOptions
boldItalic = bold <> italic

calcMainStarts :: Number -> Number
calcMainStarts t =
  let
    -- where the piece starts, as there is a full bar rest
    -- so for example, at q=72, t=2.5 will yield 0.0
    placeInGrid = t - (3.0 * crotchet)

    diffFrom12 = ((placeInGrid * tempo / 60.0) % 12.0)

    -- zoom out if we're a full measure behind
    hd = if diffFrom12 > 8.3 then 24.0 else 12.0

    -- ie 12.0 - 5.0 = 7.0 beats remaining
    beatsRemaining = hd - diffFrom12
  in
    t + (beatsRemaining * crotchet)

scene :: Interactions -> Array PlayerEvent -> SilentNightAccumulator -> CanvasInfo -> Number -> Behavior (AV D2 SilentNightAccumulator)
scene inter evts acc' ci'@(CanvasInfo ci) time = go <$> (interactionLog inter)
  where
  go p =
    AV
      { audio:
          Just
            ( speaker'
                ( gain_ "globalMasterFader" (1.0) $ toNel players
                )
            )
      , visual:
          Just
            { words: Nil
            , painting: const cvs
            }
      , accumulator: vizAcc
      }
    where
    -- 10.0 hardcoded for now
    inCoda = maybe false (\v -> time >= v + pieceInMeasures - 15.0) acc'.mainStarts

    codizedActivity =
      if inCoda && not acc'.initiatedCoda then
        ( ( case _ of
              SilentNightPlayer i ->
                SilentNightPlayer
                  i
                    { playerEvents = A.take 1 i.playerEvents <> [ Heart Nothing ]
                    }
              x -> x
          )
            acc'.activity
        )
      else
        acc'.activity

    acc =
      acc'
        { mousePosition =
          ( \{ x, y } ->
              { x: x - ci.boundingClientRect.x, y: y - ci.boundingClientRect.y
              }
          )
            <$> p.referencePosition
        , initiatedClick = (_.id <$> A.head p.interactions) /= acc'.curClickId
        , inClick = p.nInteractions /= 0
        , curClickId = _.id <$> A.head p.interactions
        , initiatedCoda = inCoda
        , activity = codizedActivity
        , mainStarts =
          if acc'.mainStarts /= Nothing then
            acc'.mainStarts
          else
            calcMainStarts <$> acc'.introEnds
        }

    (Tuple vizAcc cvs) = runReader (makeCanvas acc time) { evts, w: ci.w, h: ci.h }

    players =
      runReader silentNight
        { initiatedCoda: vizAcc.initiatedCoda
        , mainStarts: vizAcc.mainStarts
        , audioMarkers: vizAcc.audioMarkers
        , time: time
        , musicalInfo: timeToMusicalInfo time
        , verseStarts: vizAcc.verseStarts
        }

allPlayerEvent =
  [ Snow baseSnows
  , Motion Nothing (Left { x: 0.18, y: 0.18 })
  , Triangle (fill (const Nothing))
  , Gears (fill (const Nothing))
  , Square (fill (const Nothing))
  , Bells baseBells
  , Large Nil
  , Shrink shrinkStart
  , Rise (fill (const Nothing))
  ] ::
    Array PlayerEvent

allPlayerEvent' =
  [ Rise (fill (const Nothing)) -- not working, change 
  , Bells baseBells -- need to introspect
  , Shrink shrinkStart
  , Triangle (fill (const Nothing))
  , Gears (fill (const Nothing))
  , Snow baseSnows
  , Motion Nothing (Left { x: 0.18, y: 0.18 })
  , Square (fill (const Nothing))
  , Large Nil
  ] ::
    Array PlayerEvent

shrinkStart = 0.1 +> 0.15 +> 0.6 +> 0.3 +> 0.35 +> 0.05 +> empty :: Vec D6 Number

acA = Intro :: Activity

acB = HarmChooser { step: Row1Animation { startsAt: 0.0 } } :: Activity

acx :: Array PlayerEvent -> Activity
acx a =
  SilentNightPlayer
    { verse: Verse1
    , verseOne: VersionOne
    , verseTwo: VersionTwo
    , verseThree: VersionThree
    , playerEvents: a
    , eventStart: 0.0
    }

acC = acx [ Triangle (fill $ const Nothing), Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

acD = acx [ Square (fill $ const Nothing), Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

acE = acx [ Motion Nothing (Left { x: 0.18, y: 0.18 }), Square (fill $ const Nothing) ] :: Activity

acF = acx [ Rise (fill $ const Nothing), Square (fill $ const Nothing) ] :: Activity

acG = acx [ Snow (L.fromFoldable $ A.replicate snowL Nothing), Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

acH = acx [ Shrink shrinkStart, Square (fill $ const Nothing) ] :: Activity

acI = acx [ Bells (L.fromFoldable $ A.replicate 24 Nil), Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

acJ = acx [ Large Nil, Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

acK = acx [ Gears (fill $ const Nothing), Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

acL = acx [ Heart Nothing, Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

shuffle :: forall a. Array a -> Effect (Array a)
shuffle x = go 0 x
  where
  l = A.length x

  go 100 a = pure a

  go z a = do
    rd <- random
    let
      idx = floor (toNumber l * rd)
    go (z + 1) (A.drop idx a <> A.take idx a)

playMe :: Klank'' SilentNightAccumulator Unit
playMe =
  { run:
      runInBrowser_ do
        inter <- getInteractivity
        (Milliseconds timeNow) <- map unInstant now
        evts' <-  {-shuffle-} pure allPlayerEvent
        evts <-
          sequence
            $ A.mapWithIndex
                ( \idx i -> do
                    n <- random
                    pure [ NoEvent (if idx == 0 then n * 2.0 + 5.0 else n * 5.0 + 2.0), i ]
                )
                evts'
        pure $ scene inter (join evts)
  , accumulator:
      \res _ ->
        res
          { initiatedClick: false
          , curClickId: Nothing
          , mousePosition: Nothing
          , activity: Intro
          , inClick: false
          , initiatedCoda: false
          , mainStarts: Nothing
          , introEnds: Nothing
          , verseStarts: { one: Nothing, two: Nothing, three: Nothing }
          , audioMarkers: defaultAudioMarkers
          }
  , exporter: defaultExporter
  , periodicWaves:
      \ctx _ res rej -> do
        smooth <-
          makePeriodicWave ctx
            (0.5 +> 0.25 +> -0.1 +> 0.07 +> 0.1 +> empty)
            (0.2 +> 0.1 +> 0.01 +> -0.03 +> -0.1 +> empty)
        rich <-
          makePeriodicWave ctx
            (0.1 +> 0.3 +> -0.1 +> 0.1 +> 0.2 +> 0.05 +> 0.1 +> 0.01 +> empty)
            (0.3 +> -0.5 +> -0.4 +> -0.03 +> -0.15 +> -0.2 +> -0.05 +> -0.02 +> empty)
        res $ O.fromFoldable [ Tuple "smooth" smooth, Tuple "rich" rich ]
  -- All sounds from freesound.org are attributed to their authors.
  , engineInfo: \res _ -> res silentNightEngineInfo
  , buffers: makeBuffersKeepingCache 20 audioFiles
  , floatArrays: \prev res _ -> res prev
  , enableMicrophone: false
  , enableCamera: false
  , recorders: \_ _ prev res _ -> res prev
  , tracks: \prev res _ -> res prev
  , images: \prev res _ -> res prev
  , videos: \prev res _ -> res prev
  , canvases: \prev res _ -> res prev
  , worklets: \prev res _ -> res prev
  , webcamCache: \_ _ -> L.take 10
  }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (Studio.component (pure playMe)) unit body
