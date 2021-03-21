module Klank.Weblib.Studio where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Timer (IntervalId, clearInterval)
import FRP.Behavior.Audio (AudioContext, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, MediaRecorder, RecorderSignature)
import Foreign.Object (Object)
import Foreign.Object as O
import Halogen (ClassName(..), SubscriptionId)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Klank.Weblib.AppAction (Action(..))
import Klank.Weblib.CanvasComponent as CanvasComponent
import Klank.Weblib.ClickPlayModal (clickPlay)
import Klank.Weblib.LoadingModal (loading)
import Klank.Weblib.Shared (playKlank, stopper, getInitialAccumulator, canvasDimensionHack)
import Type.Klank.Dev (Klank'')
import Type.Proxy (Proxy(..))
import Web.HTML (HTMLCanvasElement, HTMLImageElement, HTMLVideoElement)

type EffectfulKlank accumulator env
  = Effect (Klank'' accumulator env)

_canvas = Proxy :: Proxy "canvas"

type State accumulator env
  = { effectfulKlank :: Effect (Klank'' accumulator env)
    , isPlaying :: Maybe Boolean
    , downloadProgress :: Maybe Number
    , stopFn :: Maybe (Effect Unit)
    , progressIntervalId :: Maybe IntervalId
    , audioCtx :: Maybe AudioContext
    , initialAccumulator :: Maybe accumulator
    , worklets :: Array String
    , loadingModalOpen :: Boolean
    , playModalOpen :: Boolean
    , tracks :: Object BrowserAudioTrack
    , recorders :: Object (RecorderSignature MediaRecorder)
    , buffers :: Object BrowserAudioBuffer
    , floatArrays :: Object BrowserFloatArray
    , periodicWaves :: Object BrowserPeriodicWave
    , images :: Object HTMLImageElement
    , videos :: Object HTMLVideoElement
    , canvases :: Object HTMLCanvasElement
    , playerSubscriptionId :: Maybe SubscriptionId
    }

data WhichCanvas
  = Canvas

derive instance genericWhichCanvas :: Generic WhichCanvas _

derive instance eqWhichCanvas :: Eq WhichCanvas

instance showWhichCanvas :: Show WhichCanvas where
  show = genericShow

instance ordWhichCanvas :: Ord WhichCanvas where
  compare a b = compare (show a) (show b)

type ChildSlots
  = ( canvas :: CanvasComponent.Slot WhichCanvas )

component :: forall accumulator env q i o m. MonadAff m => EffectfulKlank accumulator env -> H.Component q i o m
component effectfulKlank =
  H.mkComponent
    { initialState:
        \_ ->
          { effectfulKlank
          , isPlaying: Nothing
          , stopFn: Nothing
          , progressIntervalId: Nothing
          , audioCtx: Nothing
          , downloadProgress: Nothing
          , loadingModalOpen: true
          , playModalOpen: false
          , initialAccumulator: Nothing
          , worklets: []
          , tracks: O.empty
          , buffers: O.empty
          , recorders: O.empty
          , floatArrays: O.empty
          , periodicWaves: O.empty
          , images: O.empty
          , videos: O.empty
          , canvases: O.empty
          , playerSubscriptionId: Nothing
          }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

render :: forall accumulator env m. MonadAff m => (State accumulator env) -> H.ComponentHTML Action ChildSlots m
render { loadingModalOpen
, playModalOpen
, isPlaying
, downloadProgress
} =
  HH.div [ HP.classes $ map ClassName [ "h-screen", "w-screen" ] ]
    ( [ HH.div
          [ HP.classes $ map ClassName [ "h-full", "w-full", "flex", "flex-col" ] ]
          [ HH.slot _canvas Canvas CanvasComponent.component
              {}
              absurd
          ]
      , clickPlay
          { open: playModalOpen
          }
      , loading
          { open: loadingModalOpen
          , progressSoFar: fromMaybe 0.0 downloadProgress
          , progressMax: 100.0
          }
      ]
        <> ( maybe []
              ( \ip ->
                  [ HH.div [ HP.classes $ map ClassName [ "modal", "fixed", "pr-8", "pb-8", "right-0", "bottom-0" ] ]
                      [ HH.div [ HP.classes $ map ClassName [ "bg-white", "rounded-full" ] ]
                          [ HH.i [ HP.classes $ map ClassName [ "fas", "fa-9x", "cursor-pointer", "z-40", (if ip then "fa-stop-circle" else "fa-play-circle") ], HE.onClick \_ -> if ip then PlayKlankFromStopButton else PlayKlankFromPlayButton ] []
                          ]
                      ]
                  ]
              )
              isPlaying
          )
    )

handleAction :: forall accumulator env o m. MonadAff m => Action -> H.HalogenM (State accumulator env) Action ChildSlots o m Unit
handleAction = case _ of
  PlayKlankFromModal -> do
    H.modify_ (_ { playModalOpen = false })
    playKlank
      { playStartFailed: PlayStartFailed
      , playStartSucceeded: PlayStartSucceeded
      , recordingRegistered: RecordingRegistered
      }
  PlayKlankFromPlayButton -> do
    playKlank
      { playStartFailed: PlayStartFailed
      , playStartSucceeded: PlayStartSucceeded
      , recordingRegistered: RecordingRegistered
      }
  ProgressUpdate n -> do
    H.modify_ (_ { downloadProgress = Just n })
  PlayKlankFromStopButton -> do
    stopper
    H.modify_ (_ { isPlaying = Just false })
  Initialize -> do
    H.liftEffect canvasDimensionHack
    initialAccumulator <- H.liftEffect $ getInitialAccumulator Nothing Just
    H.modify_ (_ { initialAccumulator = initialAccumulator })
    H.modify_
      ( _
          { playModalOpen = true
          , loadingModalOpen = false
          }
      )
    sid <- H.gets _.progressIntervalId
    H.modify_ (_ { progressIntervalId = Nothing })
    maybe (pure unit) (H.liftEffect <<< clearInterval) sid
    pure mempty
  PlayStartSucceeded playerInfo -> do
    H.modify_
      ( _
          { stopFn = playerInfo.stopFn
          , isPlaying = playerInfo.isPlaying
          , periodicWaves = playerInfo.periodicWaves
          , audioCtx = playerInfo.audioCtx
          , recorders = playerInfo.recorders
          , worklets = playerInfo.worklets
          , tracks = playerInfo.tracks
          , buffers = playerInfo.buffers
          , floatArrays = playerInfo.floatArrays
          }
      )
  PlayStartFailed s -> pure mempty
  RecordingRegistered k v -> pure mempty
