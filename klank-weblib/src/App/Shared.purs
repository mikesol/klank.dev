module Klank.Weblib.Shared where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, try)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import FRP.Behavior.Audio (AudioContext, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserMicrophone, BrowserPeriodicWave, MediaRecorder, RecorderSignature, audioWorkletAddModule, makeAudioContext)
import Foreign.Object (Object)
import Foreign.Object as O
import Graphics.Canvas (CanvasElement)
import Halogen as H
import Halogen.Subscription as HS
import Type.Klank.Dev (Klank'')
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (HTMLCanvasElement, HTMLImageElement, HTMLVideoElement)

type PlayerUpdate
  = { stopFn :: Maybe (Effect Unit)
    , isPlaying :: Maybe Boolean
    , periodicWaves :: Object BrowserPeriodicWave
    , audioCtx :: Maybe AudioContext
    , recorders :: Object (RecorderSignature MediaRecorder)
    , worklets :: Array String
    , tracks :: Object BrowserAudioTrack
    , buffers :: Object BrowserAudioBuffer
    , floatArrays :: Object BrowserFloatArray
    }

type PlayKlank =
  forall 
    (m :: Type -> Type)
    (output :: Type)
    (slots :: Row Type)
    (accumulator :: Type)
    (env :: Type)
    (action :: Type)
    (r :: Row Type).
    MonadEffect m =>
    MonadAff m =>
    {
      playStartFailed :: String -> action
    , playStartSucceeded :: PlayerUpdate -> action
    , recordingRegistered :: String -> String -> action
    } ->
    H.HalogenM {
      audioCtx :: Maybe AudioContext
    , buffers :: Object BrowserAudioBuffer
    , canvases :: Object HTMLCanvasElement
    , effectfulKlank :: Effect (Klank'' accumulator env)
    , floatArrays :: Object BrowserFloatArray
    , images :: Object HTMLImageElement
    , periodicWaves :: Object BrowserPeriodicWave
    , playerSubscriptionId :: Maybe H.SubscriptionId
    , recorders :: Object (MediaRecorder -> Effect Unit)
    , stopFn :: Maybe (Effect Unit)
    , tracks :: Object BrowserAudioTrack
    , videos :: Object HTMLVideoElement
    , worklets :: Array String
    | r
    }
    action slots output m Unit
foreign import data BrowserMediaStream :: Type

foreign import loadCustomAudioNodes :: AudioContext -> Effect (Promise Unit)
foreign import data BrowserCamera :: Type
browserMediaStreamToBrowserMicrophone :: BrowserMediaStream -> BrowserMicrophone
browserMediaStreamToBrowserMicrophone = unsafeCoerce

browserMediaStreamToBrowserCamera :: BrowserMediaStream -> BrowserCamera
browserMediaStreamToBrowserCamera = unsafeCoerce

getMicrophoneAndCamera :: Boolean -> Boolean -> Aff { microphone :: Maybe BrowserMicrophone, camera :: Maybe BrowserCamera }
getMicrophoneAndCamera audio video =
  ( \i ->
      { microphone: if audio then Just $ browserMediaStreamToBrowserMicrophone i else Nothing
      , camera: if video then Just $ browserMediaStreamToBrowserCamera i else Nothing
      }
  )
    <$> toAffE (getBrowserMediaStreamImpl audio video)
foreign import getBrowserMediaStreamImpl :: Boolean -> Boolean -> Effect (Promise BrowserMediaStream)
foreign import cameraToVideo :: BrowserCamera -> Effect HTMLVideoElement
affable :: forall a. ((a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit) -> Aff a
affable f =
  makeAff \cb -> do
    f (cb <<< Right) (cb <<< Left)
    pure mempty
foreign import canvasOrBust :: Effect CanvasElement

playKlank :: PlayKlank
playKlank actions = do
  stopKlank
  oldSubId <- H.gets _.playerSubscriptionId
  maybe (pure unit) H.unsubscribe oldSubId
  klank' <- H.gets _.effectfulKlank
  klank <- H.liftEffect klank'
  ctx <- H.liftEffect makeAudioContext
  H.liftAff (toAffE $ loadCustomAudioNodes ctx)
  prevWorklets <- H.gets _.worklets
  prevTracks <- H.gets _.tracks
  prevRecorders <- H.gets _.recorders
  prevBuffers <- H.gets _.buffers
  prevFloatArrays <- H.gets _.floatArrays
  prevPeriodicWaves <- H.gets _.periodicWaves
  prevImages <- H.gets _.images
  prevVideos <- H.gets _.videos
  prevCanvases <- H.gets _.canvases
  -- steps
  -- 1. refactor all of the aff stuff below to subscribe function
  -- body of this subscription
  -- note that the subscription won't have a finalizer for now
  -- may lead to wanky memory, we can look into that...
  -- 2. create actions for everything below.
  { emitter, listener } <- H.liftEffect HS.create
  subId <- H.subscribe emitter
  H.liftAff do
    res <-
      try do
        { microphone, camera } <- case klank.enableMicrophone || klank.enableCamera of 
          true -> getMicrophoneAndCamera klank.enableMicrophone klank.enableCamera
          false -> pure { microphone: Nothing, camera: Nothing }
        let
          microphones = maybe O.empty (O.singleton "microphone") microphone
        cameraAsVideo <- case camera of
          Nothing -> pure Nothing
          Just c -> Just <$> H.liftEffect (cameraToVideo c)
        accumulator <- affable $ klank.accumulator
        worklets <- (affable $ klank.worklets prevWorklets)
        -------------
        ----- maybe it's just superstition
        ---- but i think this didn't work unless I explicitly asked for a variable `o`
        --- instead of _ <-
        --------- weird...
        o <- traverse (toAffE <<< audioWorkletAddModule ctx) worklets
        tracks <- affable $ klank.tracks prevTracks
        buffers <- affable $ klank.buffers ctx prevBuffers
        images <- affable $ klank.images prevImages
        videos <- affable $ klank.videos prevVideos
        sourceCanvases <- affable $ klank.canvases prevCanvases
        recorders <-
          affable
            $ klank.recorders
                O.empty
                ( \k v -> HS.notify listener (actions.recordingRegistered k v)
                )
                prevRecorders
        floatArrays <- affable $ klank.floatArrays prevFloatArrays
        periodicWaves <- affable $ klank.periodicWaves ctx prevPeriodicWaves
        engineInfo <- affable $ klank.engineInfo
        turnMeOff <-
          H.liftEffect
            ( klank.run
                accumulator
                ctx
                engineInfo
                { microphones, recorders, tracks, buffers, floatArrays, periodicWaves }
                { canvases: O.singleton "canvas" canvasOrBust
                , images: images
                , videos: videos
                , cameras:
                    case cameraAsVideo of
                      Nothing -> O.empty
                      Just c -> O.singleton "camera" { camera: c, cache: klank.webcamCache }
                , sourceCanvases: sourceCanvases
                }
                klank.exporter
            )
        pure
          { stopFn: Just turnMeOff
          , isPlaying: Just true
          , periodicWaves: periodicWaves
          , audioCtx: Just ctx
          , recorders: recorders
          , worklets: worklets
          , tracks: tracks
          , buffers: buffers
          , floatArrays: floatArrays
          }
    H.liftEffect
      $ case res of
          Left err -> HS.notify listener (actions.playStartFailed (show err))
          Right resp -> H.liftEffect $ HS.notify listener (actions.playStartSucceeded resp)
  H.modify_ (_ { playerSubscriptionId = Just subId })
  pure unit


type StopKlank = forall m output slots action r. MonadEffect m => H.HalogenM { audioCtx :: Maybe AudioContext, playerSubscriptionId :: Maybe H.SubscriptionId, stopFn :: Maybe (Effect Unit) | r } action slots output m Unit

foreign import stopAudioContext :: AudioContext -> Effect Unit

stopKlank :: StopKlank
stopKlank = do
  sfn <- H.gets _.stopFn
  ctx <- H.gets _.audioCtx
  H.modify_ (_ { stopFn = Nothing, audioCtx = Nothing, playerSubscriptionId = Nothing })
  maybe (pure unit) H.liftEffect sfn
  maybe (pure unit) (H.liftEffect <<< stopAudioContext) ctx

foreign import canvasDimensionHack :: Effect Unit

fetchAssets :: forall m r accumulator env.
  Bind m =>
  MonadState {
    buffers :: Object BrowserAudioBuffer
  , images :: Object HTMLImageElement
  , videos :: Object HTMLVideoElement
  , canvases :: Object HTMLCanvasElement 
  , effectfulKlank :: Effect (Klank'' accumulator env)
  | r } m =>
  MonadEffect m =>
  MonadAff m =>
  m Unit
fetchAssets = do
  prevBuffers <- H.gets _.buffers
  prevImages <- H.gets _.images
  prevVideos <- H.gets _.videos
  prevCanvases <- H.gets _.canvases
  klank' <- H.gets _.effectfulKlank
  klank <- H.liftEffect klank'
  ctx <- H.liftEffect makeAudioContext
  buffers <- H.liftAff (affable $ klank.buffers ctx prevBuffers)
  images <- H.liftAff (affable $ klank.images prevImages)
  videos <- H.liftAff (affable $ klank.videos prevVideos)
  canvases <- H.liftAff (affable $ klank.canvases prevCanvases)
  H.modify_ (_ { buffers = buffers, images = images, videos = videos, canvases = canvases })
