module Klank.Weblib.Shared where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, try)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import FRP.Behavior.Audio (AudioContext, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserMicrophone, BrowserPeriodicWave, MediaRecorder, audioWorkletAddModule, makeAudioContext)
import Foreign.Object (Object)
import Foreign.Object as O
import Graphics.Canvas (CanvasElement)
import Halogen as H
import Halogen.Subscription as HS
import Klank.Weblib.AppAction (Action(..))
import Type.Klank.Dev (Klank'')
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (HTMLCanvasElement, HTMLImageElement, HTMLVideoElement)

type PlayKlank = ∀ (t315 ∷ Type -> Type) (t316 ∷ Type) (t317 ∷ Row Type) (t380 ∷ Type) (accumulator ∷ Type) (t583 ∷ Type) (t593 ∷ Type) (t603 ∷ Type) (t621 ∷ Type) (t650 ∷ Type) (t675 ∷ Type) (env ∷ Type) (t681 ∷ Row Type) (t724 ∷ Row Type). MonadEffect t315 ⇒ MonadAff t315 ⇒ H.HalogenM { audioCtx ∷ Maybe AudioContext , buffers ∷     Object BrowserAudioBuffer
 , canvases ∷     Object HTMLCanvasElement
 , effectfulKlank ∷ Effect (Klank'' accumulator env) , floatArrays ∷     Object BrowserFloatArray
 , images ∷     Object HTMLImageElement
 , initialAccumulator ∷ Maybe accumulator , periodicWaves ∷     Object BrowserPeriodicWave
 , playerSubscriptionId ∷ Maybe H.SubscriptionId , recorders ∷ Object (MediaRecorder -> Effect Unit) , stopFn ∷ Maybe (Effect Unit) , tracks ∷ Object BrowserAudioTrack
 , videos ∷     Object HTMLVideoElement
 , worklets ∷ Array String | t724 } Action t317 t316 t315 Unit
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
playKlank = do
  stopper
  oldSubId <- H.gets _.playerSubscriptionId
  maybe (pure unit) H.unsubscribe oldSubId
  klank' <- H.gets _.effectfulKlank
  klank <- H.liftEffect klank'
  ctx <- H.liftEffect makeAudioContext
  H.liftAff (toAffE $ loadCustomAudioNodes ctx)
  -- a bit hackish: we unsafely coerce the initial accumulator as we
  -- do not know what it will be
  initialAccumulator <- unsafeCoerce <$> H.gets _.initialAccumulator
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
        { microphone, camera } <- if klank.enableMicrophone || klank.enableCamera then getMicrophoneAndCamera klank.enableMicrophone klank.enableCamera else pure { microphone: Nothing, camera: Nothing }
        let
          microphones = maybe O.empty (O.singleton "microphone") microphone
        cameraAsVideo <- case camera of
          Nothing -> pure Nothing
          Just c -> Just <$> H.liftEffect (cameraToVideo c)
        accumulator <- case initialAccumulator of
          Nothing -> (affable $ klank.accumulator)
          Just acc -> pure acc
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
                ( \k v -> HS.notify listener (RecordingRegistered k v)
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
          Left err -> HS.notify listener (PlayStartFailed (show err))
          Right resp -> H.liftEffect $ HS.notify listener (PlayStartSucceeded resp)
  H.modify_ (_ { playerSubscriptionId = Just subId })
  pure unit


type Stopper = ∀ t32 t33 t34 t35 t42. MonadEffect t32 => H.HalogenM { audioCtx ∷ Maybe AudioContext, playerSubscriptionId ∷ Maybe H.SubscriptionId, stopFn ∷ Maybe (Effect Unit) | t42 } t35 t34 t33 t32 Unit

foreign import stopAudioContext :: AudioContext -> Effect Unit

stopper :: Stopper
stopper = do
  sfn <- H.gets _.stopFn
  ctx <- H.gets _.audioCtx
  H.modify_ (_ { stopFn = Nothing, audioCtx = Nothing, playerSubscriptionId = Nothing })
  maybe (pure unit) H.liftEffect sfn
  maybe (pure unit) (H.liftEffect <<< stopAudioContext) ctx
foreign import getInitialAccumulator :: forall accumulator. Maybe accumulator -> (accumulator -> Maybe accumulator) -> Effect (Maybe accumulator)

foreign import canvasDimensionHack :: Effect Unit
