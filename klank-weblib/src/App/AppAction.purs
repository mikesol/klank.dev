module Klank.Weblib.AppAction where

import Prelude
import Klank.Weblib.AceComponent as AceComponent
import Klank.Weblib.XTermComponent as XTermComponent
import Data.Maybe (Maybe)
import Effect (Effect)
import FRP.Behavior.Audio (AudioContext, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, MediaRecorder, RecorderSignature)
import Foreign.Object (Object)

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

data Action
  = Initialize
  | PlayStartSucceeded PlayerUpdate
  | PlayStartFailed String
  | RecordingRegistered String String
  | CloseLinkModal
  | PlayKlankFromModal
  | PlayKlankFromPlayButton
  | PlayKlankFromStopButton
  | ProgressUpdate Number
  | HandleAceUpdate AceComponent.Output
  | HandleTerminalUpdate XTermComponent.Output
