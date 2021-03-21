module Klank.Weblib.AppAction where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import FRP.Behavior.Audio (AudioContext, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, MediaRecorder, RecorderSignature)
import Foreign.Object (Object)

data Action
  = Initialize
  | PlayStartSucceeded PlayerUpdate
  | PlayStartFailed String
  | RecordingRegistered String String
  | PlayKlankFromModal
  | PlayKlankFromPlayButton
  | PlayKlankFromStopButton
  | ProgressUpdate Number
