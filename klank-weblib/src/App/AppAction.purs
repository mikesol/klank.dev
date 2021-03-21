module Klank.Weblib.AppAction where

import Klank.Weblib.Shared (PlayerUpdate)

data Action
  = Initialize
  | PlayStartSucceeded PlayerUpdate
  | PlayStartFailed String
  | RecordingRegistered String String
  | PlayKlankFromModal
  | PlayKlankFromPlayButton
  | PlayKlankFromStopButton
  | ProgressUpdate Number
