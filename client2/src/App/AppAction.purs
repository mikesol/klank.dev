module App.AppAction where

import App.AceComponent as AceComponent
import App.DropzoneComponent as DropzoneComponent
import App.XTermComponent as XTermComponent

data Action
  = Initialize
  | CloseLinkModal
  | PlayKlankFromModal
  | PlayKlankFromPlayButton
  | PlayKlankFromStopButton
  | HandleAceUpdate AceComponent.Output
  | HandleTerminalUpdate XTermComponent.Output
  | HandleFileDrop DropzoneComponent.Output
