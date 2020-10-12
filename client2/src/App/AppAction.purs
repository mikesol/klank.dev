module App.AppAction where
import App.AceComponent as AceComponent

import App.XTermComponent as XTermComponent
data Action
  = Initialize
  | CloseLinkModal
  | PlayKlankFromModal
  | HandleAceUpdate AceComponent.Output
  | HandleTerminalUpdate XTermComponent.Output
