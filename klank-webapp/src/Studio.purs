module Studio where

import Prelude
import App.App (AppMode(..))
import App.App as App
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (App.component Studio) unit body
