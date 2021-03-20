module Studio where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Klank.Weblib.App as App
import Klank.Studio as Studio

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (App.component (pure Studio.main)) unit body
