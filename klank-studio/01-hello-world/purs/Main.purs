module Main where

import Prelude
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior.Audio (AudioUnit, gain', runInBrowser, sinOsc, speaker')
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Klank.Weblib.Studio as Studio
import Type.Klank.Dev (Klank, klank)

scene :: AudioUnit D1
scene = speaker' (gain' 0.2 (sinOsc $ 440.0))

playMe :: Klank
playMe =
  klank
    { run = runInBrowser scene
    }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (Studio.component (pure playMe)) unit body
