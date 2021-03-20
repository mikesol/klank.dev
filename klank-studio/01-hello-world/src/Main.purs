module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, gain', runInBrowser, sinOsc, speaker)
import Math (pi, sin, cos)
import Type.Klank.Dev (Klank, klank)
import Klank.Weblib.Studio as Studio

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  pure
    ( speaker
        ( (gain' (0.1 + (-0.1) * cos (0.5 * rad)) (sinOsc $ 440.0 + 6.0 * (sin (0.1 * rad))))
            :| (gain' (0.1 + (-0.1) * cos (0.47 * rad)) (sinOsc $ 330.0 + 4.0 * (sin (0.2 * rad))))
            : Nil
        )
    )
  where
  rad = pi * time

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
