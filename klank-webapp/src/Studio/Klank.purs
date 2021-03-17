module Klank.Studio where

import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, gain', runInBrowser, sinOsc, speaker)
import Math (pi, sin, cos)
import Type.Klank.Dev (Klank, klank)

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

type Main
  = Klank

main :: Main
main =
  klank
    { run = runInBrowser scene
    }
