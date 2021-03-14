module Test.Simple where

import Prelude
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, runInBrowser, sinOsc, speaker')
import Type.Klank.Dev (Klank, klank)

scene :: Number -> Behavior (AudioUnit D1)
scene =
  const
    $ pure
        ( speaker'
            ( sinOsc 440.0
            )
        )

main :: Klank
main = klank { run = runInBrowser scene }
