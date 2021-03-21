module Main where

import Prelude
import Control.Promise (toAffE)
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Traversable (sequence)
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior.Audio (AudioUnit, decodeAudioDataFromUri, gain', loopBuf, runInBrowser, speaker)
import Foreign.Object as O
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Klank.Dev.Util (affable)
import Klank.Weblib.Studio as Studio
import Math (pi, sin)
import Type.Klank.Dev (Klank, Buffers, klank)

vol = 1.4 :: Number

scene :: Number -> AudioUnit D1
scene time =
  speaker
    ( (gain' (0.3 * vol) (loopBuf "atar" (1.0 + 0.1 * sin rad) 0.0 0.0))
        :| ( gain' (0.15 * vol)
              ( loopBuf "atar"
                  (1.5 + 0.1 * sin (2.0 * rad))
                  (0.1 + 0.1 * sin rad)
                  (0.5 + 0.25 * sin (2.0 * rad))
              )
          )
        : (gain' (0.3 * vol) (loopBuf "atar" 0.25 0.0 0.0))
        : Nil
    )
  where
  rad = pi * time

buffers :: Buffers
buffers ctx _ =
  affable
    $ sequence
        ( O.singleton "atar"
            $ toAffE (decodeAudioDataFromUri ctx "https://freesound.org/data/previews/100/100981_1234256-lq.mp3")
        )

playMe :: Klank
playMe =
  klank
    { run = runInBrowser scene
    , buffers = buffers
    }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (Studio.component (pure playMe)) unit body
