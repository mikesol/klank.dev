module App.InitialPS where

import Prelude
import App.Chalk (CColor(..), chalk, defaultChalk)
import Data.Maybe (Maybe(..))

welcomeMsg =
  ( "Welcome to "
      <> ( chalk
            ( defaultChalk
                { color = Just CCyan
                , underline = true
                , bold = true
                }
            )
            "klank.dev"
        )
      <> "!\r\n🔊  🎧  in the browser using PureScript.\r\n- Ctrl-K (Win/Linux) or Command-K (Mac) to start/stop the current audio scene\r\n- Ctrl-J (Win/Linux) or Command-J (Mac) to recompile when you make changes\r\n- Docs and issues on https://github.com/mikesol/purescript-audio-behaviors\r\n- Type h then ENTER for more commands\r\n$ "
  ) ::
    String

initialPS =
  """module Klank.Dev where
-- New to PureScript? Check out https://www.purescript.org/ for learning resources!
import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, gain', runInBrowser_, sinOsc, speaker)
import Math (pi, sin)

scene :: Behavior Number -> Behavior (AudioUnit D1)
scene time = f <$> time
  where
  f s =
    let
      rad = pi * s
    in
      speaker
        $ ( (gain' 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
              :| (gain' 0.25 $ sinOsc (235.0 + (10.0 * sin (1.7 * rad))))
              : (gain' 0.2 $ sinOsc (337.0 + (10.0 * sin rad)))
              : (gain' 0.1 $ sinOsc (530.0 + (19.0 * (5.0 * sin rad))))
              : Nil
          )

-- change to true if you use `microphone` above
enableMicrophone = false

-- add a mouse, keyboard or other device here if needed
main = runInBrowser_ (pure scene)""" ::
    String
