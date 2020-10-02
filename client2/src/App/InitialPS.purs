module App.InitialPS where

import Prelude
import App.Chalk (CColor(..), chalk, defaultChalk)
import Data.Maybe (Maybe(..))

helpMsg = ("> help (h)    | This help message. More help @ https://discourse.klank.dev\r\n> compile (k) | Compile the current scene\r\n> play (p)    | Play the current scene\r\n> stop (s)    | Stop the current scene\r\n> edit (e)    | Show the editor\r\n> split (ec)  | Show the editor and canvas\r\n> canvas (c)  | Show only the canvas") :: String

frontMatter =
  "Welcome to "
    <> ( chalk
          ( defaultChalk
              { color = Just CCyan
              , underline = true
              , bold = true
              }
          )
          "klank.dev"
      )
    <> "!\r\nSound and animation in the browser using PureScript." ::
    String

afterMatter = "\r\n- Type k then ENTER to compile when you make changes\r\n- Type p then ENTER to play the current scene\r\n- Type s then ENTER to stop the current scene\r\n- Discussion and issues on https://discourse.klank.dev\r\n- Type h then ENTER for more commands\r\n$ " :: String

welcomeMsg =
  ( frontMatter
      <> afterMatter
  ) ::
    String

initialPS =
  """module Klank.Dev where
-- New to PureScript? Check out https://www.purescript.org/ for learning resources!
-- To learn more about FRP and the behavior pattern, make sure to check out:
-- • https://github.com/paf31/purescript-behaviors
-- • https://github.com/mikesol/purescript-audio-behaviors
import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, gain', runInBrowser, sinOsc, speaker)
import Math (pi, sin)

scene :: Number -> Behavior (AudioUnit D1)
scene time = let
      rad = pi * time
    in
      pure $ speaker
         ( (gain' 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
              :| (gain' 0.25 $ sinOsc (235.0 + (10.0 * sin (1.7 * rad))))
              : (gain' 0.2 $ sinOsc (337.0 + (10.0 * sin rad)))
              : (gain' 0.1 $ sinOsc (530.0 + (19.0 * (5.0 * sin rad))))
              : Nil
          )

main = runInBrowser scene
















""" ::
    String
