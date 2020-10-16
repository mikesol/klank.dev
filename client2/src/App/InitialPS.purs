module App.InitialPS where

import Prelude
import App.Chalk (CColor(..), chalk, defaultChalk)
import Data.Maybe (Maybe(..))

helpMsg = ("> help (h)    | This help message. More help @ https://discourse.klank.dev\r\n> compile (k) | Compile the current scene\r\n> play (p)    | Play the current scene\r\n> stop (s)    | Stop the current scene\r\n> link (l)    | Generate a link to the current klank\r\n> flink (fl)  | Generate a link to the current klank as a file\r\n> edit (e)    | Show the editor\r\n> split (ec)  | Show the editor and canvas\r\n> canvas (c)  | Show only the canvas") :: String

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
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, gain', runInBrowser, sinOsc, speaker')
import Type.Klank.Dev (Klank, klank)

scene :: Number -> Behavior (AudioUnit D1)
scene _ = pure (speaker' (gain' 0.2 (sinOsc 440.0)))

main :: Klank
main =
  klank
    { run = runInBrowser scene
    }""" ::
    String
