curl -X POST -H "Content-Type: application/json" -d "{\"code\":\"module Klank.Dev where\n\n-- New to PureScript? Check out https://www.purescript.org/ for learning resources!\n-- To learn more about FRP and the behavior pattern, make sure to check out:\n-- \u2022 https://github.com/paf31/purescript-behaviors\n-- \u2022 https://github.com/mikesol/purescript-audio-behaviors\nimport Prelude\nimport Data.Typelevel.Num (D1)\nimport FRP.Behavior (Behavior)\nimport Data.List ((:), List(..))\nimport Data.NonEmpty ((:|))\nimport FRP.Behavior.Audio (AudioUnit, gain', runInBrowser, sinOsc, speaker)\nimport Type.Klank.Dev (Klank, klank)\nimport Math (pi, sin, cos)\n\nscene :: Number -> Behavior (AudioUnit D1)\nscene time =\n  pure\n    ( speaker\n        ( (gain' (0.1 + (-0.1) * cos (0.5 * pi * time)) (sinOsc 440.0))\n            :| (gain' (0.1 + 0.1 * sin (0.47 * pi * time)) (sinOsc 330.0))\n            : Nil\n        )\n    )\n\nmain :: Klank\nmain =\n  klank\n    { run = runInBrowser scene\n    }\"}" https://klank-dev.ew.r.appspot.com/ | jq .res -r > dev/first-klank.js