{ name = "klank-homepage"
, dependencies =
  [ "atari"
  , "audio-behaviors"
  , "bwv846"
  , "console"
  , "e2020"
  , "effect"
  , "halogen"
  , "hello-world"
  , "klank-lib"
  , "klank-weblib"
  , "painting"
  , "psci-support"
  , "routing"
  , "silent-night"
  , "waves"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
