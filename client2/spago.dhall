{ name = "halogen-project"
, dependencies =
  [ "ace"
  , "aff-promise"
  , "affjax"
  , "argonaut-codecs"
  , "audio-behaviors"
  , "console"
  , "drawing"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "parsing"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
