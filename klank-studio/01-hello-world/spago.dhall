{ name = "halogen-project"
, dependencies =
  [ "ace"
  , "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut-codecs"
  , "audio-behaviors"
  , "b64"
  , "console"
  , "effect"
  , "halogen"
  , "halogen-storybook"
  , "klank-lib"
  , "klank-weblib"
  , "painting"
  , "parsing"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
