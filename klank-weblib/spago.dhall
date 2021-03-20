{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "audio-behaviors"
  , "b64"
  , "console"
  , "effect"
  , "halogen"
  , "klank-lib"
  , "painting"
  , "parsing"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
