{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "audio-behaviors"
  , "console"
  , "effect"
  , "halogen"
  , "klank-lib"
  , "painting"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
