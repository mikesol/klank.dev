{ name = "halogen-project"
, dependencies =
  [ "audio-behaviors"
  , "console"
  , "effect"
  , "halogen"
  , "klank-lib"
  , "klank-weblib"
  , "painting"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
