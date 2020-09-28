{ name = "halogen-project"
, dependencies =
  [ "ace"
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
