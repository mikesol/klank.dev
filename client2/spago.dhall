{ name = "halogen-project"
, dependencies =
  [ "ace"
  , "console"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "parsing"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
