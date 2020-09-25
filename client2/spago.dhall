{ name = "halogen-project"
, dependencies =
  [ "ace", "console", "effect", "generics-rep", "halogen", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
