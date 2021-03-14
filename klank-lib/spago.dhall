{ name = "klank-lib"
, dependencies =
  [ "audio-behaviors", "console", "effect", "painting", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
