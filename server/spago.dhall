{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "audio-behaviors"
  , "console"
  , "effect"
  , "klank-dev-util"
  , "node-child-process"
  , "node-fs"
  , "node-process"
  , "profunctor-lenses"
  , "psci-support"
  , "quickcheck"
  , "simple-json"
  , "sunde"
  , "typelevel-klank-dev"
  , "uuid"
  , "web-touchevents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
