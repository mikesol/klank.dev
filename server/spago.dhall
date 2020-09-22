{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "audio-behaviors"
  , "console"
  , "effect"
  , "node-child-process"
  , "node-fs"
  , "node-process"
  , "payload"
  , "psci-support"
  , "sunde"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
