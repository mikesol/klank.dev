let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210313/packages.dhall sha256:ba6368b31902aad206851fec930e89465440ebf5a1fe0391f8be396e2d2f1d87

let overrides = {=}

let additions =
      { audio-behaviors =
        { dependencies =
          [ "aff-promise"
          , "behaviors"
          , "console"
          , "debug"
          , "effect"
          , "drawing"
          , "canvas"
          , "foreign-object"
          , "heterogeneous"
          , "psci-support"
          , "sized-vectors"
          , "typelevel-prelude"
          , "typelevel-graph"
          ]
        , repo = "https://github.com/mikesol/purescript-audio-behaviors.git"
        , version = "master"
        }
      , typelevel-graph =
        { dependencies = [ "typelevel-peano" ]
        , repo = "https://github.com/mikesol/purescript-typelevel-graph.git"
        , version = "main"
        }
      , typelevel-peano =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "prelude"
          , "psci-support"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/csicar/purescript-typelevel-peano.git"
        , version = "v1.0.1"
        }
      , event =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/mikesol/purescript-event.git"
        , version = "master"
        }
      , behaviors =
        { dependencies =
          [ "psci-support"
          , "effect"
          , "ordered-collections"
          , "filterable"
          , "nullable"
          , "event"
          , "web-html"
          , "web-events"
          , "web-uievents"
          ]
        , repo = "https://github.com/mikesol/purescript-behaviors.git"
        , version = "master"
        }
      , painting =
        { dependencies =
          [ "canvas"
          , "colors"
          , "console"
          , "effect"
          , "foldable-traversable"
          , "foreign-object"
          , "psci-support"
          , "web-html"
          ]
        , repo = "https://github.com/mikesol/purescript-painting.git"
        , version = "main"
        }
      }

in  upstream // overrides // additions
