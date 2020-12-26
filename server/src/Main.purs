module Main where

import Prelude
import Data.Array (catMaybes, head)
import Data.Array.NonEmpty (tail)
import Data.Either (either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.UUID (genUUID, toString)
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.ChildProcess (defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, mkdir, readTextFile, rmdir, unlink, writeTextFile)
import Simple.JSON (writeJSON)
import Sunde (spawn)

type Code
  = { code :: String
    }

type Compiled
  = { res :: Maybe String
    , error :: Maybe String
    , moduleName :: Maybe String
    }

extractModule :: String -> Maybe String
extractModule s = either (const Nothing) (\p -> match p s >>= \a -> (join <<< head <<< tail) a) re
  where
  re = regex "^[ \\t]*module[ \\t]+([a-zA-Z0-9\\.]+)[ \\t]+where" noFlags

hackishlyGetModule :: String -> String
hackishlyGetModule s = fromMaybe "Could.Not.Find.Module" (head $ (catMaybes $ map extractModule lines))
  where
  lines = split (Pattern "\n") s

hackishlyRenameModule :: String -> String -> String -> Effect { code :: String, moduleName :: String }
hackishlyRenameModule u cd m = do
  let
    moduleName = "A" <> (replaceAll (Pattern "-") (Replacement "") u)
  pure
    { code: (replaceAll (Pattern ("module " <> m <> " where")) (Replacement ("module " <> moduleName <> " where")) cd)
    , moduleName
    }

compiler :: { body :: Code } -> Aff Compiled
compiler { body } =
  bracket
    ( do
        uuid' <- liftEffect $ genUUID
        exts <- liftEffect $ exists ("/tmp/deps")
        _ <- liftEffect $ when (not exts) (mkdir ("/tmp/deps"))
        _ <-
          spawn
            { args:
                [ "-r"
                , ".spago/"
                , "/tmp/deps/.spago"
                ]
            , cmd: "cp"
            , stdin: Nothing
            }
            defaultSpawnOptions
        _ <-
          spawn
            { args:
                [ "-r"
                , "output/"
                , "/tmp/deps/output"
                ]
            , cmd: "cp"
            , stdin: Nothing
            }
            defaultSpawnOptions
        _ <- liftEffect $ mkdir ("/tmp/deps/" <> (toString uuid'))
        pure (toString uuid')
    )
    ( \uuid -> do
        let
          moduleName =
            "A"
              <> (replaceAll (Pattern "-") (Replacement "") uuid)
        _ <-
          try
            ( liftEffect
                $ unlink
                    ("/tmp/deps/" <> uuid <> "/index_.js")
            )
        _ <-
          try
            ( liftEffect
                $ unlink
                    ("/tmp/deps/" <> uuid <> "/index.js")
            )
        _ <-
          try
            ( liftEffect
                $ unlink
                    ("/tmp/output/" <> moduleName <> "/externs.cbor")
            )
        _ <-
          try
            ( liftEffect
                $ unlink
                    ("/tmp/output/" <> moduleName <> "/index.js")
            )
        _ <-
          try
            ( liftEffect
                $ rmdir
                    ("/tmp/output/" <> moduleName)
            )
        liftEffect $ unlink ("/tmp/deps/" <> uuid <> "/Main.purs")
        liftEffect $ rmdir ("/tmp/deps/" <> uuid)
        liftEffect $ unlink ("/tmp/deps/" <> uuid <> ".dhall")
    )
    ( \uuid -> do
        _ <- liftEffect $ log "Trying to compile on lambda path"
        _ <-
          liftEffect
            $ writeTextFile
                UTF8
                ("/tmp/deps/" <> uuid <> ".dhall")
                ("let conf = ./spago.dhall\nin conf // { sources = conf.sources # [ \"" <> uuid <> "/Main.purs\" ] }")
        let
          mod = hackishlyGetModule body.code
        renamed <-
          liftEffect
            $ hackishlyRenameModule uuid body.code mod
        _ <-
          liftEffect
            $ writeTextFile
                UTF8
                ("/tmp/deps/" <> uuid <> "/index_.js")
                ("window.klank = require(\"../output/" <> renamed.moduleName <> "/\").main")
        _ <-
          liftEffect
            $ writeTextFile
                UTF8
                ("/tmp/deps/" <> uuid <> "/Main.purs")
                renamed.code
        whatHappened <-
          spawn
            { args:
                [ "compile"
                ]
                  <> cmd
                  <> [ uuid <> "/Main.purs"
                    ]
            , cmd: "/var/task/node_modules/.bin/purs"
            , stdin: Nothing
            }
            defaultSpawnOptions
              { cwd = Just "/tmp/deps"
              }
        whatHappened2 <-
          spawn
            { args:
                [ uuid <> "/index_.js"
                , "--bundle"
                , "--minify"
                , "--outfile=" <> uuid <> "/index.js"
                ]
            , cmd: "/var/task/node_modules/.bin/esbuild"
            , stdin: Nothing
            }
            defaultSpawnOptions
              { cwd = Just "/tmp/deps"
              }
        worked <- liftEffect $ exists ("/tmp/deps/" <> uuid <> "/index.js")
        if worked then
          ( do
              res <-
                liftEffect
                  $ readTextFile
                      UTF8
                      ("/tmp/deps/" <> uuid <> "/index.js")
              pure
                { res: Just res
                , error: Nothing
                , moduleName: Just renamed.moduleName
                }
          )
        else
          pure
            { res: Nothing
            , error: Just (whatHappened.stderr <> " *&* " <> whatHappened2.stderr)
            , moduleName: Nothing
            }
    )

compile :: { body :: Code } -> (String -> Effect Unit) -> Effect Unit
compile { body } cb =
  launchAff_ do
    res <- compiler { body }
    liftEffect $ cb (writeJSON res)

cmd :: Array String
cmd =
  [ ".spago/aff/v5.1.2/src/**/*.purs"
  , ".spago/aff-promise/v2.1.0/src/**/*.purs"
  , ".spago/ansi/v5.0.0/src/**/*.purs"
  , ".spago/argonaut/v7.0.0/src/**/*.purs"
  , ".spago/argonaut-codecs/v7.0.0/src/**/*.purs"
  , ".spago/argonaut-core/v5.1.0/src/**/*.purs"
  , ".spago/argonaut-traversals/v8.0.0/src/**/*.purs"
  , ".spago/arraybuffer-types/v2.0.0/src/**/*.purs"
  , ".spago/arrays/v5.3.1/src/**/*.purs"
  , ".spago/audio-behaviors/master/src/**/*.purs"
  , ".spago/avar/v3.0.0/src/**/*.purs"
  , ".spago/behaviors/v8.0.0/src/**/*.purs"
  , ".spago/bifunctors/v4.0.0/src/**/*.purs"
  , ".spago/canvas/v4.0.0/src/**/*.purs"
  , ".spago/catenable-lists/v5.0.1/src/**/*.purs"
  , ".spago/colors/v5.0.0/src/**/*.purs"
  , ".spago/console/v4.4.0/src/**/*.purs"
  , ".spago/const/v4.1.0/src/**/*.purs"
  , ".spago/contravariant/v4.0.1/src/**/*.purs"
  , ".spago/control/v4.2.0/src/**/*.purs"
  , ".spago/datetime/v4.1.1/src/**/*.purs"
  , ".spago/debug/v4.0.0/src/**/*.purs"
  , ".spago/distributive/v4.0.0/src/**/*.purs"
  , ".spago/drawing/v4.0.0/src/**/*.purs"
  , ".spago/effect/v2.0.1/src/**/*.purs"
  , ".spago/either/v4.1.1/src/**/*.purs"
  , ".spago/enums/v4.0.1/src/**/*.purs"
  , ".spago/event/v1.3.0/src/**/*.purs"
  , ".spago/exceptions/v4.0.0/src/**/*.purs"
  , ".spago/exists/v4.0.0/src/**/*.purs"
  , ".spago/filterable/v3.0.2/src/**/*.purs"
  , ".spago/foldable-traversable/v4.1.1/src/**/*.purs"
  , ".spago/foreign/v5.0.0/src/**/*.purs"
  , ".spago/foreign-generic/v10.0.0/src/**/*.purs"
  , ".spago/foreign-object/v2.0.3/src/**/*.purs"
  , ".spago/fork/v4.0.0/src/**/*.purs"
  , ".spago/free/v5.2.0/src/**/*.purs"
  , ".spago/functions/v4.0.0/src/**/*.purs"
  , ".spago/functors/v3.1.1/src/**/*.purs"
  , ".spago/gen/v2.1.1/src/**/*.purs"
  , ".spago/generics-rep/v6.1.1/src/**/*.purs"
  , ".spago/globals/v4.1.0/src/**/*.purs"
  , ".spago/heterogeneous/v0.4.1/src/**/*.purs"
  , ".spago/identity/v4.1.0/src/**/*.purs"
  , ".spago/integers/v4.0.0/src/**/*.purs"
  , ".spago/invariant/v4.1.0/src/**/*.purs"
  , ".spago/js-date/v6.0.0/src/**/*.purs"
  , ".spago/js-timers/v4.0.1/src/**/*.purs"
  , ".spago/klank-dev-util/main/src/**/*.purs"
  , ".spago/lazy/v4.0.0/src/**/*.purs"
  , ".spago/lcg/v2.0.0/src/**/*.purs"
  , ".spago/lists/v5.4.1/src/**/*.purs"
  , ".spago/math/v2.1.1/src/**/*.purs"
  , ".spago/maybe/v4.0.1/src/**/*.purs"
  , ".spago/media-types/v4.0.1/src/**/*.purs"
  , ".spago/mmorph/v5.1.0/src/**/*.purs"
  , ".spago/newtype/v3.0.0/src/**/*.purs"
  , ".spago/node-buffer/v6.0.0/src/**/*.purs"
  , ".spago/node-child-process/v6.0.0/src/**/*.purs"
  , ".spago/node-fs/v5.0.1/src/**/*.purs"
  , ".spago/node-path/v3.0.0/src/**/*.purs"
  , ".spago/node-process/v7.0.0/src/**/*.purs"
  , ".spago/node-streams/v4.0.1/src/**/*.purs"
  , ".spago/nonempty/v5.0.0/src/**/*.purs"
  , ".spago/now/v4.0.0/src/**/*.purs"
  , ".spago/nullable/v4.1.1/src/**/*.purs"
  , ".spago/ordered-collections/v1.6.1/src/**/*.purs"
  , ".spago/orders/v4.0.0/src/**/*.purs"
  , ".spago/parallel/v4.0.0/src/**/*.purs"
  , ".spago/parseint/v1.1.1/src/**/*.purs"
  , ".spago/partial/v2.0.1/src/**/*.purs"
  , ".spago/pipes/v6.0.0/src/**/*.purs"
  , ".spago/posix-types/v4.0.0/src/**/*.purs"
  , ".spago/prelude/v4.1.1/src/**/*.purs"
  , ".spago/profunctor/v4.1.0/src/**/*.purs"
  , ".spago/profunctor-lenses/v6.3.0/src/**/*.purs"
  , ".spago/proxy/v3.0.0/src/**/*.purs"
  , ".spago/psci-support/v4.0.0/src/**/*.purs"
  , ".spago/quickcheck/v6.1.0/src/**/*.purs"
  , ".spago/random/v4.0.0/src/**/*.purs"
  , ".spago/record/v2.0.2/src/**/*.purs"
  , ".spago/record-extra/v3.0.1/src/**/*.purs"
  , ".spago/refs/v4.1.0/src/**/*.purs"
  , ".spago/simple-json/v7.0.0/src/**/*.purs"
  , ".spago/sized-vectors/v5.0.1/src/**/*.purs"
  , ".spago/spec/v4.0.1/src/**/*.purs"
  , ".spago/st/v4.1.1/src/**/*.purs"
  , ".spago/strings/v4.0.2/src/**/*.purs"
  , ".spago/sunde/v2.0.0/src/**/*.purs"
  , ".spago/tailrec/v4.1.1/src/**/*.purs"
  , ".spago/transformers/v4.2.0/src/**/*.purs"
  , ".spago/tuples/v5.1.0/src/**/*.purs"
  , ".spago/type-equality/v3.0.0/src/**/*.purs"
  , ".spago/typelevel/v6.0.0/src/**/*.purs"
  , ".spago/typelevel-graph/main/src/**/*.purs"
  , ".spago/typelevel-klank-dev/main/src/**/*.purs"
  , ".spago/typelevel-peano/v0.1.8/src/**/*.purs"
  , ".spago/typelevel-prelude/v5.0.2/src/**/*.purs"
  , ".spago/unfoldable/v4.1.0/src/**/*.purs"
  , ".spago/unsafe-coerce/v4.0.0/src/**/*.purs"
  , ".spago/unsafe-reference/v3.0.1/src/**/*.purs"
  , ".spago/uuid/v6.1.0/src/**/*.purs"
  , ".spago/variant/v6.0.1/src/**/*.purs"
  , ".spago/web-dom/v4.1.0/src/**/*.purs"
  , ".spago/web-events/v2.0.1/src/**/*.purs"
  , ".spago/web-file/v2.3.0/src/**/*.purs"
  , ".spago/web-html/v2.3.0/src/**/*.purs"
  , ".spago/web-storage/v3.0.0/src/**/*.purs"
  , ".spago/web-touchevents/v2.0.0/src/**/*.purs"
  , ".spago/web-uievents/v2.0.0/src/**/*.purs"
  ]
