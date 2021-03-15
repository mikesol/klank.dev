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
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.ChildProcess (defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, mkdir, readTextFile, writeTextFile)
import Simple.JSON (writeJSON)
import Sunde (spawn)

foreign import clearDeps :: Effect Unit

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
        _ <- liftEffect $ when (exts) clearDeps
        _ <- liftEffect $ (mkdir ("/tmp/deps"))
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
        _ <-
          spawn
            { args:
                [ "-r"
                , "klank-lib/"
                , "/tmp/deps/klank-lib"
                ]
            , cmd: "cp"
            , stdin: Nothing
            }
            defaultSpawnOptions
        _ <-
          spawn
            { args:
                [ "spago.dhall"
                , "/tmp/deps/spago.dhall"
                ]
            , cmd: "cp"
            , stdin: Nothing
            }
            defaultSpawnOptions
        _ <-
          spawn
            { args:
                [ "package.json"
                , "/tmp/deps/package.json"
                ]
            , cmd: "cp"
            , stdin: Nothing
            }
            defaultSpawnOptions
        _ <-
          spawn
            { args:
                [ "-r"
                , "src"
                , "/tmp/deps/src"
                ]
            , cmd: "cp"
            , stdin: Nothing
            }
            defaultSpawnOptions
        _ <-
          spawn
            { args:
                [ "packages2.dhall"
                , "/tmp/deps/packages.dhall"
                ]
            , cmd: "cp"
            , stdin: Nothing
            }
            defaultSpawnOptions
        _ <-
          spawn
            { args:
                [ "-r"
                , "node_modules/"
                , "/tmp/deps/node_modules"
                ]
            , cmd: "cp"
            , stdin: Nothing
            }
            defaultSpawnOptions
        _ <- liftEffect $ mkdir ("/tmp/deps/" <> (toString uuid'))
        pure (toString uuid')
    )
    (const $ pure unit)
    ( \uuid -> do
        _ <- liftEffect $ log "Trying to compile on lambda path"
        _ <-
          liftEffect
            $ writeTextFile
                UTF8
                ("/tmp/deps/lambda.dhall")
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
            { args: [ "compile" ] <> files <> [ uuid <> "/Main.purs" ]
            , cmd: "node_modules/.bin/purs"
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
            , cmd: "node_modules/.bin/esbuild"
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
            , error: Just (whatHappened.stdout <> " *@* " <> whatHappened.stderr <> " *&* " <> whatHappened2.stderr)
            , moduleName: Nothing
            }
    )

compile :: { body :: Code } -> (String -> Effect Unit) -> Effect Unit
compile { body } cb =
  launchAff_ do
    res <- compiler { body }
    liftEffect $ cb (writeJSON res)

files =
  [ ".spago/aff/v6.0.0/src/**/*.purs"
  , ".spago/aff-promise/v3.0.0/src/**/*.purs"
  , ".spago/ansi/v6.0.0/src/**/*.purs"
  , ".spago/argonaut/v8.0.0/src/**/*.purs"
  , ".spago/argonaut-codecs/v8.0.0/src/**/*.purs"
  , ".spago/argonaut-core/v6.0.0/src/**/*.purs"
  , ".spago/argonaut-traversals/v9.0.0/src/**/*.purs"
  , ".spago/arraybuffer-types/v3.0.0/src/**/*.purs"
  , ".spago/arrays/v6.0.0/src/**/*.purs"
  , ".spago/audio-behaviors/master/src/**/*.purs"
  , ".spago/avar/v4.0.0/src/**/*.purs"
  , ".spago/behaviors/master/src/**/*.purs"
  , ".spago/bifunctors/v5.0.0/src/**/*.purs"
  , ".spago/canvas/v5.0.0/src/**/*.purs"
  , ".spago/catenable-lists/v6.0.0/src/**/*.purs"
  , ".spago/colors/v5.0.0/src/**/*.purs"
  , ".spago/console/v5.0.0/src/**/*.purs"
  , ".spago/const/v5.0.0/src/**/*.purs"
  , ".spago/contravariant/v5.0.0/src/**/*.purs"
  , ".spago/control/v5.0.0/src/**/*.purs"
  , ".spago/datetime/v5.0.0/src/**/*.purs"
  , ".spago/debug/v4.0.1/src/**/*.purs"
  , ".spago/distributive/v5.0.0/src/**/*.purs"
  , ".spago/drawing/v4.0.0/src/**/*.purs"
  , ".spago/effect/v3.0.0/src/**/*.purs"
  , ".spago/either/v5.0.0/src/**/*.purs"
  , ".spago/enums/v5.0.0/src/**/*.purs"
  , ".spago/event/master/src/**/*.purs"
  , ".spago/exceptions/v5.0.0/src/**/*.purs"
  , ".spago/exists/v5.0.0/src/**/*.purs"
  , ".spago/filterable/v4.0.0/src/**/*.purs"
  , ".spago/foldable-traversable/v5.0.0/src/**/*.purs"
  , ".spago/foreign/v6.0.0/src/**/*.purs"
  , ".spago/foreign-generic/master/src/**/*.purs"
  , ".spago/foreign-object/v3.0.0/src/**/*.purs"
  , ".spago/fork/v5.0.0/src/**/*.purs"
  , ".spago/free/v6.0.0/src/**/*.purs"
  , ".spago/functions/v5.0.0/src/**/*.purs"
  , ".spago/functors/v4.0.0/src/**/*.purs"
  , ".spago/gen/v3.0.0/src/**/*.purs"
  , ".spago/heterogeneous/v0.5.0/src/**/*.purs"
  , ".spago/identity/v5.0.0/src/**/*.purs"
  , ".spago/integers/v5.0.0/src/**/*.purs"
  , ".spago/invariant/v5.0.0/src/**/*.purs"
  , ".spago/js-date/v7.0.0/src/**/*.purs"
  , ".spago/js-timers/v5.0.1/src/**/*.purs"
  , "./klank-lib/src/**/*.purs"
  , ".spago/lazy/v5.0.0/src/**/*.purs"
  , ".spago/lcg/v3.0.0/src/**/*.purs"
  , ".spago/lists/v6.0.0/src/**/*.purs"
  , ".spago/math/v3.0.0/src/**/*.purs"
  , ".spago/maybe/v5.0.0/src/**/*.purs"
  , ".spago/media-types/v5.0.0/src/**/*.purs"
  , ".spago/mmorph/v6.0.0/src/**/*.purs"
  , ".spago/newtype/v4.0.0/src/**/*.purs"
  , ".spago/node-buffer/v7.0.0/src/**/*.purs"
  , ".spago/node-child-process/v7.0.0/src/**/*.purs"
  , ".spago/node-fs/v6.0.0/src/**/*.purs"
  , ".spago/node-path/v4.0.0/src/**/*.purs"
  , ".spago/node-process/v8.0.0/src/**/*.purs"
  , ".spago/node-streams/v5.0.0/src/**/*.purs"
  , ".spago/nonempty/v6.0.0/src/**/*.purs"
  , ".spago/now/v5.0.0/src/**/*.purs"
  , ".spago/nullable/v5.0.0/src/**/*.purs"
  , ".spago/numbers/v8.0.0/src/**/*.purs"
  , ".spago/ordered-collections/v2.0.0/src/**/*.purs"
  , ".spago/orders/v5.0.0/src/**/*.purs"
  , ".spago/painting/main/src/**/*.purs"
  , ".spago/parallel/v5.0.0/src/**/*.purs"
  , ".spago/partial/v3.0.0/src/**/*.purs"
  , ".spago/pipes/v7.0.0/src/**/*.purs"
  , ".spago/posix-types/v5.0.0/src/**/*.purs"
  , ".spago/prelude/v5.0.0/src/**/*.purs"
  , ".spago/profunctor/v5.0.0/src/**/*.purs"
  , ".spago/profunctor-lenses/v7.0.0/src/**/*.purs"
  , ".spago/psci-support/v5.0.0/src/**/*.purs"
  , ".spago/quickcheck/v7.0.0/src/**/*.purs"
  , ".spago/random/v5.0.0/src/**/*.purs"
  , ".spago/record/v3.0.0/src/**/*.purs"
  , ".spago/refs/v5.0.0/src/**/*.purs"
  , ".spago/safe-coerce/v1.0.0/src/**/*.purs"
  , ".spago/simple-json/ps-0.14/src/**/*.purs"
  , ".spago/sized-vectors/v5.0.2/src/**/*.purs"
  , ".spago/spec/v5.0.0/src/**/*.purs"
  , ".spago/st/v5.0.0/src/**/*.purs"
  , ".spago/strings/v5.0.0/src/**/*.purs"
  , ".spago/sunde/v2.0.0/src/**/*.purs"
  , ".spago/tailrec/v5.0.0/src/**/*.purs"
  , ".spago/transformers/v5.0.0/src/**/*.purs"
  , ".spago/tuples/v6.0.0/src/**/*.purs"
  , ".spago/type-equality/v4.0.0/src/**/*.purs"
  , ".spago/typelevel/v6.0.0/src/**/*.purs"
  , ".spago/typelevel-graph/main/src/**/*.purs"
  , ".spago/typelevel-peano/v1.0.1/src/**/*.purs"
  , ".spago/typelevel-prelude/v6.0.0/src/**/*.purs"
  , ".spago/unfoldable/v5.0.0/src/**/*.purs"
  , ".spago/unsafe-coerce/v5.0.0/src/**/*.purs"
  , ".spago/unsafe-reference/v4.0.0/src/**/*.purs"
  , ".spago/uuid/master/src/**/*.purs"
  , ".spago/variant/v7.0.2/src/**/*.purs"
  , ".spago/web-dom/v5.0.0/src/**/*.purs"
  , ".spago/web-events/v3.0.0/src/**/*.purs"
  , ".spago/web-file/v3.0.0/src/**/*.purs"
  , ".spago/web-html/v3.0.1/src/**/*.purs"
  , ".spago/web-storage/v4.0.0/src/**/*.purs"
  , ".spago/web-touchevents/v3.0.0/src/**/*.purs"
  , ".spago/web-uievents/v3.0.0/src/**/*.purs"
  , "src/**/*.purs"
  ] ::
    Array String
