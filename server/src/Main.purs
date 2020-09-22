module Main where

import Prelude
import Data.Array (catMaybes, head)
import Data.Array.NonEmpty (tail)
import Data.Either (either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.UUID (genUUID, toString)
import Effect (Effect)
import Effect.Aff (Aff, bracket, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.ChildProcess (defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, mkdir, readTextFile, rmdir, unlink, writeTextFile)
import Node.Platform (Platform(..))
import Node.Process (platform)
import Payload.Server as Payload
import Payload.Spec (Spec(Spec), POST)
import Sunde (spawn)

type Code
  = { code :: String
    }

type Compiled
  = { res :: Maybe String
    , error :: Maybe String
    }

spec ::
  Spec
    { compiler ::
        POST "/"
          { body :: Code
          , response :: Compiled
          }
    }
spec = Spec

extractModule :: String -> Maybe String
extractModule s = either (const Nothing) (\p -> match p s >>= \a -> (join <<< head <<< tail) a) re
  where
  re = regex "^[ \\t]*module[ \\t]+([a-zA-Z0-9\\.]+)[ \\t]+where" noFlags

hackishlyGetModule :: String -> String
hackishlyGetModule s = fromMaybe "Could.Not.Find.Module" (head $ (catMaybes $ map extractModule lines))
  where
  lines = split (Pattern "\n") s

compiler :: { body :: Code } -> Aff Compiled
compiler { body } =
  bracket
    ( do
        uuid' <- liftEffect $ genUUID
        _ <- liftEffect $ mkdir ("deps/" <> (toString uuid'))
        liftEffect $ log "Created dep"
        pure (toString uuid')
    )
    ( \uuid -> do
        liftEffect $ log "Deleting dep"
        _ <- try (liftEffect $ unlink ("./deps/" <> uuid <> "/index.js"))
        liftEffect $ unlink ("./deps/" <> uuid <> "/Main.purs")
        liftEffect $ rmdir ("./deps/" <> uuid)
        liftEffect $ unlink ("./deps/" <> uuid <> ".dhall")
    )
    ( \uuid -> do
        _ <-
          liftEffect
            $ writeTextFile
                UTF8
                ("deps/" <> uuid <> ".dhall")
                ("let conf = ./spago.dhall\nin conf // { sources = conf.sources # [ \"" <> uuid <> "/Main.purs\" ] }")
        _ <-
          liftEffect
            $ writeTextFile
                UTF8
                ("deps/" <> uuid <> "/Main.purs")
                body.code
        whatHappened <-
          spawn
            { args:
                [ "-x"
                , uuid <> ".dhall"
                , "bundle-module"
                , "--main"
                , hackishlyGetModule body.code
                , "--to"
                , uuid <> "/index.js"
                ]
            , cmd: if platform == Just Win32 then "spago.cmd" else "spago"
            , stdin: Nothing
            }
            defaultSpawnOptions
              { cwd = Just "deps"
              }
        worked <- liftEffect $ exists ("deps/" <> uuid <> "/index.js")
        if worked then
          ( do
              res <-
                liftEffect
                  $ readTextFile
                      UTF8
                      ("deps/" <> uuid <> "/index.js")
              pure { res: Just res, error: Nothing }
          )
        else
          pure { res: Nothing, error: Just whatHappened.stderr }
    )

main :: Effect Unit
main = Payload.launch spec { compiler }
