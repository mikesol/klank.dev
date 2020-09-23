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
import Node.Platform (Platform(..))
import Node.Process (platform)
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
    { code: (replaceAll (Pattern m) (Replacement moduleName) cd)
    , moduleName
    }

compiler :: { body :: Code } -> Aff Compiled
compiler { body } =
  bracket
    ( do
        uuid' <- liftEffect $ genUUID
        _ <- liftEffect $ mkdir ("deps/" <> (toString uuid'))
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
                    ("./deps/" <> uuid <> "/index.js")
            )
        _ <-
          try
            ( liftEffect
                $ unlink
                    ("./output/" <> moduleName <> "/externs.cbor")
            )
        _ <-
          try
            ( liftEffect
                $ unlink
                    ("./output/" <> moduleName <> "/index.js")
            )
        _ <-
          try
            ( liftEffect
                $ rmdir
                    ("./output/" <> moduleName)
            )
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
        let
          mod = hackishlyGetModule body.code
        renamed <-
          liftEffect
            $ hackishlyRenameModule uuid body.code mod
        _ <-
          liftEffect
            $ writeTextFile
                UTF8
                ("deps/" <> uuid <> "/Main.purs")
                renamed.code
        whatHappened <-
          spawn
            { args:
                [ "-x"
                , uuid <> ".dhall"
                , "bundle-module"
                , "--main"
                , renamed.moduleName
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
              pure
                { res: Just res
                , error: Nothing
                , moduleName: Just renamed.moduleName
                }
          )
        else
          pure
            { res: Nothing
            , error: Just whatHappened.stderr
            , moduleName: Nothing
            }
    )

compile :: { body :: Code } -> (String -> Effect Unit) -> Effect Unit
compile { body } cb = launchAff_ do
  res <- compiler {body}
  liftEffect $ cb (writeJSON res)