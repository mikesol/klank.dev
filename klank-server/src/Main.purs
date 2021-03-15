module Main where

import Prelude
import Data.Array (catMaybes, head, intercalate)
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
import Node.Process (lookupEnv)
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

tearDown =
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

compiler :: { body :: Code } -> Aff Compiled
compiler { body } =
  bracket
    ( do
        uuid' <- liftEffect $ genUUID
        _ <- liftEffect $ mkdir (toString uuid')
        pure (toString uuid')
    )
    (const $ pure unit)
    ( \uuid -> do
        _ <- liftEffect $ log "Trying to compile on lambda path"
        _ <-
          liftEffect
            $ writeTextFile
                UTF8
                (uuid <> ".dhall")
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
                (uuid <> "/index_.js")
                ("window.klank = require(\"../output/" <> renamed.moduleName <> "/\").main")
        _ <-
          liftEffect
            $ writeTextFile
                UTF8
                (uuid <> "/Main.purs")
                renamed.code
        whatHappened <-
          spawn
            { args: [ "spago", "-x", (uuid <> ".dhall"), "build" ]
            , cmd: "npx"
            , stdin: Nothing
            }
            defaultSpawnOptions
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
        worked <- liftEffect $ exists (uuid <> "/index.js")
        if worked then
          ( do
              res <-
                liftEffect
                  $ readTextFile
                      UTF8
                      (uuid <> "/index.js")
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
