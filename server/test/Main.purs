module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Main (compiler)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main ∷ Effect Unit
main =
  launchAff_
    $ runSpec' (defaultConfig { timeout = Just $ Milliseconds 10000.0 }) [ consoleReporter ] do
        describe "Compiler" do
          it "Compiles correct code" do
            res <-
              compiler
                { body:
                    { code: "module Foo.Bar.Baz where\nimport Prelude\nhello = \"hello\" :: String"
                    }
                }
            res.error `shouldEqual` Nothing
          it "Fails incorrect code" do
            res <-
              compiler
                { body:
                    { code: "m"
                    }
                }
            res.error `shouldNotEqual` Nothing
