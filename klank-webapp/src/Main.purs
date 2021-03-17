module Main where

import Prelude
import App.App (AppMode(..), noDiceSafari)
import App.App as App
import App.ClickPlayModal as ClickPlayModal
import App.LoadingModal as LoadingModal
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object as Object
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Storybook (Stories, runStorybook, proxy)
import Halogen.VDom.Driver (runUI)

playRender render =
  H.mkComponent
    { initialState: \_ -> { open: true }
    , render
    , eval:
        H.mkEval H.defaultEval
    }

loadingRender pgrs render =
  H.mkComponent
    { initialState: \_ -> { open: true, progressSoFar: pgrs, progressMax: 100.0 }
    , render
    , eval:
        H.mkEval H.defaultEval
    }

noDiceSafariStory :: forall query input output m. H.Component query input output m
noDiceSafariStory =
  H.mkComponent
    { initialState: \_ -> {}
    , render:
        \_ ->
          HH.div [ HP.classes $ map HH.ClassName [ "h-screen", "w-screen" ] ]
            [ HH.div
                [ HP.classes $ map HH.ClassName [ "h-full", "w-full", "flex", "flex-col" ] ]
                noDiceSafari
            ]
    , eval:
        H.mkEval H.defaultEval
    }

stories :: forall m. Stories m
stories =
  Object.fromFoldable
    [ Tuple "loading 50%" $ proxy (loadingRender 50.0 LoadingModal.loading)
    , Tuple "loading 10%" $ proxy (loadingRender 10.0 LoadingModal.loading)
    , Tuple "loading 0%" $ proxy (loadingRender 0.0 LoadingModal.loading)
    , Tuple "loading 95%" $ proxy (loadingRender 95.0 LoadingModal.loading)
    , Tuple "play" $ proxy (playRender ClickPlayModal.clickPlay)
    , Tuple "no dice" $ proxy noDiceSafariStory
    ]

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    -- runStorybook { stories, logo: Nothing } body
    runUI (App.component KlankDev) unit body
