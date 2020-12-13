module Main where

import Prelude
import App.App (noDice)
import App.App as App
import App.ClickPlayModal as ClickPlayModal
import App.LoadingModal as LoadingModal
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object as Object
import Halogen (Component, ComponentSlot)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Storybook (Stories, runStorybook, proxy)
import Halogen.VDom.Driver (runUI)

playRender :: ∀ t15 t2 t20 t23 t3 t5 t9. ({ open ∷ Boolean } → t9 (ComponentSlot t9 t5 t2 t15) t15) → Component t9 t23 t20 t3 t2
playRender render =
  H.mkComponent
    { initialState: \_ -> { open: true }
    , render
    , eval:
        H.mkEval H.defaultEval
    }

loadingRender :: ∀ t29 t30 t32 t36 t42 t47 t50. Number -> ({ open ∷ Boolean, progressMax ∷ Number, progressSoFar ∷ Number } → t36 (ComponentSlot t36 t32 t29 t42) t42) → Component t36 t50 t47 t30 t29
loadingRender pgrs render =
  H.mkComponent
    { initialState: \_ -> { open: true, progressSoFar: pgrs, progressMax: 100.0 }
    , render
    , eval:
        H.mkEval H.defaultEval
    }

noDiceStory :: ∀ t26 t27 t64 t67. Component HH.HTML t67 t64 t27 t26
noDiceStory =
  H.mkComponent
    { initialState: \_ -> {}
    , render:
        \_ ->
          HH.div [ HP.classes $ map HH.ClassName [ "h-screen", "w-screen" ] ]
            [ HH.div
                [ HP.classes $ map HH.ClassName [ "h-full", "w-full", "flex", "flex-col" ] ]
                noDice
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
    , Tuple "no dice" $ proxy noDiceStory
    ]

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    -- runStorybook { stories, logo: Nothing } body
    runUI App.component unit body
