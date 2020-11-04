module Main where

import Prelude
import App.App as App
import App.ClickPlayModal as ClickPlayModal
import App.LoadingModal as LoadingModal
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object as Object
import Halogen (Component, ComponentSlot)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Storybook (Stories, runStorybook, proxy)
import Halogen.VDom.Driver (runUI)

modalStory :: ∀ t15 t2 t20 t23 t3 t5 t9. ({ open ∷ Boolean } → t9 (ComponentSlot t9 t5 t2 t15) t15) → Component t9 t23 t20 t3 t2
modalStory render =
  H.mkComponent
    { initialState: \_ -> { open: true }
    , render
    , eval:
        H.mkEval H.defaultEval
    }

stories :: forall m. Stories m
stories =
  Object.fromFoldable
    [ Tuple "loading" $ proxy (modalStory LoadingModal.loading)
    , Tuple "play" $ proxy (modalStory ClickPlayModal.clickPlay)
    ]

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    -- runStorybook { stories, logo: Nothing } body
    runUI App.component unit body
