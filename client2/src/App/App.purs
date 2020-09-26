module App.App where

import Prelude
import App.AceComponent as AceComponent
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

_ace = SProxy :: SProxy "ace"

{-, --, HH.div_ --    [ HH.slot _ace Editor AceComponent.component unit (Just <<< HandleAceUpdate) ]   
    -}
type State
  = { count :: Int
    , editorText :: Maybe String
    }

data WhichAce
  = Editor
  | Console

derive instance genericWhichAce :: Generic WhichAce _

derive instance eqWhichAce :: Eq WhichAce

instance showWhichAce :: Show WhichAce where
  show = genericShow

instance ordWhichAce :: Ord WhichAce where
  compare a b = compare (show a) (show b)

type ChildSlots
  = ( ace :: AceComponent.Slot WhichAce
    )

data Action
  = Increment
  | HandleAceUpdate AceComponent.Output

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0, editorText: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.p_
        [ HH.text $ "You clicked " <> show state.count <> " times" ]
    , HH.button
        [ HE.onClick \_ -> Just Increment ]
        [ HH.text "Click me" ]
    , HH.div
        [ HP.classes $ map ClassName [ "grid", "grid-rows-3", "grid-flow-col", "gap-4" ] ]
        [ HH.div [ HP.classes $ map ClassName [ "row-span-3" ] ]
            [ HH.text "hello1"
            ]
        , HH.div [ HP.classes $ map ClassName [ "row-span-1", "col-span-2" ] ]
            [ HH.text "hello2"
            ]
        , HH.div [ HP.classes $ map ClassName [ "row-span-2", "col-span-2" ] ]
            [ HH.text "hello3"
            ]
        ]
    {-<div class="grid grid-rows-3 grid-flow-col gap-4">
  <div class="row-span-3 ..."></div>
  <div class="row-span-1 col-span-2 ..."></div>
  <div class="row-span-2 col-span-2 ..."></div>
</div>-}
    ]

handleAction :: forall o m. MonadAff m => Action → H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Increment -> H.modify_ \st -> st { count = st.count + 1 }
  HandleAceUpdate msg -> handleAceOuput msg

handleAceOuput :: forall o m. MonadAff m => AceComponent.Output -> H.HalogenM State Action ChildSlots o m Unit
handleAceOuput = case _ of
  AceComponent.TextChanged editorText -> H.modify_ (_ { editorText = Just editorText })
