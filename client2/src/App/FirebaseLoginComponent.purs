module App.FirebaseLoginComponent where

import Prelude
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

foreign import data FirebaseUI :: Type

type Input
  = {}

type Slot
  = H.Slot Query Output

data Query a

type Output
  = Void

data Action
  = Initialize
  | Finalize

type State
  = {}

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , initialize = Just Initialize
              , finalize = Just Finalize
              }
    }

initialState :: Input -> State
initialState _ = {}

render :: forall m. State -> H.ComponentHTML Action () m
render =
  const
    $ HH.div
        [ HP.ref (H.RefLabel "firebaseui") ]
        []

foreign import firebaseUI :: Effect FirebaseUI

foreign import attachFirebaseUIToElement :: HTMLElement -> FirebaseUI -> Effect Unit

foreign import onAuthStateChangedImpl :: forall a. (User -> Maybe User) -> Maybe User -> (Maybe User -> Effect a) -> Effect Unit

type User
  = { displayName :: String
    , uid :: String
    }

onAuthStateChanged :: forall a. (Maybe User -> Effect a) -> Effect Unit
onAuthStateChanged = onAuthStateChangedImpl Just Nothing

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    H.getHTMLElementRef (H.RefLabel "firebaseui")
      >>= traverse_ \element -> do
          fbui <- H.liftEffect $ firebaseUI
          H.liftEffect $ attachFirebaseUIToElement element fbui
  Finalize -> pure unit

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery _ = pure Nothing
