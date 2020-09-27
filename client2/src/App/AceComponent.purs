module App.AceComponent where

import Prelude
import Ace as Ace
import Ace.Config as Config
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Types (Editor)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES

type Input
  = { editorStyling :: Editor -> Effect Unit }

type Slot
  = H.Slot Query Output

data Query a
  = ChangeText String a

data Output
  = TextChanged String

data Action
  = Initialize
  | Finalize
  | HandleChange

-- | The state for the ace component - we only need a reference to the editor,
-- | as Ace editor has its own internal state that we can query instead of
-- | replicating it within Halogen.
type State
  = { editor :: Maybe Editor, editorStyling :: Editor -> Effect Unit }

-- | The Ace component definition.
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
initialState { editorStyling } = { editor: Nothing, editorStyling }

-- As we're embedding a 3rd party component we only need to create a placeholder
-- div here and attach the ref property which will let us reference the element
-- in eval.
render :: forall m. State -> H.ComponentHTML Action () m
render =
  const
    $ HH.div
        [ HP.ref (H.RefLabel "ace")
        , HP.classes $ map ClassName [ "h-full", "w-full" ]
        ]
        []

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    H.getHTMLElementRef (H.RefLabel "ace")
      >>= traverse_ \element -> do
          _ <-
            H.liftEffect
              $ Config.set Config.basePath "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.12/"
          editor <- H.liftEffect $ Ace.editNode element Ace.ace
          session <- H.liftEffect $ Editor.getSession editor
          editorStyling <- H.gets _.editorStyling
          _ <- H.liftEffect $ editorStyling editor
          H.modify_ (_ { editor = Just editor })
          void $ H.subscribe
            $ ES.effectEventSource \emitter -> do
                Session.onChange session (\_ -> ES.emit emitter HandleChange)
                pure mempty
  Finalize -> do
    -- Release the reference to the editor and do any other cleanup that a
    -- real world component might need.
    H.modify_ (_ { editor = Nothing })
  HandleChange -> do
    H.gets _.editor
      >>= traverse_ \editor -> do
          text <- H.liftEffect (Editor.getValue editor)
          H.raise $ TextChanged text

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  ChangeText text next -> do
    maybeEditor <- H.gets _.editor
    case maybeEditor of
      Nothing -> pure unit
      Just editor -> do
        current <- H.liftEffect $ Editor.getValue editor
        when (text /= current) do
          void $ H.liftEffect $ Editor.setValue text Nothing editor
    H.raise $ TextChanged text
    pure (Just next)
