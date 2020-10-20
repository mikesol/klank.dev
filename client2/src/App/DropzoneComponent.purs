module App.DropzoneComponent where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.File.File (File)
import Halogen.Query.EventSource as ES

type Input
  = {}

data Output
  = FileDropped File

data Action
  = Initialize
  | AcceptFile File

-- | The state for the ace component - we only need a reference to the editor,
-- | as Ace editor has its own internal state that we can query instead of
-- | replicating it within Halogen.
type State
  = {}

-- | The Ace component definition.
component :: forall m q. MonadAff m => H.Component HH.HTML q Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

initialState :: Input -> State
initialState = const {}

type Slot query
  = H.Slot query Output

-- As we're embedding a 3rd party component we only need to create a placeholder
-- div here and attach the ref property which will let us reference the element
-- in eval.
render :: forall m. State -> H.ComponentHTML Action () m
render =
  const
    $ HH.div [ HP.classes $ map ClassName [ "h-full", "w-full" ] ]
        [ HH.p
            [ HP.classes $ map ClassName [ "text-2xl", "font-bold" ]
            ]
            [ HH.text "Audio file dropper" ]
        , HH.p [] [ HH.text "You can your audio files below and they will be read into the current session cache as a buffer. The cache is the second argument passed to `buffers` function. The key for playBuf or loopBuf will be the filename, so make sure to use unique names for each file!" ]
        , HH.p [] [ HH.text "Note that this does not upload the files to a server, just to your session. In the future, there may be a hosting option." ]
        , HH.p [] [ HH.text "If you see your files below, that means that they have been loaded into the cache and you can flip back to the editor view with the command `e`." ]
        , HH.div
            [ HP.id_ "audiodrop"
            , HP.classes $ map ClassName [ "h-full", "w-full", "dropzone", "needsclick", "dz-clickable" ]
            ]
            [ HH.div [ HP.classes $ map ClassName [ "dz-message", "needsclick" ] ] [ HH.button [ HP.classes $ map ClassName [ "dz-button" ] ] [ HH.text "Drop files here or click to upload" ] ]
            ]
        ]

foreign import setUpDropzone :: (File -> Effect Unit) -> Effect Unit

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    void $ H.subscribe
      $ ES.effectEventSource \emitter -> do
          setUpDropzone (ES.emit emitter <<< AcceptFile)
          pure mempty
    pure mempty
  AcceptFile filez -> H.raise $ FileDropped filez
