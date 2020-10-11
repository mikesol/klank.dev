module App.CanvasComponent where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (Drawing)
import Graphics.Drawing as GD
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Input
  = {}

type Slot
  = H.Slot Query Output

data Query a
  = ChangeDrawing Drawing a

type Output
  = Void

data Action
  = Initialize
  | Finalize

-- | The state for the ace component - we only need a reference to the editor,
-- | as Ace editor has its own internal state that we can query instead of
-- | replicating it within Halogen.
type State
  = {}

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
initialState = const {}

-- As we're embedding a 3rd party component we only need to create a placeholder
-- div here and attach the ref property which will let us reference the element
-- in eval.
render :: forall m. State -> H.ComponentHTML Action () m
render =
  const
    $ HH.canvas
        [ HP.id_ "klank-canvas"
        , HP.classes $ map ClassName [ "h-full", "w-full" ]
        ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> pure mempty
  Finalize -> pure mempty

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  ChangeDrawing drawing next -> do
    ci' <- H.liftEffect $ getCanvasElementById "klank-canvas"
    H.liftEffect
      $ maybe (pure unit)
          ( \ci -> do
              ctx <- getContext2D ci
              GD.render ctx drawing
          )
          ci'
    pure (Just next)
