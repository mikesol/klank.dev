module Klank.Weblib.XTermComponent where

import Prelude
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String as DS
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Klank.Weblib.ComponentTypes (XTermOutput(..))
import Klank.Weblib.XTermTheme (XTermTheme, darkTheme)
import Web.HTML (HTMLElement)

foreign import data XTerm :: Type

type Input
  = { terminalStyling :: XTerm -> Effect Unit }

type Slot
  = H.Slot Query Output

data Query a
  = ChangeText String a

type Output
  = XTermOutput

data Action
  = Initialize
  | Finalize
  | AppendChar String
  | HandleChange String

-- | The state for the ace component - we only need a reference to the editor,
-- | as Ace editor has its own internal state that we can query instead of
-- | replicating it within Halogen.
type State
  = { currentLine :: String
    , terminal :: Maybe XTerm
    , terminalStyling :: XTerm -> Effect Unit
    }

-- | The Ace component definition.
component :: forall m. MonadAff m => H.Component Query Input Output m
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
initialState { terminalStyling } =
  { currentLine: ""
  , terminal: Nothing
  , terminalStyling
  }

-- As we're embedding a 3rd party component we only need to create a placeholder
-- div here and attach the ref property which will let us reference the element
-- in eval.
render :: forall m. State -> H.ComponentHTML Action () m
render =
  const
    $ HH.div
        [ HP.ref (H.RefLabel "xterm")
        , HP.classes $ map ClassName [ "h-full", "w-full" ]
        ]
        []

foreign import newTerminal :: XTermTheme -> Effect XTerm

foreign import attachTerminalToElement :: HTMLElement -> XTerm -> Effect Unit

foreign import monitorForChange :: XTerm -> (String -> Effect Unit) -> Effect Unit

foreign import writeText :: String -> XTerm -> Effect Unit

foreign import setFontSize :: Int -> XTerm -> Effect Unit

foreign import focus :: XTerm -> Effect Unit

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    H.getHTMLElementRef (H.RefLabel "xterm")
      >>= traverse_ \element -> do
          xterm <- H.liftEffect $ newTerminal darkTheme
          _ <- H.liftEffect $ attachTerminalToElement element xterm
          terminalStyling <- H.gets _.terminalStyling
          _ <- H.liftEffect $ terminalStyling xterm
          H.modify_ (_ { terminal = Just xterm })
          { emitter, listener } <- H.liftEffect HS.create
          void $ H.subscribe emitter
          H.liftEffect
            $ monitorForChange xterm (HS.notify listener <<< AppendChar)
  Finalize -> do
    -- Release the reference to the editor and do any other cleanup that a
    -- real world component might need.
    H.modify_ (_ { terminal = Nothing })
  AppendChar s -> do
    case s of
      _
        | s == "\r" || s == "\x03" ->
          ( do
              st <- H.gets identity
              H.raise $ XTermTextChanged st.currentLine
              H.modify_ (_ { currentLine = "" })
          )
        | s == "\x7F" ->
          ( do
              st <- H.gets identity
              H.modify_
                ( \i ->
                    let
                      l = DS.length i.currentLine
                    in
                      i
                        { currentLine = if l == 0 then "" else DS.take (l - 1) i.currentLine
                        }
                )
              if st.currentLine == "" then
                pure unit
              else case st.terminal of
                Nothing -> pure unit
                Just xterm -> do
                  void $ H.liftEffect $ writeText "\x08 \x08" xterm
          )
        | otherwise ->
          ( do
              st <- H.gets identity
              H.modify_ (\i -> i { currentLine = i.currentLine <> s })
              case st.terminal of
                Nothing -> pure unit
                Just xterm -> do
                  void $ H.liftEffect $ writeText s xterm
          )
  HandleChange s -> do
    H.liftEffect (log $ "handling change")
    H.raise $ XTermTextChanged s

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  ChangeText text next -> do
    maybeTerminal <- H.gets _.terminal
    case maybeTerminal of
      Nothing -> pure unit
      Just xterm -> do
        void $ H.liftEffect $ writeText text xterm
    pure (Just next)
