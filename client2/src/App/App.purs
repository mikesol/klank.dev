module App.App where

import Prelude
import Ace (Position(..))
import Ace.EditSession as EditSession
import Ace.Editor as Editor
import App.AceComponent as AceComponent
import App.CanvasComponent as CanvasComponent
import App.CLI as CLI
import App.InitialPS (helpMsg, initialPS, welcomeMsg)
import App.XTermComponent (focus, setFontSize, writeText)
import App.XTermComponent as XTermComponent
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Text.Parsing.Parser (runParser)

_ace = SProxy :: SProxy "ace"

_xterm = SProxy :: SProxy "xterm"

_canvas = SProxy :: SProxy "canvas"

data MainDisplay
  = EditorDisplay
  | CanvasDisplay
  | SplitDisplay

type State
  = { editorText :: String
    , mainDisplay :: MainDisplay
    }

data WhichAce
  = Editor

derive instance genericWhichAce :: Generic WhichAce _

derive instance eqWhichAce :: Eq WhichAce

instance showWhichAce :: Show WhichAce where
  show = genericShow

instance ordWhichAce :: Ord WhichAce where
  compare a b = compare (show a) (show b)

data WhichTerm
  = Terminal

derive instance genericWhichTerm :: Generic WhichTerm _

derive instance eqWhichTerm :: Eq WhichTerm

instance showWhichTerm :: Show WhichTerm where
  show = genericShow

instance ordWhichTerm :: Ord WhichTerm where
  compare a b = compare (show a) (show b)

data WhichCanvas
  = Canvas

derive instance genericWhichCanvas :: Generic WhichCanvas _

derive instance eqWhichCanvas :: Eq WhichCanvas

instance showWhichCanvas :: Show WhichCanvas where
  show = genericShow

instance ordWhichCanvas :: Ord WhichCanvas where
  compare a b = compare (show a) (show b)

type ChildSlots
  = ( ace :: AceComponent.Slot WhichAce
    , xterm :: XTermComponent.Slot WhichTerm
    , canvas :: CanvasComponent.Slot WhichCanvas
    )

data Action
  = Initialize
  | HandleAceUpdate AceComponent.Output
  | HandleTerminalUpdate XTermComponent.Output

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState:
        \_ ->
          { editorText: initialPS
          , mainDisplay: EditorDisplay
          }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

editorDisplay :: ∀ m. MonadAff m ⇒ String → HH.HTML (H.ComponentSlot HH.HTML ChildSlots m Action) Action
editorDisplay editorText =
  HH.slot _ace Editor AceComponent.component
    { editorStyling:
        \e -> do
          Editor.setTheme "ace/theme/monokai" e
          Editor.setShowPrintMargin false e
          session <- Editor.getSession e
          EditSession.setMode "ace/mode/haskell" session
          Editor.setFontSize "20px" e
          void $ Editor.setValue editorText Nothing e
          Editor.moveCursorToPosition
            ( Position
                { row: 0
                , column: 0
                }
            )
            e
    }
    (Just <<< HandleAceUpdate)

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render { editorText, mainDisplay } =
  HH.div [ HP.classes $ map ClassName [ "h-screen", "w-screen" ] ]
    [ HH.div
        [ HP.classes $ map ClassName [ "h-full", "w-full", "flex", "flex-col" ] ]
        [ HH.div [ HP.classes $ map ClassName [ "flex-grow" ] ] case mainDisplay of
            EditorDisplay -> [ editorDisplay editorText ]
            CanvasDisplay ->
              [ HH.slot _canvas Canvas CanvasComponent.component
                  {}
                  absurd
              ]
            SplitDisplay ->
              [ HH.div [ HP.classes $ map ClassName [ "h-full", "w-full", "grid", "grid-cols-2", "grid-rows-1", "gap-0" ] ]
                  [ editorDisplay editorText
                  , HH.slot _canvas Canvas CanvasComponent.component
                      {}
                      absurd
                  ]
              ]
        , HH.div [ HP.classes $ map ClassName [ "flex-grow-0" ] ]
            [ HH.slot _xterm Terminal XTermComponent.component
                { terminalStyling:
                    \t -> do
                      setFontSize 20 t
                      writeText welcomeMsg t
                      focus t
                }
                (Just <<< HandleTerminalUpdate)
            ]
        ]
    ]

handleAction :: forall o m. MonadAff m => Action → H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Initialize -> pure mempty -- no longer used, use if need init
  HandleAceUpdate msg -> handleAceOuput msg
  HandleTerminalUpdate msg -> handleTerminalOutput msg

handleAceOuput :: forall o m. MonadAff m => AceComponent.Output -> H.HalogenM State Action ChildSlots o m Unit
handleAceOuput = case _ of
  AceComponent.TextChanged editorText -> H.modify_ (_ { editorText = editorText })

handleTerminalOutput :: forall o m. MonadAff m => XTermComponent.Output -> H.HalogenM State Action ChildSlots o m Unit
handleTerminalOutput = case _ of
  XTermComponent.TextChanged tt -> do
    let
      parserRes = runParser tt CLI.cli
    case parserRes of
      Left _ -> void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\nSorry, I didn't understand \"" <> tt <> "\"\r\nPlease type h and ENTER to list commands\r\n$ "))
      Right CLI.Help -> void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ ("\r\n" <> helpMsg <> "\r\n$ ")))
      Right CLI.Editor -> do
        void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\n$ "))
        H.modify_ (_ { mainDisplay = EditorDisplay })
      Right CLI.EditorCanvas -> do
        void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\n$ "))
        H.modify_ (_ { mainDisplay = SplitDisplay })
      Right CLI.Canvas -> do
        void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\n$ "))
        H.modify_ (_ { mainDisplay = CanvasDisplay })
      Right CLI.Play -> void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\nStub for play\r\n$ "))
      Right CLI.Stop -> void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\nStub for play\r\n$ "))
      Right CLI.Compile -> void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\nStub for play\r\n$ "))
    pure unit
