module App.App where

import Prelude
import Ace (Position(..))
import Ace.EditSession as EditSession
import Ace.Editor as Editor
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as AXRF
import App.AceComponent as AceComponent
import App.CLI as CLI
import App.CanvasComponent as CanvasComponent
import App.InitialPS (helpMsg, initialPS, welcomeMsg)
import App.XTermComponent (focus, setFontSize, writeText)
import App.XTermComponent as XTermComponent
import Control.Monad.State (class MonadState)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (getField, toObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
-- import Effect.Class.Console (log)
import FRP.Behavior.Audio (AudioContext, AudioInfo, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, VisualInfo, makeAudioContext)
import Foreign.Object (Object)
import Foreign.Object as O
import Graphics.Canvas (CanvasElement)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Text.Parsing.Parser (runParser)

foreign import data BrowserMicrophone :: Type

foreign import serverUrl :: Effect String

foreign import completelyUnsafeEval :: String -> Effect Unit

foreign import canvasOrBust :: Effect CanvasElement

foreign import getKlank ::
  forall accumulator.
  (forall anything. anything -> Aff anything) ->
  Effect
    { enableMicrophone :: Boolean
    , accumulator :: Aff accumulator
    , tracks :: Aff (Object BrowserAudioTrack)
    , buffers :: AudioContext -> Aff (Object BrowserAudioBuffer)
    , floatArrays :: Aff (Object BrowserFloatArray)
    , periodicWaves :: AudioContext -> Aff (Object BrowserPeriodicWave)
    , main ::
        accumulator ->
        Int ->
        Int ->
        AudioContext ->
        AudioInfo (Object BrowserMicrophone) (Object BrowserAudioTrack) (Object BrowserAudioBuffer) (Object BrowserFloatArray) (Object BrowserPeriodicWave) ->
        VisualInfo ->
        Effect (Effect Unit)
    }

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
    , stopFn :: Maybe (Effect Unit)
    , audioCtx :: Maybe AudioContext
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
          , stopFn: Nothing
          , audioCtx: Nothing
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

foreign import stopAudioContext :: AudioContext -> Effect Unit

foreign import loadCustomAudioNodes :: AudioContext -> Effect (Promise Unit)

foreign import getMicrophoneImpl :: Effect (Promise BrowserMicrophone)

getMicrophone :: Aff BrowserMicrophone
getMicrophone = toAffE getMicrophoneImpl

handleAction :: forall o m. MonadAff m => Action → H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Initialize -> do
    pure mempty
  HandleAceUpdate msg -> handleAceOuput msg
  HandleTerminalUpdate msg -> handleTerminalOutput msg

handleAceOuput :: forall o m. MonadAff m => AceComponent.Output -> H.HalogenM State Action ChildSlots o m Unit
handleAceOuput = case _ of
  AceComponent.TextChanged editorText -> H.modify_ (_ { editorText = editorText })

stopper :: ∀ t1 t8. Bind t1 ⇒ MonadState { stopFn :: Maybe (Effect Unit), audioCtx :: Maybe AudioContext | t8 } t1 ⇒ MonadEffect t1 ⇒ t1 Unit
stopper = do
  sfn <- H.gets _.stopFn
  ctx <- H.gets _.audioCtx
  H.modify_ (_ { stopFn = Nothing })
  H.modify_ (_ { audioCtx = Nothing })
  maybe (pure unit) H.liftEffect sfn
  maybe (pure unit) (H.liftEffect <<< stopAudioContext) ctx

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
      Right CLI.Play -> do
        stopper
        klank <- H.liftEffect $ getKlank pure
        ctx <- H.liftEffect makeAudioContext
        H.modify_ (_ { audioCtx = Just ctx })
        H.liftAff (toAffE $ loadCustomAudioNodes ctx)
        microphones <-
          if klank.enableMicrophone then
            ( do
                mic <- H.liftAff getMicrophone
                pure $ O.singleton "microphone" mic
            )
          else
            pure O.empty
        accumulator <- H.liftAff klank.accumulator
        tracks <- H.liftAff klank.tracks
        buffers <- H.liftAff $ klank.buffers ctx
        floatArrays <- H.liftAff klank.floatArrays
        periodicWaves <- H.liftAff $ klank.periodicWaves ctx
        turnMeOff <-
          H.liftEffect
            ( klank.main
                accumulator
                20
                15
                ctx
                { microphones, tracks, buffers, floatArrays, periodicWaves }
                { canvases: O.singleton "canvas" canvasOrBust }
            )
        H.modify_ (_ { stopFn = Just turnMeOff })
        _ <- H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\n$ ")
        pure unit
      Right CLI.Stop -> do
        _ <- H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\n$ ")
        stopper
      Right CLI.Compile -> do
        url <- H.liftEffect serverUrl
        txt_ <- H.query _ace Editor $ H.request AceComponent.GetText
        maybe
          ( do
              _ <-
                H.query
                  _xterm
                  Terminal
                  $ H.tell (XTermComponent.ChangeText $ "\r\nWell, this is embarassing. We can't access the DOM, so this website won't work. Please file a bug request!\r\n$ ")
              pure unit
          )
          ( \code -> do
              _ <-
                H.query
                  _xterm
                  Terminal
                  $ H.tell (XTermComponent.ChangeText $ "\r\nCompiling. Please be patient.\r\nThis should take 2-6 seconds depending on your internet connection speed.")
              response <-
                H.liftAff
                  $ AX.post AXRF.json url (Just (RequestBody.json $ encodeJson { code }))
              either
                ( const do
                    _ <-
                      H.query
                        _xterm
                        Terminal
                        $ H.tell (XTermComponent.ChangeText $ "\r\nThe klank server is down or overloaded. Try again, and if your request doesn't work a second time, please file a bug. Sorry!\r\n$ ")
                    pure unit
                )
                ( \{ body } -> do
                    let
                      body' = toObject body
                    maybe (pure unit)
                      ( \body'' -> do
                          let
                            error_ = getField body'' "error"
                          let
                            res_ = getField body'' "res"
                          either
                            ( const
                                ( do
                                    _ <-
                                      H.query
                                        _xterm
                                        Terminal
                                        $ H.tell (XTermComponent.ChangeText $ "\r\nSorry, your code did not compile. Here is the error message:\r\n" <> either (const "Unidentified error") (replace (Pattern "\n") (Replacement "\r\n")) error_ <> "\r\n$ ")
                                    pure unit
                                )
                            )
                            ( \res -> do
                                H.liftEffect $ completelyUnsafeEval res
                                _ <-
                                  H.query
                                    _xterm
                                    Terminal
                                    $ H.tell (XTermComponent.ChangeText $ "\r\nSuccess! You're code is compiled.\r\np to play, s to stop.\r\n$ ")
                                pure unit
                            )
                            res_
                      )
                      body'
                )
                response
              pure unit
          )
          txt_
    pure unit
