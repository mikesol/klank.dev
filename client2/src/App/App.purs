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
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.Base64 (decode)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, throw)
import FRP.Behavior.Audio (AudioContext, AudioInfo, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, VisualInfo, makeAudioContext, audioWorkletAddModule)
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

foreign import firebaseUrl :: Effect String

foreign import firebaseToken :: Effect String

foreign import getK :: Effect Boolean

foreign import ifpsGet :: String -> Effect (Promise String)

foreign import ifpsPut :: String -> Effect (Promise String)

foreign import getB64 :: Maybe String -> (String -> Maybe String) -> Effect (Maybe String)

foreign import getIPFS :: Maybe String -> (String -> Maybe String) -> Effect (Maybe String)

foreign import escape :: String -> Effect String

foreign import canvasDimensionHack :: Effect Unit

foreign import copyToClipboard :: String -> Effect Unit

foreign import completelyUnsafeEval :: String -> Effect Unit

foreign import canvasOrBust :: Effect CanvasElement

foreign import getKlank ::
  forall accumulator.
  Effect
    { enableMicrophone :: Boolean
    , accumulator :: (accumulator -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
    , worklets :: (Array String) -> (Array String -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
    , tracks :: (Object BrowserAudioTrack) -> (Object BrowserAudioTrack -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
    , buffers :: AudioContext -> (Object BrowserAudioBuffer) -> (Object BrowserAudioBuffer -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
    , floatArrays :: (Object BrowserFloatArray) -> (Object BrowserFloatArray -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
    , periodicWaves :: AudioContext -> (Object BrowserPeriodicWave) -> (Object BrowserPeriodicWave -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
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

type State accumulator
  = { editorText :: String
    , mainDisplay :: MainDisplay
    , stopFn :: Maybe (Effect Unit)
    , audioCtx :: Maybe AudioContext
    , worklets :: Array String
    , tracks :: Object BrowserAudioTrack
    , buffers :: Object BrowserAudioBuffer
    , floatArrays :: Object BrowserFloatArray
    , periodicWaves :: Object BrowserPeriodicWave
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

affable :: forall a. ((a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit) -> Aff a
affable f =
  makeAff \cb -> do
    f (cb <<< Right) (cb <<< Left)
    pure mempty

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState:
        \_ ->
          { editorText: initialPS
          , mainDisplay: EditorDisplay
          , stopFn: Nothing
          , audioCtx: Nothing
          , worklets: []
          , tracks: O.empty
          , buffers: O.empty
          , floatArrays: O.empty
          , periodicWaves: O.empty
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

render :: forall a m. MonadAff m => (State a) -> H.ComponentHTML Action ChildSlots m
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

handleAction :: forall a o m. MonadAff m => Action → H.HalogenM (State a) Action ChildSlots o m Unit
handleAction = case _ of
  Initialize -> do
    b64 <- H.liftEffect $ getB64 Nothing Just
    ipfs <- H.liftEffect $ getIPFS Nothing Just
    k <- H.liftEffect $ getK
    case b64 of
      Nothing -> pure unit
      Just txt ->
        ( do
            let
              editorText' = decode txt
            either (const $ pure unit)
              ( \editorText -> do
                  H.modify_ (_ { editorText = editorText })
                  _ <- H.query _ace Editor $ H.tell (AceComponent.ChangeText editorText)
                  pure unit
              )
              editorText'
        )
    case ipfs of
      Nothing -> pure unit
      Just txt ->
        ( do
            editorText <- H.liftAff (toAffE $ ifpsGet txt)
            H.modify_ (_ { editorText = editorText })
            _ <- H.query _ace Editor $ H.tell (AceComponent.ChangeText editorText)
            pure unit
        )
    case k of
      true -> compile
      false -> pure unit
    pure mempty
  HandleAceUpdate msg -> handleAceOuput msg
  HandleTerminalUpdate msg -> handleTerminalOutput msg

handleAceOuput :: forall a o m. MonadAff m => AceComponent.Output -> H.HalogenM (State a) Action ChildSlots o m Unit
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

compile :: ∀ t119 t123 t124 t125 t126 t140 t293. MonadEffect t123 ⇒ MonadAff t123 ⇒ H.HalogenM t126 t125 ( ace ∷ H.Slot AceComponent.Query t119 WhichAce, xterm ∷ H.Slot XTermComponent.Query t140 WhichTerm | t293 ) t124 t123 Unit
compile = do
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
            $ H.tell (XTermComponent.ChangeText $ "\r\nCompiling. Please be patient.\r\nThis should take 2-8 seconds depending on your internet connection speed.")
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
                                  $ H.tell (XTermComponent.ChangeText $ "\r\nSorry, your code did not compile. Here is the error message:\r\n" <> either (const "Unidentified error") (replaceAll (Pattern "\n") (Replacement "\r\n")) error_ <> "\r\n$ ")
                              pure unit
                          )
                      )
                      ( \res -> do
                          H.liftEffect $ completelyUnsafeEval res
                          _ <-
                            H.query
                              _xterm
                              Terminal
                              $ H.tell (XTermComponent.ChangeText $ "\r\nSuccess! Your code is compiled.\r\np to play, s to stop.\r\n$ ")
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

handleTerminalOutput :: forall a o m. MonadAff m => XTermComponent.Output -> H.HalogenM (State a) Action ChildSlots o m Unit
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
        H.liftEffect canvasDimensionHack
      Right CLI.Canvas -> do
        void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\n$ "))
        H.modify_ (_ { mainDisplay = CanvasDisplay })
        H.liftEffect canvasDimensionHack
      Right CLI.Play -> do
        stopper
        klank <- H.liftEffect getKlank
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
        accumulator <- H.liftAff (affable $ klank.accumulator)
        prevWorklets <- H.gets _.worklets
        worklets <- H.liftAff (affable $ klank.worklets prevWorklets)
        H.modify_ (_ { worklets = worklets })
        -------------
        ----- maybe it's just superstition
        ---- but i think this didn't work unless I explicitly asked for a variable `o`
        --- instead of _ <-
        --------- weird...
        o <- traverse (H.liftAff <<< toAffE <<< audioWorkletAddModule ctx) worklets
        prevTracks <- H.gets _.tracks
        tracks <- H.liftAff (affable $ klank.tracks prevTracks)
        H.modify_ (_ { tracks = tracks })
        prevBuffers <- H.gets _.buffers
        buffers <- H.liftAff (affable $ klank.buffers ctx prevBuffers)
        H.modify_ (_ { buffers = buffers })
        prevFloatArrays <- H.gets _.floatArrays
        floatArrays <- H.liftAff (affable $ klank.floatArrays prevFloatArrays)
        H.modify_ (_ { floatArrays = floatArrays })
        prevPeriodicWaves <- H.gets _.periodicWaves
        periodicWaves <- H.liftAff (affable $ klank.periodicWaves ctx prevPeriodicWaves)
        H.modify_ (_ { periodicWaves = periodicWaves })
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
      Right CLI.Compile -> compile
      Right CLI.Link -> do
        url <- H.liftEffect firebaseUrl
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
          ( \code' -> do
              code <- maybe (H.liftEffect $ throw "Could not retrieve the code") pure code'
              _ <-
                H.query
                  _xterm
                  Terminal
                  $ H.tell (XTermComponent.ChangeText $ "\r\nGenerating a link and copying it to your clipboard.")
              asIPFS <- H.liftAff $ (toAffE $ ifpsPut code)
              firebaseT <- H.liftEffect firebaseToken
              response <-
                H.liftAff
                  $ AX.request
                      ( AX.defaultRequest
                          { headers = []
                          , method = Left POST
                          , url = url <> "?key=" <> firebaseT
                          , content =
                            ( Just
                                ( RequestBody.json
                                    $ encodeJson
                                        { dynamicLinkInfo:
                                            { domainUriPrefix: "https://link.klank.dev"
                                            , link: "https://klank.dev/?k&ipfs=" <> asIPFS
                                            }
                                        }
                                )
                            )
                          , responseFormat = AXRF.json
                          }
                      )
              either
                ( const do
                    _ <-
                      H.query
                        _xterm
                        Terminal
                        $ H.tell (XTermComponent.ChangeText $ "\r\nThe link shortening server is either down or overloaded. Try again, and if your request doesn't work a second time, please file a bug. Sorry!\r\n$ ")
                    pure unit
                )
                ( \{ body } -> do
                    let
                      body' = toObject body
                    maybe (pure unit)
                      ( \body'' -> do
                          let
                            link_ = getField body'' "shortLink"
                          either
                            ( const
                                ( do
                                    _ <-
                                      H.query
                                        _xterm
                                        Terminal
                                        $ H.tell (XTermComponent.ChangeText $ "\r\nSorry, we couldn't create a link. Please try again later.\r\n$ ")
                                    pure unit
                                )
                            )
                            ( \res -> do
                                H.liftEffect $ copyToClipboard res
                                _ <-
                                  H.query
                                    _xterm
                                    Terminal
                                    $ H.tell (XTermComponent.ChangeText $ "\r\nYour link has been copied to the clipboard. Share with reckless abandon!\r\n$ ")
                                pure unit
                            )
                            link_
                      )
                      body'
                )
                response
              pure unit
          )
          txt_
    pure unit
