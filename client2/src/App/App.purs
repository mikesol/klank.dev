module App.App where

import Prelude
import Ace (Position(..))
import Ace.EditSession as EditSession
import Ace.Editor as Editor
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import App.AceComponent as AceComponent
import App.AppAction (Action(..))
import App.CLI as CLI
import App.CanvasComponent as CanvasComponent
import App.ClickPlayModal (clickPlay)
import App.DropzoneComponent as DropzoneComponent
import App.InitialPS (helpMsg, initialPS, welcomeMsg)
import App.LinkModal (modal)
import App.LoadingModal (loading)
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
import FRP.Behavior.Audio (AudioContext, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, audioWorkletAddModule, makeAudioContext)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as O
import Graphics.Canvas (CanvasElement)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Text.Parsing.Parser (runParser)
import Type.Klank.Dev (Klank'')
import Web.File.File (File)
import Web.File.File as WF

foreign import data BrowserMicrophone :: Type

foreign import serverUrl :: Effect String

foreign import firebaseUrl :: Effect String

foreign import firebaseToken :: Effect String

foreign import getK :: Effect Boolean

foreign import getC :: Effect Boolean

foreign import getEC :: Effect Boolean

foreign import getNoterm :: Effect Boolean

foreign import getB64 :: Maybe String -> (String -> Maybe String) -> Effect (Maybe String)

foreign import getInitialAccumulator :: Maybe Foreign -> (Foreign -> Maybe Foreign) -> Effect (Maybe Foreign)

foreign import getUrl :: Maybe String -> (String -> Maybe String) -> Effect (Maybe String)

foreign import escape :: String -> Effect String

foreign import canvasDimensionHack :: Effect Unit

foreign import completelyUnsafeEval :: String -> Effect Unit

foreign import canvasOrBust :: Effect CanvasElement

foreign import getKlank ::
  forall accumulator env.
  Effect (Klank'' accumulator env)

foreign import bufferFromFile :: AudioContext -> File -> Effect (Promise BrowserAudioBuffer)

_ace = SProxy :: SProxy "ace"

_xterm = SProxy :: SProxy "xterm"

_canvas = SProxy :: SProxy "canvas"

_upload = SProxy :: SProxy "upload"

data MainDisplay
  = EditorDisplay
  | CanvasDisplay
  | SplitDisplay
  | UploadDisplay

type State
  = { editorText :: String
    , mainDisplay :: MainDisplay
    , stopFn :: Maybe (Effect Unit)
    , audioCtx :: Maybe AudioContext
    , initialAccumulator :: Maybe Foreign
    , worklets :: Array String
    , linkModalUrl :: String
    , loadingModalOpen :: Boolean
    , linkModalOpen :: Boolean
    , linkModalProperNoun :: String
    , playModalOpen :: Boolean
    , showTerminal :: Boolean
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

data WhichUploader
  = Uploader

derive instance genericWhichUploader :: Generic WhichUploader _

derive instance eqWhichUploader :: Eq WhichUploader

instance showWhichUploader :: Show WhichUploader where
  show = genericShow

instance ordWhichUploader :: Ord WhichUploader where
  compare a b = compare (show a) (show b)

type ChildSlots
  = ( ace :: AceComponent.Slot WhichAce
    , xterm :: XTermComponent.Slot WhichTerm
    , canvas :: CanvasComponent.Slot WhichCanvas
    , upload :: forall query. DropzoneComponent.Slot query WhichUploader
    )

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
          , loadingModalOpen: true
          , linkModalOpen: false
          , showTerminal: true
          , linkModalUrl: ""
          , linkModalProperNoun: "klank"
          , playModalOpen: false
          , initialAccumulator: Nothing
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

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render { editorText
, mainDisplay
, linkModalOpen
, linkModalUrl
, linkModalProperNoun
, loadingModalOpen
, playModalOpen
, showTerminal
} =
  HH.div [ HP.classes $ map ClassName [ "h-screen", "w-screen" ] ]
    [ HH.div
        [ HP.classes $ map ClassName [ "h-full", "w-full", "flex", "flex-col" ] ]
        ( join
            [ [ HH.div [ HP.classes $ map ClassName [ "flex-grow" ] ] case mainDisplay of
                  EditorDisplay -> [ editorDisplay editorText ]
                  UploadDisplay ->
                    [ HH.slot _upload Uploader DropzoneComponent.component
                        {}
                        (Just <<< HandleFileDrop)
                    ]
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
              ]
            , if showTerminal then
                [ HH.div [ HP.classes $ map ClassName [ "flex-grow-0" ] ]
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
              else
                []
            ]
        )
    , modal
        { url: linkModalUrl, open: linkModalOpen, properNoun: linkModalProperNoun
        }
    , clickPlay
        { open: playModalOpen
        }
    , loading
        { open: loadingModalOpen
        }
    ]

foreign import stopAudioContext :: AudioContext -> Effect Unit

foreign import loadCustomAudioNodes :: AudioContext -> Effect (Promise Unit)

foreign import getMicrophoneImpl :: Effect (Promise BrowserMicrophone)

getMicrophone :: Aff BrowserMicrophone
getMicrophone = toAffE getMicrophoneImpl

handleAction :: forall o m. MonadAff m => Action → H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  PlayKlankFromModal -> do
    H.modify_ (_ { playModalOpen = false })
    playKlank
  CloseLinkModal -> do
    H.modify_ (_ { linkModalOpen = false })
  Initialize -> do
    b64 <- H.liftEffect $ getB64 Nothing Just
    url <- H.liftEffect $ getUrl Nothing Just
    k <- H.liftEffect $ getK
    c <- H.liftEffect $ getC
    ec <- H.liftEffect $ getEC
    noterm <- H.liftEffect $ getNoterm
    when noterm
      ( do
          H.modify_
            ( _
                { showTerminal = false
                , mainDisplay = CanvasDisplay
                }
            )
          H.liftEffect canvasDimensionHack
      )
    initialAccumulator <- H.liftEffect $ getInitialAccumulator Nothing Just
    when ec
      ( do
          H.modify_ (_ { mainDisplay = SplitDisplay })
          H.liftEffect canvasDimensionHack
      )
    when c
      ( do
          H.modify_ (_ { mainDisplay = CanvasDisplay })
          H.liftEffect canvasDimensionHack
      )
    H.modify_ (_ { initialAccumulator = initialAccumulator })
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
    case url of
      Nothing -> pure unit
      Just txt ->
        ( do
            result <-
              H.liftAff
                $ AX.request
                    ( AX.defaultRequest
                        { url = txt
                        , method = Left GET
                        , responseFormat = AXRF.string
                        }
                    )
            editorText <-
              either (\_ -> H.liftEffect $ throw "Could not retrieve")
                (pure <<< _.body)
                result
            H.modify_ (_ { editorText = editorText })
            _ <- H.query _ace Editor $ H.tell (AceComponent.ChangeText editorText)
            pure unit
        )
    case (k || noterm) of
      true -> compile
      false -> pure unit
    if noterm then
      ( do
          H.modify_
            ( _
                { playModalOpen = true
                , loadingModalOpen = false
                }
            )
      )
    else
      H.modify_ (_ { loadingModalOpen = false })
    pure mempty
  HandleAceUpdate msg -> handleAceOuput msg
  HandleTerminalUpdate msg -> handleTerminalOutput msg
  HandleFileDrop msg -> handleUploaderOutput msg

handleAceOuput :: forall o m. MonadAff m => AceComponent.Output -> H.HalogenM State Action ChildSlots o m Unit
handleAceOuput = case _ of
  AceComponent.TextChanged editorText -> H.modify_ (_ { editorText = editorText })

handleUploaderOutput :: forall o m. MonadAff m => DropzoneComponent.Output -> H.HalogenM State Action ChildSlots o m Unit
handleUploaderOutput = case _ of
  DropzoneComponent.FileDropped file -> do
    ctx <- H.liftEffect makeAudioContext
    newBuffer <- H.liftAff $ toAffE (bufferFromFile ctx file)
    H.modify_ (\i -> i { buffers = O.union (O.singleton (WF.name file) newBuffer) i.buffers })

stopper :: ∀ t1 t8. Bind t1 ⇒ MonadState { stopFn :: Maybe (Effect Unit), audioCtx :: Maybe AudioContext | t8 } t1 ⇒ MonadEffect t1 ⇒ t1 Unit
stopper = do
  sfn <- H.gets _.stopFn
  ctx <- H.gets _.audioCtx
  H.modify_ (_ { stopFn = Nothing })
  H.modify_ (_ { audioCtx = Nothing })
  maybe (pure unit) H.liftEffect sfn
  maybe (pure unit) (H.liftEffect <<< stopAudioContext) ctx

compile :: ∀ t119 t123 t124 t125 t140 t293. MonadEffect t123 ⇒ MonadAff t123 ⇒ H.HalogenM State t125 ( ace ∷ H.Slot AceComponent.Query t119 WhichAce, xterm ∷ H.Slot XTermComponent.Query t140 WhichTerm | t293 ) t124 t123 Unit
compile = do
  url <- H.liftEffect serverUrl
  -- in case we are in compile mode, we use editorText
  txt_ <-
    (H.query _ace Editor $ H.request AceComponent.GetText)
      >>= (maybe (Just <$> H.gets _.editorText) pure)
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
                          -- H.liftEffect $ log res
                          H.liftEffect $ completelyUnsafeEval res
                          _ <-
                            H.query
                              _xterm
                              Terminal
                              $ H.tell (XTermComponent.ChangeText $ "\r\nSuccess! Your code is compiled.\r\nType p then ENTER to play, s then ENTER to stop.\r\n$ ")
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

playKlank :: ∀ t338 t341 m t346 t347. MonadEffect m ⇒ MonadAff m ⇒ H.HalogenM State t347 ( xterm ∷ H.Slot XTermComponent.Query t341 WhichTerm | t338 ) t346 m Unit
playKlank = do
  stopper
  _ <- H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\nRetrieving assets...")
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
  initialAccumulator <- H.gets _.initialAccumulator
  accumulator <- case initialAccumulator of
    Nothing -> H.liftAff (affable $ klank.accumulator)
    Just acc -> pure acc
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
      ( klank.run
          accumulator
          ctx
          klank.engineInfo
          { microphones, tracks, buffers, floatArrays, periodicWaves }
          { canvases: O.singleton "canvas" canvasOrBust }
          klank.exporter
      )
  H.modify_ (_ { stopFn = Just turnMeOff })
  _ <- H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\nPlaying\r\n$ ")
  pure unit

-- todo - avoid Firebase call for extra link
makeLink :: ∀ m t723 t724. MonadEffect m ⇒ MonadAff m ⇒ Boolean -> Boolean -> H.HalogenM State t724 ChildSlots t723 m Unit
makeLink noTerm justLink = do
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
            $ H.tell (XTermComponent.ChangeText $ "\r\nGenerating a link...")
        -- mx@
        surl <- H.liftEffect serverUrl
        _uploadLink' <-
          H.liftAff
            $ AX.request
                ( AX.defaultRequest
                    { headers = []
                    , method = Left POST
                    , url = (surl <> "u")
                    , content =
                      ( Just
                          ( RequestBody.json
                              $ encodeJson
                                  { code }
                          )
                      )
                    , responseFormat = AXRF.string
                    }
                )
        _uploadLink <-
          either
            ( \_ -> do
                _ <-
                  H.query
                    _xterm
                    Terminal
                    $ H.tell (XTermComponent.ChangeText $ "\r\nThe link shortening server is either down or overloaded. Try again, and if your request doesn't work a second time, please file a bug. Sorry!\r\n$ ")
                pure unit
                H.liftEffect $ throw "Could not process sound."
            )
            (pure <<< _.body)
            _uploadLink'
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
                                      -- mx@
                                      { domainUriPrefix: "https://link.klank.dev"
                                      , link: "https://klank.dev/?" <> (if noTerm then "noterm" else "k") <> "&url=" <> _uploadLink
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
          ( \{ status: (StatusCode sc), body } -> do
              let
                body' = toObject body
              maybe (pure unit)
                ( \body'' -> do
                    let
                      link_ = getField body'' "shortLink"
                    maybe
                      ( do
                          _ <-
                            H.query
                              _xterm
                              Terminal
                              $ H.tell (XTermComponent.ChangeText $ "\r\nSorry, we couldn't create a link. Please try again later.\r\n$ ")
                          pure unit
                      )
                      ( \res -> do
                          H.modify_
                            ( _
                                { linkModalUrl = if justLink then _uploadLink else res
                                , linkModalOpen = true
                                , linkModalProperNoun = if justLink then "file" else "klank"
                                }
                            )
                          _ <-
                            H.query
                              _xterm
                              Terminal
                              $ H.tell (XTermComponent.ChangeText $ "\r\n$ ")
                          pure unit
                      )
                      ((either (const Nothing) Just link_) >>= \x -> if (sc >= 400) then Nothing else Just x)
                )
                body'
          )
          response
        pure unit
    )
    txt_

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
      Right CLI.Upload -> do
        void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\n$ "))
        H.modify_ (_ { mainDisplay = UploadDisplay })
      Right CLI.EditorCanvas -> do
        void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\n$ "))
        H.modify_ (_ { mainDisplay = SplitDisplay })
        H.liftEffect canvasDimensionHack
      Right CLI.Canvas -> do
        void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\n$ "))
        H.modify_ (_ { mainDisplay = CanvasDisplay })
        H.liftEffect canvasDimensionHack
      Right CLI.Play -> playKlank
      Right CLI.Stop -> do
        _ <- H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\n$ ")
        stopper
      Right CLI.Compile -> compile
      Right CLI.LinkNoTerm -> makeLink true false
      Right CLI.Link -> makeLink false false
      Right CLI.FileLink -> makeLink false true
    pure unit
