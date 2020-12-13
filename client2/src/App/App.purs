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
import Data.Argonaut (class EncodeJson, getField, toObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as A
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String as S
import Data.String.Base64 (decode)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, try)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, throw)
import Effect.Now (now)
import Effect.Timer (clearInterval, setInterval)
import FRP.Behavior.Audio (AudioContext, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, MediaRecorder, RecorderSignature, audioWorkletAddModule, makeAudioContext)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as O
import Graphics.Canvas (CanvasElement)
import Halogen (ClassName(..), SubscriptionId)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (Finalizer(..))
import Halogen.Query.EventSource as ES
import Text.Parsing.Parser (runParser)
import Type.Klank.Dev (Klank'')
import Web.File.File (File)
import Web.File.File as WF

foreign import data BrowserMicrophone :: Type

foreign import serverUrl :: Effect String

foreign import firebaseUrl :: Effect String

foreign import firebaseToken :: Effect String

foreign import getK :: Effect Boolean

foreign import getForce :: Effect Boolean

foreign import getC :: Effect Boolean

foreign import getEC :: Effect Boolean

foreign import getNoterm :: Effect Boolean

foreign import getNostop :: Effect Boolean

foreign import getB64 :: Maybe String -> (String -> Maybe String) -> Effect (Maybe String)

foreign import getInitialAccumulator :: Maybe Foreign -> (Foreign -> Maybe Foreign) -> Effect (Maybe Foreign)

foreign import getUrl :: Maybe String -> (String -> Maybe String) -> Effect (Maybe String)

foreign import getKlankUrl :: Maybe String -> (String -> Maybe String) -> Effect (Maybe String)

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
  | DownloadsDisplay
  | CanvasDisplay
  | SplitDisplay
  | UploadDisplay

type State
  = { editorText :: String
    , isPlaying :: Maybe Boolean
    , downloadProgress :: Maybe Number
    , compiledKlank :: Maybe String
    , mainDisplay :: MainDisplay
    , stopFn :: Maybe (Effect Unit)
    , progressSubscriptionId :: Maybe SubscriptionId
    , audioCtx :: Maybe AudioContext
    , initialAccumulator :: Maybe Foreign
    , klankShouldWork :: Boolean
    , worklets :: Array String
    , linkModalUrl :: String
    , loadingModalOpen :: Boolean
    , linkModalOpen :: Boolean
    , linkModalProperNoun :: String
    , playModalOpen :: Boolean
    , showTerminal :: Boolean
    , noStop :: Boolean
    , tracks :: Object BrowserAudioTrack
    , recorders :: Object (RecorderSignature MediaRecorder)
    , downloadLinks :: Array (Tuple String String)
    , buffers :: Object BrowserAudioBuffer
    , floatArrays :: Object BrowserFloatArray
    , periodicWaves :: Object BrowserPeriodicWave
    , playerSubscriptionId :: Maybe SubscriptionId
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
          , isPlaying: Nothing
          , compiledKlank: Nothing
          , mainDisplay: EditorDisplay
          , stopFn: Nothing
          , progressSubscriptionId: Nothing
          , audioCtx: Nothing
          , downloadProgress: Nothing
          , loadingModalOpen: true
          , linkModalOpen: false
          , showTerminal: true
          , noStop: false
          , linkModalUrl: ""
          , linkModalProperNoun: "klank"
          , playModalOpen: false
          , initialAccumulator: Nothing
          , worklets: []
          , tracks: O.empty
          , buffers: O.empty
          , recorders: O.empty
          , downloadLinks: []
          , floatArrays: O.empty
          , periodicWaves: O.empty
          , klankShouldWork: true
          , playerSubscriptionId: Nothing
          }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

editorDisplay :: ∀ m. MonadAff m => String -> HH.HTML (H.ComponentSlot HH.HTML ChildSlots m Action) Action
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

noDice :: ∀ t119 t120. Array (HH.HTML t120 t119)
noDice =
  [ HH.div [ HP.classes $ map ClassName [ "h-full", "w-full", "flex", "flex-col" ] ]
      [ HH.div [ HP.classes $ map ClassName [ "flex-grow" ] ] []
      , HH.div [ HP.classes $ map ClassName [ "w-full", "flex", "flex-row", "flex-none" ] ]
          [ HH.div [ HP.classes $ map ClassName [ "flex-grow" ] ] []
          , HH.div [ HP.classes $ map ClassName [ "flex-none" ] ]
              [ HH.p
                  [ HP.classes $ map ClassName [ "text-2xl", "font-bold" ]
                  ]
                  [ HH.text "Drats!" ]
              , HH.p [] [ HH.text "klank.dev does not work on this browser." ]
              , HH.p [] [ HH.text "Please open this link in ", HH.a [ HP.classes $ map ClassName [ "underline" ], HP.href "https://www.mozilla.org/en-US/firefox/new/" ] [ HH.text "Firefox" ], HH.text " or ", HH.a [ HP.classes $ map ClassName [ "underline" ], HP.href "https://www.google.com/chrome/" ] [ HH.text "Chrome" ], HH.text " to listen!" ]
              ]
          , HH.div [ HP.classes $ map ClassName [ "flex-grow" ] ] []
          ]
      , HH.div [ HP.classes $ map ClassName [ "flex-grow" ] ] []
      ]
  ]

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render { editorText
, mainDisplay
, linkModalOpen
, linkModalUrl
, klankShouldWork
, linkModalProperNoun
, loadingModalOpen
, playModalOpen
, showTerminal
, isPlaying
, downloadLinks
, downloadProgress
, noStop
} =
  HH.div [ HP.classes $ map ClassName [ "h-screen", "w-screen" ] ]
    ( [ HH.div
          [ HP.classes $ map ClassName [ "h-full", "w-full", "flex", "flex-col" ] ]
          ( if (not klankShouldWork) then
              noDice
            else
              ( join
                  [ [ HH.div [ HP.classes $ map ClassName [ "flex", "flex-grow" ] ] case mainDisplay of
                        EditorDisplay -> [ editorDisplay editorText ]
                        DownloadsDisplay ->
                          [ HH.div
                              [ HP.classes
                                  $ map ClassName [ "h-full", "w-full" ]
                              ]
                              ( [ HH.p
                                    [ HP.classes $ map ClassName [ "text-2xl", "font-bold" ]
                                    ]
                                    [ HH.text "Download links" ]
                                , HH.p [] [ HH.text "Below you can find links to downloadable files from the current session" ]
                                , HH.ul [] (map (\(Tuple name url) -> HH.li [] [ HH.a [ HP.href url, HP.download name ] [ HH.text name ] ]) downloadLinks)
                                ]
                              )
                          ]
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
                          [ HH.div [ HP.classes $ map ClassName [ "h-full", "flex-grow", "w-full", "grid", "grid-cols-2", "grid-rows-1", "gap-0" ] ]
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
          )
      , modal
          { url: linkModalUrl, open: linkModalOpen, properNoun: linkModalProperNoun
          }
      , clickPlay
          { open: playModalOpen
          }
      , loading
          { open: loadingModalOpen
          , progressSoFar: fromMaybe 0.0 downloadProgress
          , progressMax: 100.0
          }
      ]
        <> ( if (not showTerminal && not loadingModalOpen && not playModalOpen && not noStop) then
              ( maybe []
                  ( \ip ->
                      [ HH.div [ HP.classes $ map ClassName [ "modal", "fixed", "pr-8", "pb-8", "right-0", "bottom-0" ] ]
                          [ HH.div [ HP.classes $ map ClassName [ "bg-white", "rounded-full" ] ]
                              [ HH.i [ HP.classes $ map ClassName [ "fas", "fa-9x", "cursor-pointer", "z-40", (if ip then "fa-stop-circle" else "fa-play-circle") ], HE.onClick \_ -> Just (if ip then PlayKlankFromStopButton else PlayKlankFromPlayButton) ] []
                              ]
                          ]
                      ]
                  )
                  isPlaying
              )
            else
              []
          )
    )

foreign import stopAudioContext :: AudioContext -> Effect Unit

data KlankBrowser
  = Opera
  | Firefox
  | Safari
  | IE
  | Edge
  | Chrome
  | EdgeChromium
  | Blink

data KlankOS
  = MacOS
  | IOS
  | Windows
  | Android
  | Linux

derive instance eqKlankOS :: Eq KlankOS

foreign import getOS_ :: Maybe KlankOS -> (KlankOS -> Maybe KlankOS) -> KlankOS -> KlankOS -> KlankOS -> KlankOS -> KlankOS -> Effect (Maybe KlankOS)

getOS :: Effect (Maybe KlankOS)
getOS = getOS_ Nothing Just MacOS IOS Windows Android Linux

foreign import getBrowser_ :: Maybe KlankBrowser -> (KlankBrowser -> Maybe KlankBrowser) -> KlankBrowser -> KlankBrowser -> KlankBrowser -> KlankBrowser -> KlankBrowser -> KlankBrowser -> KlankBrowser -> KlankBrowser -> Effect (Maybe KlankBrowser)

getBrowser :: Effect (Maybe KlankBrowser)
getBrowser = getBrowser_ Nothing Just Opera Firefox Safari IE Edge Chrome EdgeChromium Blink

foreign import loadCustomAudioNodes :: AudioContext -> Effect (Promise Unit)

foreign import getMicrophoneImpl :: Effect (Promise BrowserMicrophone)

getMicrophone :: Aff BrowserMicrophone
getMicrophone = toAffE getMicrophoneImpl

simplGetr :: forall m. Bind m => MonadAff m => String -> m String
simplGetr txt = do
  result <-
    H.liftAff
      $ AX.request
          ( AX.defaultRequest
              { url = txt
              , method = Left GET
              , responseFormat = AXRF.string
              }
          )
  opt <-
    either (\_ -> H.liftEffect $ throw "Could not retrieve")
      (pure <<< _.body)
      result
  pure opt

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b

bindBetween :: Number -> Number -> Number -> Number
bindBetween mn mx n = max mn (min mx n)

triggerProgressLoader :: forall o m. MonadAff m => Int -> Boolean -> H.HalogenM State Action ChildSlots o m Unit
triggerProgressLoader nLines isCompiled = do
  os <- H.liftEffect getOS
  let
    totalDur =
      ( case os of
          Nothing -> 5.0
          Just MacOS -> 10.0
          Just Android -> 10.0
          Just _ -> 5.0
      )
        * (toNumber nLines)
        / (if isCompiled then 2000.0 else 1000.0)
  subId <-
    H.subscribe
      $ ES.effectEventSource \emitter -> do
          startTime <- ((_ / 1000.0) <<< unwrap <<< unInstant) <$> now
          iid <-
            H.liftEffect
              $ setInterval 100 do
                  currentTime <- ((_ / 1000.0) <<< unwrap <<< unInstant) <$> now
                  let
                    updatedProgress = bindBetween 0.0 95.0 $ calcSlope startTime 0.0 (startTime + totalDur) 95.0 currentTime
                  ES.emit emitter (ProgressUpdate updatedProgress)
          pure $ Finalizer (H.liftEffect $ clearInterval iid)
  H.modify_ (_ { progressSubscriptionId = Just subId })

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  PlayKlankFromModal -> do
    H.modify_ (_ { playModalOpen = false })
    playKlank
  PlayKlankFromPlayButton -> do
    playKlank
  ProgressUpdate n -> do
    H.modify_ (_ { downloadProgress = Just n })
  PlayKlankFromStopButton -> do
    stopper
    H.modify_ (_ { isPlaying = Just false })
  CloseLinkModal -> do
    H.modify_ (_ { linkModalOpen = false })
  Initialize -> do
    browser <- H.liftEffect getBrowser
    os <- H.liftEffect getOS
    force <- H.liftEffect $ getForce
    let
      klankShouldWork =
        force
          || ( case os, browser of
                _, Just Safari -> false
                Just Linux, Just Chrome -> false
                _, _ -> true
            )
    H.modify_
      ( _
          { klankShouldWork = klankShouldWork
          }
      )
    if (not klankShouldWork) then
      H.modify_ (_ { loadingModalOpen = false })
    else do
      b64 <- H.liftEffect $ getB64 Nothing Just
      url <- H.liftEffect $ getUrl Nothing Just
      klankUrl <- H.liftEffect $ getKlankUrl Nothing Just
      k <- H.liftEffect $ getK
      c <- H.liftEffect $ getC
      ec <- H.liftEffect $ getEC
      noterm <- H.liftEffect $ getNoterm
      nostop <- H.liftEffect $ getNostop
      when noterm
        ( do
            H.modify_
              ( _
                  { showTerminal = false
                  , noStop = nostop
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
              editorText <- simplGetr txt
              H.modify_ (_ { editorText = editorText })
              _ <- H.query _ace Editor $ H.tell (AceComponent.ChangeText editorText)
              triggerProgressLoader (A.length (S.split (S.Pattern "\n") editorText)) (isJust klankUrl)
              pure unit
          )
      case klankUrl of
        Nothing -> pure unit
        Just txt ->
          ( do
              compiledKlank <- simplGetr txt
              H.modify_ (_ { compiledKlank = Just compiledKlank })
              H.liftEffect $ completelyUnsafeEval compiledKlank
              bufferCacheHack
              pure unit
          )
      case (k || (noterm && isNothing klankUrl)) of
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
      sid <- H.gets _.progressSubscriptionId
      H.modify_ (_ { progressSubscriptionId = Nothing })
      maybe (pure unit) H.unsubscribe sid
      pure mempty
  PlayStartSucceeded playerInfo -> do
    H.modify_
      ( _
          { stopFn = playerInfo.stopFn
          , isPlaying = playerInfo.isPlaying
          , periodicWaves = playerInfo.periodicWaves
          , audioCtx = playerInfo.audioCtx
          , recorders = playerInfo.recorders
          , worklets = playerInfo.worklets
          , tracks = playerInfo.tracks
          , buffers = playerInfo.buffers
          , floatArrays = playerInfo.floatArrays
          }
      )
    void $ H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\nPlaying\r\n$ ")
  PlayStartFailed s -> void $ H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\nPlaying failed. " <> s <> " Please try agian later.\r\n$ ")
  RecordingRegistered k v -> H.modify_ (\i -> i { downloadLinks = i.downloadLinks <> [ Tuple k v ] })
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

stopper :: ∀ t32 t33 t34 t35 t42. MonadEffect t32 => H.HalogenM { audioCtx ∷ Maybe AudioContext, playerSubscriptionId ∷ Maybe SubscriptionId, stopFn ∷ Maybe (Effect Unit) | t42 } t35 t34 t33 t32 Unit
stopper = do
  sfn <- H.gets _.stopFn
  ctx <- H.gets _.audioCtx
  H.modify_ (_ { stopFn = Nothing, audioCtx = Nothing, playerSubscriptionId = Nothing })
  maybe (pure unit) H.liftEffect sfn
  maybe (pure unit) (H.liftEffect <<< stopAudioContext) ctx

bufferCacheHack :: ∀ t168 t175. Bind t168 ⇒ MonadState { buffers :: Object BrowserAudioBuffer | t175 } t168 ⇒ MonadEffect t168 ⇒ MonadAff t168 ⇒ t168 Unit
bufferCacheHack = do
  prevBuffers <- H.gets _.buffers
  klank <- H.liftEffect getKlank
  ctx <- H.liftEffect makeAudioContext
  buffers <- H.liftAff (affable $ klank.buffers ctx prevBuffers)
  H.modify_ (_ { buffers = buffers })

compile :: ∀ t119 t123 t124 t125 t140 t293. MonadEffect t123 => MonadAff t123 => H.HalogenM State t125 ( ace ∷ H.Slot AceComponent.Query t119 WhichAce, xterm ∷ H.Slot XTermComponent.Query t140 WhichTerm | t293 ) t124 t123 Unit
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
                          H.modify_ (_ { compiledKlank = Just res })
                          H.liftEffect $ completelyUnsafeEval res
                          -- fill the buffer cache on compile
                          bufferCacheHack
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

playKlank :: forall m o. MonadEffect m => MonadAff m => H.HalogenM State Action ChildSlots o m Unit
playKlank = do
  stopper
  oldSubId <- H.gets _.playerSubscriptionId
  maybe (pure unit) H.unsubscribe oldSubId
  _ <- H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\nRetrieving assets...")
  klank <- H.liftEffect getKlank
  ctx <- H.liftEffect makeAudioContext
  H.liftAff (toAffE $ loadCustomAudioNodes ctx)
  initialAccumulator <- H.gets _.initialAccumulator
  prevWorklets <- H.gets _.worklets
  prevTracks <- H.gets _.tracks
  prevRecorders <- H.gets _.recorders
  prevBuffers <- H.gets _.buffers
  prevFloatArrays <- H.gets _.floatArrays
  prevPeriodicWaves <- H.gets _.periodicWaves
  -- steps
  -- 1. refactor all of the aff stuff below to subscribe function
  -- body of this subscription
  -- note that the subscription won't have a finalizer for now
  -- may lead to wanky memory, we can look into that...
  -- 2. create actions for everything below.
  subId <-
    H.subscribe
      $ ES.affEventSource \emitter -> do
          res <-
            try do
              microphones <-
                if klank.enableMicrophone then
                  ( do
                      mic <- getMicrophone
                      pure $ O.singleton "microphone" mic
                  )
                else
                  pure O.empty
              accumulator <- case initialAccumulator of
                Nothing -> (affable $ klank.accumulator)
                Just acc -> pure acc
              worklets <- (affable $ klank.worklets prevWorklets)
              -------------
              ----- maybe it's just superstition
              ---- but i think this didn't work unless I explicitly asked for a variable `o`
              --- instead of _ <-
              --------- weird...
              o <- traverse (toAffE <<< audioWorkletAddModule ctx) worklets
              tracks <- affable $ klank.tracks prevTracks
              buffers <- affable $ klank.buffers ctx prevBuffers
              recorders <-
                affable
                  $ klank.recorders
                      O.empty
                      ( \k v -> launchAff_ $ ES.emit emitter (RecordingRegistered k v)
                      )
                      prevRecorders
              floatArrays <- affable $ klank.floatArrays prevFloatArrays
              periodicWaves <- affable $ klank.periodicWaves ctx prevPeriodicWaves
              engineInfo <- affable $ klank.engineInfo
              turnMeOff <-
                H.liftEffect
                  ( klank.run
                      accumulator
                      ctx
                      engineInfo
                      { microphones, recorders, tracks, buffers, floatArrays, periodicWaves }
                      { canvases: O.singleton "canvas" canvasOrBust }
                      klank.exporter
                  )
              pure
                { stopFn: Just turnMeOff
                , isPlaying: Just true
                , periodicWaves: periodicWaves
                , audioCtx: Just ctx
                , recorders: recorders
                , worklets: worklets
                , tracks: tracks
                , buffers: buffers
                , floatArrays: floatArrays
                }
          case res of
            Left err -> ES.emit emitter (PlayStartFailed (show err))
            Right resp -> ES.emit emitter (PlayStartSucceeded resp)
          pure mempty -- todo: make a better finalizer?
  H.modify_ (_ { playerSubscriptionId = Just subId })
  pure unit

data LinkType
  = FromEditor
  | FromCompiledKlank

makeUploadLink :: ∀ t167 t210 t211 t212 t213. MonadEffect t210 => MonadAff t210 => EncodeJson t167 => LinkType -> t167 -> H.HalogenM t213 t212 ChildSlots t211 t210 String
makeUploadLink linkType code = do
  surl <- H.liftEffect serverUrl
  _uploadLink' <-
    H.liftAff
      $ AX.request
          ( AX.defaultRequest
              { headers = []
              , method = Left POST
              , url =
                ( surl
                    <> ( case linkType of
                          FromEditor -> "u"
                          FromCompiledKlank -> "uk"
                      )
                )
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

-- todo - avoid Firebase call for extra link
makeLink :: ∀ m t723 t724. MonadEffect m => MonadAff m => Boolean -> Boolean -> LinkType -> H.HalogenM State t724 ChildSlots t723 m Unit
makeLink noTerm justLink linkType = do
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
        _ <-
          H.query
            _xterm
            Terminal
            $ H.tell (XTermComponent.ChangeText $ "\r\nGenerating a link...")
        code <-
          maybe (H.liftEffect $ throw "Could not retrieve the code") pure code'
        _codeUploadLink <- makeUploadLink FromEditor code
        compiled' <- H.gets _.compiledKlank
        _compiledUploadLink <-
          ( case linkType of
              FromEditor -> pure Nothing
              FromCompiledKlank -> do
                compiled <-
                  maybe (H.liftEffect $ throw "Could not retrieve the code") pure compiled'
                Just <$> makeUploadLink FromCompiledKlank compiled
          )
        ----
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
                                      , link:
                                          "https://klank.dev/?" <> (if noTerm then "noterm" else "k") <> "&url="
                                            <> _codeUploadLink
                                            <> ( case _compiledUploadLink of
                                                  Nothing -> mempty
                                                  Just x -> "&klank=" <> x
                                              )
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
                                { linkModalUrl = if justLink then _codeUploadLink else res
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
      Right CLI.Downloads -> do
        void (H.query _xterm Terminal $ H.tell (XTermComponent.ChangeText $ "\r\n$ "))
        H.modify_ (_ { mainDisplay = DownloadsDisplay })
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
        H.modify_ (_ { isPlaying = Just false })
      Right CLI.Compile -> compile
      Right CLI.LinkNoTerm -> makeLink true false FromEditor
      Right CLI.LinkCompiledKlank -> makeLink false false FromCompiledKlank
      Right CLI.Link -> makeLink false false FromEditor
      Right CLI.FileLink -> makeLink false true FromEditor
    pure unit
