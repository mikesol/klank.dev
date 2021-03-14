module App.App where

import Prelude
import Ace (Position(..))
import Ace.EditSession as EditSession
import Ace.Editor as Editor
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as AXRF
import App.AceComponent as AceComponent
import App.AppAction (Action(..))
import App.CLI as CLI
import App.CanvasComponent as CanvasComponent
import App.ClickPlayModal (clickPlay)
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
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String as S
import Data.String.Base64 (decode)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, try)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, throw)
import Effect.Now (now)
import Effect.Timer (IntervalId, clearInterval, setInterval)
import FRP.Behavior.Audio (AudioContext, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserMicrophone, BrowserPeriodicWave, MediaRecorder, RecorderSignature, audioWorkletAddModule, makeAudioContext)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as O
import Graphics.Canvas (CanvasElement)
import Halogen (ClassName(..), SubscriptionId)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Text.Parsing.Parser (runParser)
import Type.Klank.Dev (Klank'')
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.File.File (File)
import Web.HTML (HTMLCanvasElement, HTMLImageElement, HTMLVideoElement)

foreign import serverUrl :: Effect String

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

_ace = Proxy :: Proxy "ace"

_xterm = Proxy :: Proxy "xterm"

_canvas = Proxy :: Proxy "canvas"

_upload = Proxy :: Proxy "upload"

data MainDisplay
  = EditorDisplay
  | DownloadsDisplay
  | CanvasDisplay
  | SplitDisplay

data KlankError
  = NoDiceSafari
  | NoDiceIOS
  | NoDiceLinuxChrome

type State
  = { editorText :: String
    , isPlaying :: Maybe Boolean
    , downloadProgress :: Maybe Number
    , compiledKlank :: Maybe String
    , mainDisplay :: MainDisplay
    , stopFn :: Maybe (Effect Unit)
    , progressIntervalId :: Maybe IntervalId
    , audioCtx :: Maybe AudioContext
    , initialAccumulator :: Maybe Foreign
    , klankErrorCondition :: Maybe KlankError
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
    , images :: Object HTMLImageElement
    , videos :: Object HTMLVideoElement
    , canvases :: Object HTMLCanvasElement
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

type ChildSlots
  = ( ace :: AceComponent.Slot WhichAce
    , xterm :: XTermComponent.Slot WhichTerm
    , canvas :: CanvasComponent.Slot WhichCanvas
    )

affable :: forall a. ((a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit) -> Aff a
affable f =
  makeAff \cb -> do
    f (cb <<< Right) (cb <<< Left)
    pure mempty

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState:
        \_ ->
          { editorText: initialPS
          , isPlaying: Nothing
          , compiledKlank: Nothing
          , mainDisplay: EditorDisplay
          , stopFn: Nothing
          , progressIntervalId: Nothing
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
          , images: O.empty
          , videos: O.empty
          , canvases: O.empty
          , klankErrorCondition: Nothing
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

editorDisplay :: ∀ m. MonadAff m => String -> HH.HTML (H.ComponentSlot ChildSlots m Action) Action
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
    HandleAceUpdate

noDiceSafari :: ∀ w a. Array (HH.HTML w a)
noDiceSafari =
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

noDiceLinuxChrome :: ∀ w a. Array (HH.HTML w a)
noDiceLinuxChrome =
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
              , HH.p [] [ HH.text "Please open this link in ", HH.a [ HP.classes $ map ClassName [ "underline" ], HP.href "https://www.mozilla.org/en-US/firefox/new/" ] [ HH.text "Firefox" ], HH.text " to listen!" ]
              ]
          , HH.div [ HP.classes $ map ClassName [ "flex-grow" ] ] []
          ]
      , HH.div [ HP.classes $ map ClassName [ "flex-grow" ] ] []
      ]
  ]

noDiceiOS :: ∀ w a. Array (HH.HTML w a)
noDiceiOS =
  [ HH.div [ HP.classes $ map ClassName [ "h-full", "w-full", "flex", "flex-col" ] ]
      [ HH.div [ HP.classes $ map ClassName [ "flex-grow" ] ] []
      , HH.div [ HP.classes $ map ClassName [ "w-full", "flex", "flex-row", "flex-none" ] ]
          [ HH.div [ HP.classes $ map ClassName [ "flex-grow" ] ] []
          , HH.div [ HP.classes $ map ClassName [ "flex-none" ] ]
              [ HH.p
                  [ HP.classes $ map ClassName [ "text-2xl", "font-bold" ]
                  ]
                  [ HH.text "Drats!" ]
              , HH.p [] [ HH.text "klank.dev does not work on iOS." ]
              , HH.p [] [ HH.text "Please open this link from a computer or Android device to listen!" ]
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
, klankErrorCondition
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
          ( case klankErrorCondition of
              Just NoDiceSafari -> noDiceSafari
              Just NoDiceIOS -> noDiceiOS
              Just NoDiceLinuxChrome -> noDiceLinuxChrome
              Nothing ->
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
                                HandleTerminalUpdate
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
                              [ HH.i [ HP.classes $ map ClassName [ "fas", "fa-9x", "cursor-pointer", "z-40", (if ip then "fa-stop-circle" else "fa-play-circle") ], HE.onClick \_ -> if ip then PlayKlankFromStopButton else PlayKlankFromPlayButton ] []
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

foreign import data BrowserMediaStream :: Type

foreign import data BrowserCamera :: Type

foreign import getBrowserMediaStreamImpl :: Boolean -> Boolean -> Effect (Promise BrowserMediaStream)

foreign import cameraToVideo :: BrowserCamera -> Effect HTMLVideoElement

browserMediaStreamToBrowserMicrophone :: BrowserMediaStream -> BrowserMicrophone
browserMediaStreamToBrowserMicrophone = unsafeCoerce

browserMediaStreamToBrowserCamera :: BrowserMediaStream -> BrowserCamera
browserMediaStreamToBrowserCamera = unsafeCoerce

getMicrophoneAndCamera :: Boolean -> Boolean -> Aff { microphone :: Maybe BrowserMicrophone, camera :: Maybe BrowserCamera }
getMicrophoneAndCamera audio video =
  ( \i ->
      { microphone: if audio then Just $ browserMediaStreamToBrowserMicrophone i else Nothing
      , camera: if video then Just $ browserMediaStreamToBrowserCamera i else Nothing
      }
  )
    <$> toAffE (getBrowserMediaStreamImpl audio video)

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
  { emitter, listener } <- H.liftEffect HS.create
  void $ H.subscribe emitter
  startTime <- H.liftEffect (((_ / 1000.0) <<< unwrap <<< unInstant) <$> now)
  iid <-
    H.liftEffect
      $ setInterval 100 do
          currentTime <- ((_ / 1000.0) <<< unwrap <<< unInstant) <$> now
          let
            updatedProgress = bindBetween 0.0 95.0 $ calcSlope startTime 0.0 (startTime + totalDur) 95.0 currentTime
          HS.notify listener (ProgressUpdate updatedProgress)
  H.modify_ (_ { progressIntervalId = Just iid })

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
      klankErrorCondition =
        if force then
          Nothing
        else
          ( case os, browser of
              _, Just Safari -> Just NoDiceSafari
              Just Linux, Just Chrome -> Just NoDiceLinuxChrome
              Just IOS, _ -> Just NoDiceIOS
              _, _ -> Nothing
          )
    H.modify_
      ( _
          { klankErrorCondition = klankErrorCondition
          }
      )
    if (isJust klankErrorCondition) then
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
                    H.tell _ace Editor (AceComponent.ChangeText editorText)
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
              H.tell _ace Editor (AceComponent.ChangeText editorText)
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
              cacheHack
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
      sid <- H.gets _.progressIntervalId
      H.modify_ (_ { progressIntervalId = Nothing })
      maybe (pure unit) (H.liftEffect <<< clearInterval) sid
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
    H.tell _xterm Terminal (XTermComponent.ChangeText $ "\r\nPlaying\r\n$ ")
  PlayStartFailed s -> H.tell _xterm Terminal (XTermComponent.ChangeText $ "\r\nPlaying failed. " <> s <> " Please try agian later.\r\n$ ")
  RecordingRegistered k v -> H.modify_ (\i -> i { downloadLinks = i.downloadLinks <> [ Tuple k v ] })
  HandleAceUpdate msg -> handleAceOuput msg
  HandleTerminalUpdate msg -> handleTerminalOutput msg

handleAceOuput :: forall o m. MonadAff m => AceComponent.Output -> H.HalogenM State Action ChildSlots o m Unit
handleAceOuput = case _ of
  AceComponent.TextChanged editorText -> H.modify_ (_ { editorText = editorText })

stopper :: ∀ t32 t33 t34 t35 t42. MonadEffect t32 => H.HalogenM { audioCtx ∷ Maybe AudioContext, playerSubscriptionId ∷ Maybe SubscriptionId, stopFn ∷ Maybe (Effect Unit) | t42 } t35 t34 t33 t32 Unit
stopper = do
  sfn <- H.gets _.stopFn
  ctx <- H.gets _.audioCtx
  H.modify_ (_ { stopFn = Nothing, audioCtx = Nothing, playerSubscriptionId = Nothing })
  maybe (pure unit) H.liftEffect sfn
  maybe (pure unit) (H.liftEffect <<< stopAudioContext) ctx

cacheHack :: ∀ t560 t567. Bind t560 ⇒ MonadState { buffers :: Object BrowserAudioBuffer, images :: Object HTMLImageElement, videos :: Object HTMLVideoElement, canvases :: Object HTMLCanvasElement | t567 } t560 ⇒ MonadEffect t560 ⇒ MonadAff t560 ⇒ t560 Unit
cacheHack = do
  prevBuffers <- H.gets _.buffers
  prevImages <- H.gets _.images
  prevVideos <- H.gets _.videos
  prevCanvases <- H.gets _.canvases
  klank <- H.liftEffect getKlank
  ctx <- H.liftEffect makeAudioContext
  buffers <- H.liftAff (affable $ klank.buffers ctx prevBuffers)
  images <- H.liftAff (affable $ klank.images prevImages)
  videos <- H.liftAff (affable $ klank.videos prevVideos)
  canvases <- H.liftAff (affable $ klank.canvases prevCanvases)
  H.modify_ (_ { buffers = buffers, images = images, videos = videos, canvases = canvases })

compile :: ∀ t119 t123 t124 t125 t140 t293. MonadEffect t123 => MonadAff t123 => H.HalogenM State t125 ( ace ∷ H.Slot AceComponent.Query t119 WhichAce, xterm ∷ H.Slot XTermComponent.Query t140 WhichTerm | t293 ) t124 t123 Unit
compile = do
  url <- H.liftEffect serverUrl
  -- in case we are in compile mode, we use editorText
  txt_ <-
    (H.request _ace Editor AceComponent.GetText)
      >>= (maybe (Just <$> H.gets _.editorText) pure)
  maybe
    ( do
        H.tell
          _xterm
          Terminal
          (XTermComponent.ChangeText $ "\r\nWell, this is embarassing. We can't access the DOM, so this website won't work. Please file a bug request!\r\n$ ")
        pure unit
    )
    ( \code -> do
        H.tell
          _xterm
          Terminal
          (XTermComponent.ChangeText $ "\r\nCompiling. Please be patient.\r\nThis should take 2-8 seconds depending on your internet connection speed.")
        response <-
          H.liftAff
            $ AX.post AXRF.json (url <> "compile") (Just (RequestBody.json $ encodeJson { code }))
        either
          ( const do
              H.tell
                _xterm
                Terminal
                (XTermComponent.ChangeText $ "\r\nThe klank server is down or overloaded. Try again, and if your request doesn't work a second time, please file a bug. Sorry!\r\n$ ")
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
                              H.tell
                                _xterm
                                Terminal
                                (XTermComponent.ChangeText $ "\r\nSorry, your code did not compile. Here is the error message:\r\n" <> either (const "Unidentified error") (replaceAll (Pattern "\n") (Replacement "\r\n")) error_ <> "\r\n$ ")
                              pure unit
                          )
                      )
                      ( \res -> do
                          -- H.liftEffect $ log res
                          H.modify_ (_ { compiledKlank = Just res })
                          H.liftEffect $ completelyUnsafeEval res
                          -- fill the buffer cache on compile
                          cacheHack
                          H.tell
                            _xterm
                            Terminal
                            (XTermComponent.ChangeText $ "\r\nSuccess! Your code is compiled.\r\nType p then ENTER to play, s then ENTER to stop.\r\n$ ")
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
  H.tell _xterm Terminal (XTermComponent.ChangeText $ "\r\nRetrieving assets...")
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
  prevImages <- H.gets _.images
  prevVideos <- H.gets _.videos
  prevCanvases <- H.gets _.canvases
  -- steps
  -- 1. refactor all of the aff stuff below to subscribe function
  -- body of this subscription
  -- note that the subscription won't have a finalizer for now
  -- may lead to wanky memory, we can look into that...
  -- 2. create actions for everything below.
  { emitter, listener } <- H.liftEffect HS.create
  subId <- H.subscribe emitter
  H.liftAff do
    res <-
      try do
        { microphone, camera } <- if klank.enableMicrophone || klank.enableCamera then getMicrophoneAndCamera klank.enableMicrophone klank.enableCamera else pure { microphone: Nothing, camera: Nothing }
        let
          microphones = maybe O.empty (O.singleton "microphone") microphone
        cameraAsVideo <- case camera of
          Nothing -> pure Nothing
          Just c -> Just <$> H.liftEffect (cameraToVideo c)
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
        images <- affable $ klank.images prevImages
        videos <- affable $ klank.videos prevVideos
        sourceCanvases <- affable $ klank.canvases prevCanvases
        recorders <-
          affable
            $ klank.recorders
                O.empty
                ( \k v -> HS.notify listener (RecordingRegistered k v)
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
                { canvases: O.singleton "canvas" canvasOrBust
                , images: images
                , videos: videos
                , cameras:
                    case cameraAsVideo of
                      Nothing -> O.empty
                      Just c -> O.singleton "camera" { camera: c, cache: klank.webcamCache }
                , sourceCanvases: sourceCanvases
                }
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
    H.liftEffect
      $ case res of
          Left err -> HS.notify listener (PlayStartFailed (show err))
          Right resp -> H.liftEffect $ HS.notify listener (PlayStartSucceeded resp)
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
        H.tell
          _xterm
          Terminal
          (XTermComponent.ChangeText $ "\r\nThe link shortening server is either down or overloaded. Try again, and if your request doesn't work a second time, please file a bug. Sorry!\r\n$ ")
        pure unit
        H.liftEffect $ throw "Could not process sound."
    )
    (pure <<< _.body)
    _uploadLink'

-- todo - avoid Firebase call for extra link
makeLink :: ∀ m t723 t724. MonadEffect m => MonadAff m => Boolean -> Boolean -> LinkType -> H.HalogenM State t724 ChildSlots t723 m Unit
makeLink noTerm justLink linkType = do
  txt_ <- H.request _ace Editor AceComponent.GetText
  maybe
    ( do
        H.tell
          _xterm
          Terminal
          (XTermComponent.ChangeText $ "\r\nWell, this is embarassing. We can't access the DOM, so this website won't work. Please file a bug request!\r\n$ ")
        pure unit
    )
    ( \code' -> do
        H.tell
          _xterm
          Terminal
          (XTermComponent.ChangeText $ "\r\nGenerating a link...")
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
        let
          link =
            "https://klank.dev/?" <> (if noTerm then "noterm" else "k") <> "&url="
              <> _codeUploadLink
              <> ( case _compiledUploadLink of
                    Nothing -> mempty
                    Just x -> "&klank=" <> x
                )
        H.modify_
          ( _
              { linkModalUrl = if justLink then _codeUploadLink else link
              , linkModalOpen = true
              , linkModalProperNoun = if justLink then "file" else "klank"
              }
          )
        H.tell
          _xterm
          Terminal
          (XTermComponent.ChangeText $ "\r\n$ ")
        pure unit
    )
    txt_

handleTerminalOutput :: forall o m. MonadAff m => XTermComponent.Output -> H.HalogenM State Action ChildSlots o m Unit
handleTerminalOutput = case _ of
  XTermComponent.TextChanged tt -> do
    let
      parserRes = runParser tt CLI.cli
    case parserRes of
      Left _ -> H.tell _xterm Terminal (XTermComponent.ChangeText $ "\r\nSorry, I didn't understand \"" <> tt <> "\"\r\nPlease type h and ENTER to list commands\r\n$ ")
      Right CLI.Help -> H.tell _xterm Terminal (XTermComponent.ChangeText $ ("\r\n" <> helpMsg <> "\r\n$ "))
      Right CLI.Editor -> do
        H.tell _xterm Terminal (XTermComponent.ChangeText $ "\r\n$ ")
        H.modify_ (_ { mainDisplay = EditorDisplay })
      Right CLI.Downloads -> do
        H.tell _xterm Terminal (XTermComponent.ChangeText $ "\r\n$ ")
        H.modify_ (_ { mainDisplay = DownloadsDisplay })
      Right CLI.EditorCanvas -> do
        H.tell _xterm Terminal (XTermComponent.ChangeText $ "\r\n$ ")
        H.modify_ (_ { mainDisplay = SplitDisplay })
        H.liftEffect canvasDimensionHack
      Right CLI.Canvas -> do
        H.tell _xterm Terminal (XTermComponent.ChangeText $ "\r\n$ ")
        H.modify_ (_ { mainDisplay = CanvasDisplay })
        H.liftEffect canvasDimensionHack
      Right CLI.Play -> playKlank
      Right CLI.Stop -> do
        H.tell _xterm Terminal (XTermComponent.ChangeText $ "\r\n$ ")
        stopper
        H.modify_ (_ { isPlaying = Just false })
      Right CLI.Compile -> compile
      Right CLI.LinkNoTerm -> makeLink true false FromEditor
      Right CLI.LinkCompiledKlank -> makeLink false false FromCompiledKlank
      Right CLI.Link -> makeLink false false FromEditor
      Right CLI.FileLink -> makeLink false true FromEditor
    pure unit
