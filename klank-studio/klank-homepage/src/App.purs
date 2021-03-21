module Klank.Dev.Home.App where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String as Str
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Klank.Studio.Atari as Atari
import Klank.Studio.BWV846 as BWV846
import Klank.Studio.E2020 as E2020
import Klank.Studio.HelloWorld as HelloWorld
import Klank.Studio.SilentNight as SilentNight
import Klank.Studio.Waves as Waves
import Klank.Weblib.Studio as Studio
import Routing.PushState (PushStateInterface, makeInterface, paths)
import Type.Proxy (Proxy(..))

_Klank = Proxy :: Proxy "Klank"

type State
  = { whereAmI :: WhereAmI
    , pushState :: Maybe PushStateInterface
    , unsubscribePusher :: Maybe (Effect Unit)
    }

data Action
  = Initialize
  | Finalize
  | Navigate WhereAmI

data WhichKlank
  = HelloWorld
  | Waves
  | Atari
  | BWV846
  | E2020
  | SilentNight

data WhereAmI
  = Home
  | InKlank WhichKlank

derive instance genericWhichKlank :: Generic WhichKlank _

derive instance eqWhichKlank :: Eq WhichKlank

instance showWhichKlank :: Show WhichKlank where
  show = genericShow

instance ordWhichKlank :: Ord WhichKlank where
  compare a b = compare (show a) (show b)

type Slot
  = H.Slot Query Output

data Query a
  = Void

type Output
  = Void

type ChildSlots
  = ( _helloWorld :: Slot WhichKlank
    , _waves :: Slot WhichKlank
    , _atari :: Slot WhichKlank
    , _bwv846 :: Slot WhichKlank
    , _e2020 :: Slot WhichKlank
    , _silentNight :: Slot WhichKlank
    )

toHash :: WhereAmI -> String
toHash Home = ""

toHash (InKlank klank) = case klank of
  HelloWorld -> "hello-world"
  Waves -> "waves"
  Atari -> "atari"
  BWV846 -> "bwv846"
  E2020 -> "e2020"
  SilentNight -> "silent-night"

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState:
        \_ ->
          { whereAmI: Home, pushState: Nothing, unsubscribePusher: Nothing
          }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

parProps = [ HP.classes $ map ClassName [ "sm:text-3xl", "md:text-2xl", "lg:text-xl" ] ]

makeLi :: ∀ (w ∷ Type). String → String → WhichKlank → HH.HTML w Action
makeLi name ghName whichKlank =
  HH.li parProps
    [ HH.text $ name <> " | "
    , HH.a [ HP.href $ "/#/" <> toHash (InKlank whichKlank) ] [ HH.text "play me" ]
    , HH.text " | "
    , HH.a [ HP.href $ "https://github.com/mikesol/klank.dev/tree/klank/klank-studio/" <> ghName ] [ HH.text "view on GitHub" ]
    ]

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render { whereAmI } =
  HH.div [ HP.classes $ map ClassName [ "h-screen", "w-screen" ] ] case whereAmI of
    Home ->
      [ HH.p
          [ HP.classes $ map ClassName [ "sm:text-5xl", "md:text-4xl", "lg:text-3xl", "font-bold" ]
          ]
          [ HH.text "Welcome to klank.dev!" ]
      , HH.p parProps
          [ HH.text "klank.dev is a browser-based media creation studio. It allows for the creation of interactive audio and visual works.  The project is free and open-source and is hosted on "
          , HH.a [ HP.href "https://github.com/mikesol/klank.dev" ] [ HH.text "GitHub" ]
          , HH.text ". Documentation for getting started is also available on the GitHub repo."
          ]
      , HH.p parProps
          [ HH.text "Below are links to some exampes created using klank.dev. All of the examples are available on the git repo as well."
          ]
      , HH.ul_
          [ makeLi "hello world" "01-hello-world" HelloWorld
          , makeLi "waves" "02-waves" Waves
          , makeLi "atari" "03-atari" Atari
          , makeLi "bwv846" "04-bwv846" BWV846
          , makeLi "e2020" "05-e2020" E2020
          , makeLi "silent-night" "06-silent-night" SilentNight
          ]
      ]
    InKlank klank -> case klank of
      HelloWorld ->
        [ HH.slot
            (Proxy :: _ "_helloWorld")
            HelloWorld
            (Studio.component (pure HelloWorld.playMe))
            {}
            absurd
        ]
      Waves ->
        [ HH.slot
            (Proxy :: _ "_waves")
            Waves
            (Studio.component (pure Waves.playMe))
            {}
            absurd
        ]
      Atari ->
        [ HH.slot
            (Proxy :: _ "_atari")
            Atari
            (Studio.component (pure Atari.playMe))
            {}
            absurd
        ]
      BWV846 ->
        [ HH.slot
            (Proxy :: _ "_bwv846")
            BWV846
            (Studio.component (pure BWV846.playMe))
            {}
            absurd
        ]
      E2020 ->
        [ HH.slot
            (Proxy :: _ "_e2020")
            E2020
            (Studio.component (pure E2020.playMe))
            {}
            absurd
        ]
      SilentNight ->
        [ HH.slot
            (Proxy :: _ "_silentNight")
            SilentNight
            (Studio.component (pure SilentNight.playMe))
            {}
            absurd
        ]

routeToPlace :: String -> WhereAmI
routeToPlace s = case canonical of
  "hello-world" -> InKlank HelloWorld
  "waves" -> InKlank Waves
  "atari" -> InKlank Atari
  "bwv846" -> InKlank BWV846
  "e2020" -> InKlank E2020
  "silent-night" -> InKlank SilentNight
  _ -> Home
  where
  canonical
    | Str.take 3 s == "/#/" = Str.drop 3 s
    | Str.take 2 s == "#/" = Str.drop 2 s
    | Str.take 2 s == "/#" = Str.drop 2 s
    | Str.take 1 s == "#" = Str.drop 1 s
    | Str.take 1 s == "/" = Str.drop 1 s
    | otherwise = s

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Initialize -> do
    { emitter, listener } <- H.liftEffect HS.create
    interface <- H.liftEffect makeInterface
    void $ H.subscribe emitter
    unsubscribePusher <-
      H.liftEffect
        $ paths
            ( \mls ls -> do
                HS.notify listener (Navigate (routeToPlace ls))
            )
            interface
    H.modify_ (_ { pushState = Just interface })
  Finalize -> do
    unsubscribePusher <- H.gets _.unsubscribePusher
    H.liftEffect (fromMaybe (pure unit) unsubscribePusher)
  Navigate klank -> do
    H.modify_ (_ { whereAmI = klank })
