module App.LoadingModal where

import Prelude
import App.AppAction (Action)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

loading :: forall w. { open :: Boolean } -> HH.HTML w Action
loading { open } =
  HH.div
    [ HP.classes $ map ClassName ([ "modal", "fixed", "w-full", "h-full", "top-0", "left-0", "flex", "items-center", "justify-center" ] <> if open then [] else [ "opacity-0", "pointer-events-none" ])
    ]
    [ HH.div
        [ HP.classes $ map ClassName [ "modal-overlay", "absolute", "w-full", "h-full", "bg-gray-900", "opacity-50" ]
        ]
        []
    , HH.div
        [ HP.classes $ map ClassName [ "modal-container", "bg-white", "w-11/12", "md:max-w-md", "mx-auto", "rounded", "shadow-lg", "z-50", "overflow-y-auto" ]
        ]
        [ HH.div
            [ HP.classes $ map ClassName [ "modal-content", "py-4", "text-left", "px-6" ]
            ]
            [ HH.div
                [ HP.classes $ map ClassName [ "flex", "justify-between", "items-center", "pb-3" ]
                ]
                [ HH.p
                    [ HP.classes $ map ClassName [ "text-2xl", "font-bold" ]
                    ]
                    [ HH.text "You've found klank.dev! 🎤🎧🔊" ]
                ]
            , HH.p [] [ HH.text "Loading... Awesomeness is imminent! ⏳" ]
            ]
        ]
    ]
