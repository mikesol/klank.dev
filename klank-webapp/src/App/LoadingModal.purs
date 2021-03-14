module App.LoadingModal where

import Prelude
import App.AppAction (Action)
import App.ModalUtil (modalBody, modalHeader)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

progressValue :: forall r i. Number -> HH.IProp ( value :: Number | r ) i
progressValue = HH.prop (HH.PropName "value")

loading :: forall w. { progressSoFar :: Number, progressMax :: Number, open :: Boolean } -> HH.HTML w Action
loading { open, progressSoFar, progressMax } =
  HH.div
    [ HP.classes $ map ClassName ([ "modal", "fixed", "w-full", "h-full", "top-0", "left-0", "flex", "items-center", "justify-center" ] <> if open then [] else [ "opacity-0", "pointer-events-none" ])
    ]
    [ HH.div
        [ HP.classes $ map ClassName [ "modal-overlay", "absolute", "w-full", "h-full", "bg-gray-900", "opacity-50" ]
        ]
        []
    , HH.div
        [ HP.classes $ map ClassName [ "modal-container", "bg-white", "mx-auto", "rounded", "shadow-lg", "z-50", "overflow-y-auto" ]
        ]
        [ HH.div
            [ HP.classes $ map ClassName [ "modal-content", "py-4", "text-left", "px-6" ]
            ]
            [ HH.div
                [ HP.classes $ map ClassName [ "flex", "justify-between", "items-center", "pb-3" ]
                ]
                [ modalHeader
                    [ HH.text "You've found klank.dev! 🎤🎧🔊" ]
                ]
            , modalBody [ HH.text "Loading..." ]
            , modalBody [ HH.progress [ progressValue progressSoFar, HP.max 100.0, HP.classes $ map ClassName [ "w-full" ] ] [] ]
            ]
        ]
    ]
