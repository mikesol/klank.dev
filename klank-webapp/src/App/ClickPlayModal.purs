module App.ClickPlayModal where

import Prelude
import App.AppAction (Action(..))
import App.ModalUtil (modalBody, modalHeader)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

clickPlay :: forall w. { open :: Boolean } -> HH.HTML w Action
clickPlay { open } =
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
                    [ HH.text "Ready when you are! 🚀🎵" ]
                ]
            , modalBody [ HH.text "Click or press play to start." ]
            , HH.div [ HP.classes $ map ClassName [ "flex", "justify-end", "pt-2" ] ]
                [ HH.button [ HP.classes $ map ClassName [ "sm:text-4xl", "md:text-3xl", "lg:text-2xl", "modal-close px-4", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> PlayKlankFromModal ] [ HH.text "Play" ]
                ]
            ]
        ]
    ]
