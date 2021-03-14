module App.LinkModal where

import Prelude
import App.AppAction (Action(..))
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

modal :: forall w. { url :: String, open :: Boolean, properNoun :: String } -> HH.HTML w Action
modal { url, open, properNoun } =
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
            [ HP.classes $ map ClassName [ "modal-close", "absolute", "top-0", "right-0", "cursor-pointer", "flex", "flex-col", "items-center", "mt-4", "mr-4", "text-white", "text-sm", "z-50" ], HE.onClick \_ -> CloseLinkModal
            ]
            [ HH.span [ HP.classes $ map ClassName [ "text-sm" ] ]
                [ HH.text "(Esc)" ]
            ]
        , HH.div
            [ HP.classes $ map ClassName [ "modal-content", "py-4", "text-left", "px-6" ]
            ]
            [ HH.div
                [ HP.classes $ map ClassName [ "flex", "justify-between", "items-center", "pb-3" ]
                ]
                [ HH.p
                    [ HP.classes $ map ClassName [ "text-2xl", "font-bold" ]
                    ]
                    [ HH.text "Share me!" ]
                , HH.div
                    [ HP.classes $ map ClassName [ "modal-close", "cursor-pointer", "z-50" ], HE.onClick \_ -> CloseLinkModal
                    ]
                    [ HH.text "x"
                    ]
                ]
            , HH.p [] [ HH.text $ "Below is the link to your " <> properNoun <> ".  Copy it and share with reckless abandon! 🚀" ]
            , HH.p [ HP.id "klank-share-url" ] [ HH.text url ]
            , HH.div [ HP.classes $ map ClassName [ "flex", "justify-end", "pt-2" ] ]
                [ HH.button [ HP.classes $ map ClassName [ "modal-close px-4", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> CloseLinkModal ] [ HH.text "Close" ]
                ]
            ]
        ]
    ]
