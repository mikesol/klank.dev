module App.LinkModal where

import Prelude

import App.AppAction (Action(..))
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Svg.Parser (SvgAttribute(..), SvgNode(..))
import Svg.Renderer.Halogen (svgElementToHtml)

mc :: ∀ t4 t5. String → HH.HTML t5 t4
mc color =
  svgElementToHtml
    $ { attributes:
          ( SvgAttribute "class" ("fill-current" <> color)
              : SvgAttribute "xmlns" "http://www.w3.org/2000/svg"
              : SvgAttribute "width" "18"
              : SvgAttribute "height" "18"
              : SvgAttribute "viewBox" "0 0 18 18"
              : Nil
          )
      , children:
          ( SvgElement
              { children: Nil
              , name: "path"
              , attributes:
                  ( SvgAttribute "d" "M14.53 4.53l-1.06-1.06L9 7.94 4.53 3.47 3.47 4.53 7.94 9l-4.47 4.47 1.06 1.06L9 10.06l4.47 4.47 1.06-1.06L10.06 9z"
                      : Nil
                  )
              }
              : Nil
          )
      , name: "svg"
      }

modal :: forall w. { url :: String, open :: Boolean } -> HH.HTML w Action
modal { url, open } =
  HH.div
    [ HP.classes $ map ClassName ([ "modal", "fixed", "w-full", "h-full", "top-0", "left-0", "flex", "items-center", "justify-center" ] <> if open then [] else ["opacity-0", "pointer-events-none"])
    ]
    [ HH.div
        [ HP.classes $ map ClassName [ "modal-overlay", "absolute", "w-full", "h-full", "bg-gray-900", "opacity-50" ]
        ]
        []
    , HH.div
        [ HP.classes $ map ClassName [ "modal-container", "bg-white", "w-11/12", "md:max-w-md", "mx-auto", "rounded", "shadow-lg", "z-50", "overflow-y-auto" ]
        ]
        [ HH.div
            [ HP.classes $ map ClassName [ "modal-close", "absolute", "top-0", "right-0", "cursor-pointer", "flex", "flex-col", "items-center", "mt-4", "mr-4", "text-white", "text-sm", "z-50" ],  HE.onClick \_ -> Just CloseLinkModal
            ]
            [ mc "text-white"
            , HH.span [ HP.classes $ map ClassName [ "text-sm" ] ]
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
                    [ HP.classes $ map ClassName [ "modal-close", "cursor-pointer", "z-50" ],  HE.onClick \_ -> Just CloseLinkModal
                    ]
                    [ mc "text-black"
                    ]
                ]
            , HH.p [] [ HH.text "Here is the link to your klank." ]
            , HH.p [HP.id_ "klank-share-url"] [ HH.text url ]
            , HH.div [ HP.classes $ map ClassName [ "flex", "justify-end", "pt-2" ] ]
                [ HH.button [ HP.classes $ map ClassName [ "modal-close px-4", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> Just CloseLinkModal ] [ HH.text "Close" ]
                ]
            ]
        ]
    ]

