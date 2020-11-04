module App.ModalUtil where

import Prelude
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

modalHeader :: ∀ t1 t2. Array (HH.HTML t2 t1) → HH.HTML t2 t1
modalHeader =
  HH.p
    [ HP.classes $ map ClassName [ "sm:text-5xl", "md:text-4xl", "lg:text-3xl", "font-bold" ]
    ]

modalBody :: ∀ t1 t2. Array (HH.HTML t2 t1) → HH.HTML t2 t1
modalBody = HH.p [ HP.classes $ map ClassName [ "sm:text-3xl", "md:text-2xl", "lg:text-xl" ] ]
