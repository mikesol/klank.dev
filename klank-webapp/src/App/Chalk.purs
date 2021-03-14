module App.Chalk where

import Data.Maybe (Maybe(..))

foreign import chalk :: Chalk -> String -> String

type Chalk
  = { color :: Maybe CColor
    , bg :: Maybe Bg
    , bold :: Boolean
    , dim :: Boolean
    , italic :: Boolean
    , underline :: Boolean
    , inverse :: Boolean
    , hidden :: Boolean
    , strikethrough :: Boolean
    , visible :: Boolean
    }

defaultChalk =
  { color: Nothing
  , bg: Nothing
  , bold: false
  , dim: false
  , italic: false
  , underline: false
  , inverse: false
  , hidden: false
  , strikethrough: false
  , visible: false
  } ::
    Chalk

data CColor
  = CBlack
  | CRed
  | CGreen
  | CYellow
  | CBlue
  | CMagenta
  | CCyan
  | CWhite
  | CBlackBright
  | CRedBright
  | CGreenBright
  | CYellowBright
  | CBlueBright
  | CMagentaBright
  | CCyanBright
  | CWhiteBright

data Bg
  = BgBlack
  | BgRed
  | BgGreen
  | BgYellow
  | BgBlue
  | BgMagenta
  | BgCyan
  | BgWhite
  | BgBlackBright
  | BgRedBright
  | BgGreenBright
  | BgYellowBright
  | BgBlueBright
  | BgMagentaBright
  | BgCyanBright
  | BgWhiteBright
