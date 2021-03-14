module App.XTermTheme where

-- from https://github.com/jan-warchol/selenized/blob/master/terminals/alacritty/selenized-dark.yml
type XTermTheme
  = { foreground :: String
    , background :: String
    , cursor :: String
    , black :: String
    , red :: String
    , green :: String
    , blue :: String
    , magenta :: String
    , cyan :: String
    , white :: String
    , brightBlack :: String
    , brightRed :: String
    , brightGreen :: String
    , brightYellow :: String
    , brightBlue :: String
    , brightMagenta :: String
    , brightCyan :: String
    , brightWhite :: String
    }

darkTheme =
  { foreground: "#adbcbc"
  , background: "#103c48"
  , cursor: "rgba(255,255,255,0.5)"
  , black: "#184956"
  , red: "#fa5750"
  , green: "#75b938"
  , blue: "#4695f7"
  , magenta: "#f275be"
  , cyan: "#41c7b9"
  , white: "#72898f"
  , brightBlack: "#2d5b69"
  , brightRed: "#ff665c"
  , brightGreen: "#84c747"
  , brightYellow: "#ebc13d"
  , brightBlue: "#58a3ff"
  , brightMagenta: "#ff84cd"
  , brightCyan: "#53d6c7"
  , brightWhite: "#cad8d9"
  } ::
    XTermTheme
