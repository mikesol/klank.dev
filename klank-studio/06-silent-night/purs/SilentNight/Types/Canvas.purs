module Klank.Studio.SilentNight.Types.Canvas where

import Control.Monad.Reader (Reader)
import Data.Tuple (Tuple)
import Graphics.Painting (Painting)
import Klank.Studio.SilentNight.Types.Accumulator (PlayerEvent, SilentNightAccumulator)

type MakeCanvasT
  = Reader { evts :: Array PlayerEvent, w :: Number, h :: Number } (Tuple SilentNightAccumulator Painting)

type MakeCanvas
  = SilentNightAccumulator -> Number -> MakeCanvasT
