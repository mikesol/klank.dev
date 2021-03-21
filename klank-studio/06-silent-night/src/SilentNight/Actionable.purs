module Klank.Studio.SilentNight.Actionable where

import Prelude
import Data.Maybe (maybe)
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Point)
import Klank.Studio.SilentNight.Types.Accumulator (SilentNightAccumulator)

inRect :: Point -> Rectangle -> Boolean
inRect p r = p.x >= r.x && p.y >= r.y && p.x <= (r.x + r.width) && p.y <= (r.y + r.height)

doAction :: SilentNightAccumulator -> Rectangle -> Boolean
doAction acc r = acc.initiatedClick && (maybe false (flip inRect r) acc.mousePosition)

doingAction :: SilentNightAccumulator -> Rectangle -> Boolean
doingAction acc r = acc.inClick && (maybe false (flip inRect r) acc.mousePosition)
