module Klank.Studio.SilentNight.Types where

import Data.List

import Data.Typelevel.Num (D2)
import FRP.Behavior.Audio (AudioUnit)

type MusicalInfo
  = { measure :: Int
    , beat :: Number
    }

type AudioListD2
  = List (AudioUnit D2)
