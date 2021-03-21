module Klank.Studio.SilentNight.PitchClass where

import Prelude

data PitchClass
  = Ab
  | A
  | Bb
  | Cb
  | C
  | Db
  | D
  | Eb
  | Fb
  | F
  | Gb
  | G

derive instance eqPitchClass :: Eq PitchClass

instance ordPitchClass :: Ord PitchClass where
  compare a b = compare (pc2i a) (pc2i b)

pc2i :: PitchClass -> Int
pc2i Ab = 0

pc2i A = 1

pc2i Bb = 2

pc2i Cb = 3

pc2i C = 4

pc2i Db = 5

pc2i D = 6

pc2i Eb = 7

pc2i Fb = 8

pc2i F = 9

pc2i Gb = 10

pc2i G = 11

pcToRefMidi :: PitchClass -> Number
pcToRefMidi Ab = 56.0

pcToRefMidi A = 57.0

pcToRefMidi Bb = 58.0

pcToRefMidi Cb = 59.0

pcToRefMidi C = 60.0

pcToRefMidi Db = 61.0

pcToRefMidi D = 62.0

pcToRefMidi Eb = 63.0

pcToRefMidi Fb = 64.0

pcToRefMidi F = 65.0

pcToRefMidi Gb = 66.0

pcToRefMidi G = 67.0
