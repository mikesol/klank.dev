module Klank.Studio.SilentNight.Types.Accumulator where

import Prelude

import Control.Monad.Reader (Reader)
import Data.Either (Either)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Typelevel.Num (D3, D4, D6)
import Data.Vec (Vec)
import Graphics.Painting (Point)
import Klank.Studio.SilentNight.Types (MusicalInfo)
import Klank.Studio.SilentNight.Types.Intro (HarmChooserStep, Verse, VerseChoice, VerseStarts)

type Marker a
  = Maybe (Tuple Number (Maybe (Tuple a (Maybe Number))))

type VecMarker :: forall k. k -> Type
type VecMarker a
  = Marker (Vec a (Maybe Number))
  
type VecMarker' :: forall k. k -> Type
type VecMarker' a
  = Marker (Vec a Number)

type TriangleMarker
  = VecMarker D3 -- startPts end

type SquareMarker
  = VecMarker D4 -- startPts end

type MotionMarker
  = Marker Point -- intensity

type RiseMarker
  = VecMarker D6 -- startPts end

type LargeMarker
  = Marker (List Number) -- onsets end

type ShrinkMarker
  = VecMarker' D6 -- circleRs end

type BellsMarker
  = Marker (List (List Number)) -- onsets end

type GearsMarker
  = VecMarker D4 -- startPts end

type SnowMarker
  = Marker (List (Maybe Number)) -- onsets end

type HeartMarker
  = Marker Number

type AudioMarkers
  = { triangle :: TriangleMarker
    , square :: SquareMarker
    , motion :: MotionMarker
    , rise :: RiseMarker
    , large :: LargeMarker
    , bells :: BellsMarker
    , gears :: GearsMarker
    , shrink :: ShrinkMarker
    , snow :: SnowMarker
    , heart :: HeartMarker
    }

type AudioEnv
  = { initiatedCoda :: Boolean
    , mainStarts :: Maybe Number
    , audioMarkers :: AudioMarkers
    , verseStarts :: VerseStarts
    , time :: Number
    , musicalInfo :: MusicalInfo
    }

type SilentNightPlayerT
  = { verse :: Verse
    , verseOne :: VerseChoice
    , verseTwo :: VerseChoice
    , verseThree :: VerseChoice
    , playerEvents :: Array PlayerEvent
    , eventStart :: Number
    }

data Activity
  = Intro
  | HarmChooser { step :: HarmChooserStep }
  | SilentNightPlayer SilentNightPlayerT

derive instance eqActivity :: Eq Activity

defaultAudioMarkers :: AudioMarkers
defaultAudioMarkers =
  { triangle: Nothing
  , square: Nothing
  , motion: Nothing
  , rise: Nothing
  , large: Nothing
  , bells: Nothing
  , gears: Nothing
  , shrink: Nothing
  , snow: Nothing
  , heart: Nothing
  }

data PlayerEvent
  = Triangle (Vec D3 (Maybe Number))
  | Square (Vec D4 (Maybe Number))
  | Motion (Maybe Point) (Either Point (Tuple Point Point)) -- resting point or offset from mouse
  | Rise (Vec D6 (Maybe (Tuple Number Number))) -- pos, stopped
  | Large (List (Tuple Point Number)) -- pos, startT
  | Bells (List (List Number))
  | Gears (Vec D4 (Maybe Number))
  | Shrink (Vec D6 (Number))
  | Snow (List (Maybe Number)) -- time
  | Heart (Maybe Number)
  | NoEvent Number -- time

derive instance eqPlayerEvent :: Eq PlayerEvent

type SilentNightAccumulator
  = { initiatedClick :: Boolean
    , inClick :: Boolean
    , curClickId :: Maybe Int
    , mousePosition :: Maybe { x :: Number, y :: Number }
    , activity :: Activity
    , initiatedCoda :: Boolean
    , verseStarts :: VerseStarts
    , mainStarts :: Maybe Number
    , introEnds :: Maybe Number
    , audioMarkers :: AudioMarkers
    }
type MusicM
  = Reader AudioEnv