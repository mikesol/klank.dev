module Klank.Studio.SilentNight.Types.Intro where

import Prelude
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

type VerseStarts
  = { one :: Maybe (Tuple Number VerseChoice) -- chosenAt choice
    , two :: Maybe (Tuple Number VerseChoice) -- chosenAt choice
    , three :: Maybe (Tuple Number VerseChoice) -- chosenAt choice
    }

data IntroLoop
  = IntroLoopA
  | IntroLoopB
  | IntroLoopC
  | IntroLoopD
  | IntroLoopE

derive instance eqIntroLoop :: Eq IntroLoop

data Verse
  = Verse1
  | Verse2
  | Verse3

derive instance eqVerse :: Eq Verse

data VerseChoice
  = VersionOne
  | VersionTwo
  | VersionThree
  | VersionFour
  | VersionFive
  | VersionSix
  | VersionSeven
  | VersionEight

derive instance eqVerseChoice :: Eq VerseChoice

data HarmChooserStep
  = Row1Animation { startsAt :: Number }
  | Row1Choose
  | Row2Animation { startsAt :: Number, verseOne :: VerseChoice }
  | Row2Choose { verseOne :: VerseChoice }
  | Row3Animation { startsAt :: Number, verseOne :: VerseChoice, verseTwo :: VerseChoice }
  | Row3Choose { verseOne :: VerseChoice, verseTwo :: VerseChoice }
  | FadeOutAnimation { startsAt :: Number, verseOne :: VerseChoice, verseTwo :: VerseChoice, verseThree :: VerseChoice }

derive instance eqHarmChooserStep :: Eq HarmChooserStep

data ChoiceEvent
  = ChoiceEventA
  | ChoiceEventB
  | ChoiceEventC

derive instance choiceEventEq :: Eq ChoiceEvent
