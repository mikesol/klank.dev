module Klank.Studio.SilentNight.Verse where

import Prelude
import Control.Monad.Reader (ask)
import Data.Array (catMaybes)
import Data.Foldable (fold)
import Data.Maybe (maybe)
import Data.Traversable (sequence)
import Data.Tuple (snd)
import FRP.Behavior.Audio (defaultParam, playBufT_)
import Klank.Studio.SilentNight.Types (AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (MusicM)
import Klank.Studio.SilentNight.Types.Intro (Verse(..), VerseChoice(..))
import Klank.Studio.SilentNight.Util (boundPlayer, crotchet, kr, silentNightInBeatsAsTime)

verseToString :: Verse -> String
verseToString Verse1 = "1"

verseToString Verse2 = "2"

verseToString Verse3 = "3"

choiceToString :: VerseChoice -> String
choiceToString VersionOne = "1"

choiceToString VersionTwo = "2"

choiceToString VersionThree = "3"

choiceToString VersionFour = "4"

choiceToString VersionFive = "5"

choiceToString VersionSix = "6"

choiceToString VersionSeven = "7"

choiceToString VersionEight = "8"

playVerse :: Number -> Verse -> VerseChoice -> MusicM AudioListD2
playVerse st v vc =
  let
    vvc = verseAndChoiceToString v vc
  in
    boundPlayer st (silentNightInBeatsAsTime + (if v == Verse3 then 60.0 else 5.0))
      ( do
          { time } <- ask
          let
            gp = st - time

            pm = if (gp < kr) then max gp 0.0 else 0.0
          pure (pure (playBufT_ ("verse_" <> vvc) vvc (defaultParam { param = 1.0, timeOffset = pm })))
      )

verseAndChoiceToString :: Verse -> VerseChoice -> String
verseAndChoiceToString v vc = "v" <> verseToString v <> "t" <> choiceToString vc

verseThreeCorrective = 0.25 * crotchet :: Number

verseThreeStart = (2.0 * silentNightInBeatsAsTime) - ((5.0 * (3.0 * crotchet)) - verseThreeCorrective) :: Number

verses :: MusicM AudioListD2
verses = do
  { mainStarts, verseStarts: { one, two, three } } <- ask
  maybe mempty
    ( \mt ->
        fold
          <$> sequence
              ( catMaybes
                  [ (playVerse (mt - 3.0 * crotchet) Verse1 <<< snd) <$> one
                  , (playVerse (mt + silentNightInBeatsAsTime - 3.0 * crotchet) Verse2 <<< snd) <$> two
                  , (playVerse (mt + verseThreeStart) Verse3 <<< snd) <$> three
                  ]
              )
    )
    mainStarts
