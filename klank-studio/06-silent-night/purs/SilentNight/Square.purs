module Klank.Studio.SilentNight.Square where

import Prelude
import Control.Monad.Reader (ask, asks)
import Data.Array as A
import Data.Foldable (foldl, fold)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d0, d1, d2, d3, D4)
import Data.Vec (Vec)
import Data.Vec as V
import FRP.Behavior.Audio (gainT_', playBuf_)
import Klank.Studio.SilentNight.Intro (choicePlayer)
import Klank.Studio.SilentNight.Optics (AccumulatorGetter, AccumulatorSetter, BegTimeGetter, BegTimeSetter, EndTimeGetter, EndTimeSetter, amark, getBegTime, getEndTime, getInter, setBegTime, setEndTime, setInter)
import Klank.Studio.SilentNight.Types (AudioListD2)
import Klank.Studio.SilentNight.Types.Accumulator (MusicM)
import Klank.Studio.SilentNight.Util (boundPlayer, boundedEffect, conv1, epwf, standardEndTimeBleed)
import Type.Proxy (Proxy(..))

data SquarePos
  = SquarePosTopLeft
  | SquarePosTopRight
  | SquarePosBottomLeft
  | SquarePosBottomRight

shrinkNormal = 18.0 :: Number

squareTravel = 1.0 :: Number

squarePosToStr :: SquarePos -> String
squarePosToStr SquarePosTopLeft = "square1"

squarePosToStr SquarePosTopRight = "square2"

squarePosToStr SquarePosBottomLeft = "square3"

squarePosToStr SquarePosBottomRight = "square4"

squareSound :: SquarePos -> Number -> MusicM AudioListD2
squareSound tp st =
  let
    sps = squarePosToStr tp
  in
    do
      { time } <- ask
      bell <-
        choicePlayer ("cplr" <> sps)
          ( case tp of
              SquarePosTopLeft -> 1.35
              SquarePosTopRight -> 0.9
              SquarePosBottomLeft -> 1.79
              SquarePosBottomRight -> 0.6723
          )
      pure
        ( bell
            : gainT_' ("gain_square_" <> sps) (epwf [ Tuple 0.0 0.0, Tuple (st + 0.0) 0.0, Tuple (st + 0.2) 0.7, Tuple (st + 5.0) 0.0 ] time)
                ( playBuf_ ("buf_square_" <> sps) sps
                    ( case tp of
                        SquarePosTopRight -> conv1 (-1.0)
                        _ -> 1.0
                    )
                )
            : Nil
        )

squareP :: SquarePos -> Number -> MusicM AudioListD2
squareP sp t = boundPlayer t 5.0 (squareSound sp t)

squareEnd :: Number -> MusicM AudioListD2
squareEnd t =
  boundPlayer t 5.0
    ( do
        { time } <- ask
        bell <- choicePlayer "ceA" 1.01
        pure
          ( bell : (gainT_' ("gain_square_end") (epwf [ Tuple 0.0 0.0, Tuple (t + 0.0) 0.0, Tuple (t + 0.2) 0.7, Tuple (t + 5.0) 0.0 ] time) (playBuf_ ("buf_square_end") "square5" 1.0)) : Nil
          )
    )

square' :: Number -> MusicM AudioListD2
square' btm = do
  v <- asks getSquareVector
  let
    topLeft = (squareP SquarePosTopLeft) <$> (join $ flip V.index d0 <$> v)

    topRight = (squareP SquarePosTopRight) <$> (join $ flip V.index d1 <$> v)

    bottomLeft = (squareP SquarePosBottomLeft) <$> (join $ flip V.index d2 <$> v)

    bottomRight = (squareP SquarePosBottomRight) <$> (join $ flip V.index d3 <$> v)

    endSound = (squareEnd <<< (_ + squareTravel) <<< foldl max 0.0) <$> (join (sequence <$> v))
  fold
    <$> sequence
        ( A.catMaybes
            [ topLeft, topRight, bottomLeft, bottomRight, endSound
            ]
        )

square :: MusicM AudioListD2
square = boundedEffect "square" getSquareBegTime getSquareEndTime square'

squareLens = amark <<< prop (Proxy :: Proxy "square")

squareEndTimeBleed = standardEndTimeBleed :: Number

getSquareBegTime = getBegTime squareLens :: BegTimeGetter

getSquareVector = getInter squareLens :: AccumulatorGetter (Vec D4 (Maybe Number))

getSquareEndTime = getEndTime squareLens :: EndTimeGetter

setSquareBegTime = setBegTime squareLens :: BegTimeSetter

setSquareVector = setInter squareLens :: AccumulatorSetter (Vec D4 (Maybe Number))

setSquareEndTime = setEndTime squareLens :: EndTimeSetter
