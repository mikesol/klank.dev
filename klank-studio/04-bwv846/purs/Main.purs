module Main where

import Prelude
import Control.Promise (toAffE)
import Data.List (List(..))
import Data.NonEmpty ((:|))
import Data.Traversable (sequence)
import Effect (Effect)
import FRP.Behavior.Audio (AudioUnit, decodeAudioDataFromUri, defaultParam, playBufT_, runInBrowser_, speaker)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Klank.Dev.Util (affable)
import Klank.Weblib.Studio as Studio
import Type.Klank.Dev (Klank, Buffers, klank)
import Data.Array (drop, foldl, length, mapWithIndex, range, take, zipWith)
import Data.Int (toNumber)
import Data.List as L
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D2)
import Effect.Aff (parallel, sequential)
import Effect.Random (random)
import Foreign.Object as O

kr = 20.0 / 1000.0 :: Number

harp :: String -> Number -> Number -> Number -> List (AudioUnit D2)
harp name t s dur =
  if t + kr > s && t < (s + dur + kr) then
    pure
      $ playBufT_ (name <> "_" <> (show s)) name
          (defaultParam { param = 1.0, timeOffset = max (s - t) 0.0 })
  else
    Nil

tempo = 0.2173 :: Number

mez :: Int -> Array Int -> Array PVD
mez n a =
  mapWithIndex
    ( \i x ->
        PVD x (toNumber (i + n * 16))
          (tempo * (4.0 + (0.5 * (toNumber $ 8 - (i `mod` 8)))))
    )
    (a_ <> a_)
  where
  a_ = a <> drop 2 a

fin = 512.0 :: Number

piece :: Array PVD
piece =
  mez 0 [ 37, 41, 44, 49, 53 ]
    <> mez 1 [ 37, 39, 46, 51, 54 ]
    <> mez 2 [ 36, 39, 44, 51, 54 ]
    <> mez 3 [ 37, 41, 44, 49, 53 ]
    <> mez 4 [ 37, 41, 46, 53, 58 ]
    <> mez 5 [ 37, 39, 43, 46, 51 ]
    <> mez 6 [ 36, 39, 44, 51, 56 ]
    <> mez 7 [ 36, 37, 41, 44, 49 ]
    <> mez 8 [ 34, 37, 41, 44, 49 ]
    <> mez 9 [ 27, 34, 39, 43, 49 ]
    <> mez 10 [ 32, 36, 39, 44, 48 ]
    <> mez 11 [ 32, 35, 41, 44, 50 ]
    <> mez 12 [ 30, 34, 39, 46, 51 ]
    <> mez 13 [ 30, 33, 39, 42, 48 ]
    <> mez 14 [ 29, 32, 37, 44, 49 ]
    <> mez 15 [ 29, 30, 34, 37, 42 ]
    <> mez 16 [ 27, 30, 34, 37, 42 ]
    <> mez 17 [ 20, 27, 32, 36, 42 ]
    <> mez 18 [ 25, 29, 32, 37, 41 ]
    <> mez 19 [ 25, 32, 35, 37, 41 ]
    <> mez 20 [ 18, 30, 34, 37, 41 ]
    <> mez 21 [ 19, 25, 34, 37, 40 ]
    <> mez 22 [ 21, 30, 36, 37, 39 ]
    <> mez 23 [ 20, 30, 32, 36, 39 ]
    <> mez 24 [ 20, 29, 32, 37, 41 ]
    <> mez 25 [ 20, 27, 32, 37, 42 ]
    <> mez 26 [ 20, 27, 32, 36, 42 ]
    <> mez 27 [ 20, 28, 34, 37, 43 ]
    <> mez 28 [ 20, 29, 32, 37, 44 ]
    <> mez 29 [ 20, 27, 32, 37, 42 ]
    <> mez 30 [ 20, 27, 32, 36, 42 ]
    <> mez 31 [ 13, 25, 32, 35, 41 ]
    <> ( map (\(PVD a b c) -> PVD a (b + fin) c)
          ( _.acc
              $ foldl
                  ( \{ acc, prev } (PVD a b d) ->
                      { acc: acc <> [ PVD a prev d ], prev: b + prev
                      }
                  )
                  { acc: [], prev: 0.0 }
                  [ PVD 13 1.0 1.5
                  , PVD 25 1.0 1.5
                  , PVD 30 1.0 1.5
                  , PVD 34 1.0 1.5
                  , PVD 37 1.0 1.5
                  , PVD 42 1.0 1.5
                  , PVD 37 1.0 1.5
                  , PVD 34 1.0 1.5
                  , PVD 37 1.0 1.5
                  , PVD 34 1.0 1.5
                  , PVD 30 1.0 1.5
                  , PVD 34 1.0 1.5
                  , PVD 30 1.1 1.5
                  , PVD 27 1.1 1.5
                  , PVD 30 1.2 1.5
                  , PVD 27 1.6 1.5
                  , PVD 13 1.9 6.0
                  , PVD 24 1.65 6.0
                  , PVD 44 1.4 6.0
                  , PVD 48 1.25 6.0
                  , PVD 51 1.2 6.0
                  , PVD 54 1.1 6.0
                  , PVD 51 1.05 6.0
                  , PVD 48 1.0 6.0
                  , PVD 51 1.05 6.0
                  , PVD 48 1.1 6.0
                  , PVD 44 1.15 6.0
                  , PVD 48 1.2 5.0
                  , PVD 39 1.3 4.0
                  , PVD 42 1.7 3.0
                  , PVD 41 2.0 3.0
                  , PVD 39 6.0 3.0
                  , PVD 13 0.5 6.0
                  , PVD 25 0.2 6.0
                  , PVD 41 0.23 6.0
                  , PVD 44 0.2 6.0
                  , PVD 49 1.0 6.0
                  , PVD 39 1.0 6.0
                  , PVD 41 1.0 6.0
                  ]
          )
      )

data PVD
  = PVD Int Number Number

scene :: List PVD -> Number -> AudioUnit D2
scene pc t =
  speaker
    ( zero
        :| ( join
              ( map
                  ( \(PVD p v d) ->
                      harp (show (p + 0)) t
                        (tempo * v)
                        d
                  )
                  pc
              )
          )
    )

buffers :: Buffers
buffers ctx _ =
  affable
    $ sequential
    $ sequence
        ( O.fromFoldable
            $ map
                ( \i ->
                    Tuple (show i)
                      $ ( parallel
                            $ toAffE
                                ( decodeAudioDataFromUri
                                    ctx
                                    $ "https://sound.klank.dev/petit-italien-k2/jeu-1/"
                                    <> show i
                                    <> ".mp3"
                                )
                        )
                )
                (range 12 63)
        )

playMe :: Klank
playMe =
  klank
    { run =
      runInBrowser_ do
        -- a slight jank to liven it up
        randy <-
          sequence
            $ map
                ( const
                    $ ( do
                          r0 <- random
                          r1 <- random
                          pure $ Tuple r0 r1
                      )
                )
                (range 0 (length piece - 1))
        additiveJank <-
          sequence
            $ map
                (const random)
                (range 0 (length piece - 1))
        let
          smaller = map (\a -> Tuple (fst a * 0.015) (snd a * 0.3)) randy
        let
          pjanked =
            zipWith
              ( \aj (PVD a b d) ->
                  ( PVD a
                      (b + (0.02 * (foldl (+) 0.0 $ take aj additiveJank)))
                      d
                  )
              )
              ( range 0
                  (length additiveJank - 1)
              )
              $ zipWith (\(Tuple r x) (PVD a b d) -> (PVD a (b + r) (d + x))) smaller piece
        pure $ scene (L.fromFoldable pjanked)
    , buffers = buffers
    }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (Studio.component (pure playMe)) unit body
