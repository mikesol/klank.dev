module Test.Main where

import Prelude
import Data.Symbol (SProxy(..))
import Data.Typelevel.Num (class Pos, D1)
import FRP.Behavior.Audio (AudioUnit, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, Oversample, defaultParam)
import Type.Data.Row (RProxy(..))
import Type.Klank.Dev (ConsSymbol, NilSymbol, SymbolListProxy, tAudioWorkletGenerator, tAudioWorkletGeneratorT, tPeriodicOsc, tPlay, tPlayBuf, tWaveShaper, toUrlArray)

--------------------------------
__test_tPlay :: forall ch. Pos ch => AudioUnit ch
__test_tPlay =
  ( tPlay
      ( RProxy ::
          RProxy
            ( bassPlease :: BrowserAudioTrack )
      )
  )
    ( SProxy ::
        SProxy "bassPlease"
    )

__test_tPlayBuf :: forall ch. Pos ch => Number -> AudioUnit ch
__test_tPlayBuf =
  ( tPlayBuf
      ( RProxy ::
          RProxy
            ( bassPlease :: BrowserAudioBuffer )
      )
      ( SProxy ::
          SProxy "bassPlease"
      )
  )

__test_tPeriodicOsc :: Number -> AudioUnit D1
__test_tPeriodicOsc =
  ( tPeriodicOsc
      ( RProxy ::
          RProxy
            ( wavey :: BrowserPeriodicWave )
      )
      ( SProxy ::
          SProxy "wavey"
      )
  )

__test_tWaveShaper :: forall ch. Pos ch => Oversample -> AudioUnit ch -> AudioUnit ch
__test_tWaveShaper =
  ( tWaveShaper
      ( RProxy ::
          RProxy
            ( crunch :: BrowserFloatArray )
      )
      ( SProxy ::
          SProxy "crunch"
      )
  )

___wu :: Array String
___wu = toUrlArray (RProxy :: RProxy ( foo :: Void, bar :: Void )) { foo: "", bar: "" }

__test_tAudioWorkletGenerator ::
  AudioUnit D1
__test_tAudioWorkletGenerator =
  ( tAudioWorkletGenerator
      ( RProxy ::
          RProxy
            ( myProc :: SymbolListProxy (ConsSymbol "foo" (ConsSymbol "bar" NilSymbol)) )
      )
      ( SProxy ::
          SProxy "myProc"
      )
      { foo: 1.0, bar: 1.0 }
  )

__test_tAudioWorkletGeneratorT ::
  AudioUnit D1
__test_tAudioWorkletGeneratorT =
  ( tAudioWorkletGeneratorT
      ( RProxy ::
          RProxy
            ( myProc :: SymbolListProxy (ConsSymbol "foo" (ConsSymbol "bar" NilSymbol)) )
      )
      ( SProxy ::
          SProxy "myProc"
      )
      { foo: (defaultParam { param = 1.0, timeOffset = 0.0 })
      , bar: (defaultParam { param = 1.0, timeOffset = 0.0 })
      }
  )
