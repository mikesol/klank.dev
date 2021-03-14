module Type.Klank.Dev where

import Prelude

import Data.List (List, take)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple, snd)
import Data.Typelevel.Num (class Pos, D1)
import Effect (Effect)
import Effect.Exception (Error)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioParameter, AudioUnit, BrowserAudioBuffer, BrowserAudioTrack, BrowserFloatArray, BrowserPeriodicWave, EngineInfo, Exporter, MediaRecorder, Oversample, RecorderSignature, Run, audioWorkletGenerator, audioWorkletGeneratorT, audioWorkletGeneratorT_, audioWorkletGenerator_, audioWorkletProcessor, audioWorkletProcessorT, audioWorkletProcessorT_, audioWorkletProcessor_, defaultExporter, loopBuf, loopBufT, loopBufT_, loopBuf_, periodicOsc, periodicOscT, periodicOscT_, periodicOsc_, play, playBuf, playBufT, playBufT_, playBuf_, play_, runInBrowser, speaker', waveShaper, waveShaper_)
import Foreign.Object (Object, fromHomogeneous)
import Foreign.Object as O
import Prim.Boolean (False, True)
import Prim.Ordering as Ordering
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.Symbol (class Compare)
import Type.Data.Boolean (class And, class Or)
import Type.Proxy (Proxy)
import Type.Row.Homogeneous (class Homogeneous)
import Web.HTML (HTMLCanvasElement, HTMLImageElement, HTMLVideoElement)

data SymbolListProxy (s :: SymbolList)
  = SymbolListProxy

type EnableMicrophone
  = Boolean

type EnableCamera
  = Boolean

type Accumulator accumulator
  = (accumulator -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type WebcamCache accumulator
  = accumulator -> Number -> List (Tuple Number HTMLCanvasElement) -> List (Tuple Number HTMLCanvasElement)

type AsyncEngineInfo
  = (EngineInfo -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type Worklets
  = (Array String) -> (Array String -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type Tracks
  = (Object BrowserAudioTrack) -> (Object BrowserAudioTrack -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type Images
  = (Object HTMLImageElement) -> (Object HTMLImageElement -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type Videos
  = (Object HTMLVideoElement) -> (Object HTMLVideoElement -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type Canvases
  = (Object HTMLCanvasElement) -> (Object HTMLCanvasElement -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type Buffers
  = AudioContext -> (Object BrowserAudioBuffer) -> (Object BrowserAudioBuffer -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

-- two caches - the current kv pairs of recorders
-- and the current functions that record
type Recorders
  = Object String -> (String -> String -> Effect Unit) -> Object (RecorderSignature MediaRecorder) -> (Object (RecorderSignature MediaRecorder) -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type FloatArrays
  = (Object BrowserFloatArray) -> (Object BrowserFloatArray -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

type PeriodicWaves
  = AudioContext -> (Object BrowserPeriodicWave) -> (Object BrowserPeriodicWave -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit


type Klank'' accumulator env
  = { run :: Run accumulator env
    , periodicWaves :: PeriodicWaves
    , floatArrays :: FloatArrays
    , recorders :: Recorders
    , buffers :: Buffers
    , tracks :: Tracks
    , images :: Images
    , videos :: Videos
    , webcamCache :: WebcamCache accumulator
    , canvases :: Canvases
    , worklets :: Worklets
    , enableMicrophone :: EnableMicrophone
    , enableCamera :: EnableCamera
    , accumulator :: Accumulator accumulator
    , exporter :: Exporter env accumulator
    , engineInfo :: AsyncEngineInfo
    }

type Klank' accumulator
  = Klank'' accumulator Unit

type Klank
  = Klank' Unit

noSound :: Number -> Behavior (AudioUnit D1)
noSound = const $ pure (speaker' zero)

defaultEngineInfo =
  { msBetweenSamples: 20
  , msBetweenPings: 15
  , fastforwardLowerBound: 0.025
  , rewindUpperBound: 0.15
  , initialOffset: 0.1
  , doWebAudio: true
  } ::
    EngineInfo

klank :: Klank
klank =
  { run: runInBrowser noSound
  , periodicWaves: \_ prev res _ -> res prev
  , floatArrays: \prev res _ -> res prev
  , recorders:
      \_ _ prev res _ -> res prev
  , buffers: \_ prev res _ -> res prev
  , tracks: \prev res _ -> res prev
  , images: \prev res _ -> res prev
  , videos: \prev res _ -> res prev
  , canvases: \prev res _ -> res prev
  , worklets: \prev res _ -> res prev
  , enableMicrophone: false
  , enableCamera: false
  , webcamCache: \_ _ -> take 10
  , accumulator: \res _ -> res unit
  , exporter: defaultExporter
  , engineInfo: \res _ -> res defaultEngineInfo
  }

class HasTrack (env :: Row Type) (s :: Symbol)

instance hasTrack ::
  ( Cons s _0 _1 tracks
  , Homogeneous tracks BrowserAudioTrack
  ) =>
  HasTrack tracks s

tPlay ::
  forall (env :: Row Type) (s :: Symbol) ch.
  Pos ch =>
  HasTrack env s =>
  IsSymbol s =>
  Proxy env ->
  Proxy s ->
  AudioUnit ch
tPlay _ s = play (reflectSymbol s)

type PlaySignature (myTracks :: Row Type)
  = forall ch s a t.
    HasTrack myTracks s =>
    Pos ch =>
    Cons s a t myTracks =>
    IsSymbol s =>
    Proxy s ->
    AudioUnit ch

tPlay_ ::
  forall (env :: Row Type) (s :: Symbol) ch.
  Pos ch =>
  HasTrack env s =>
  IsSymbol s =>
  String ->
  Proxy env ->
  Proxy s ->
  AudioUnit ch
tPlay_ n _ s = play_ n (reflectSymbol s)

type Play_Signature (myTracks :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasTrack myTracks s =>
    Cons s a t myTracks =>
    IsSymbol s =>
    String ->
    Proxy s ->
    AudioUnit ch

class HasBuffer (env :: Row Type) (s :: Symbol)

instance hasBuffer ::
  ( Cons s _0 _1 buffers
  , Homogeneous buffers BrowserAudioBuffer
  ) =>
  HasBuffer buffers s

tPlayBuf ::
  forall (env :: Row Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  Proxy env ->
  Proxy s ->
  Number ->
  AudioUnit ch
tPlayBuf _ s = playBuf (reflectSymbol s)

type PlayBufSignature (myBuffers :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    Proxy s ->
    Number ->
    AudioUnit ch

tPlayBuf_ ::
  forall (env :: Row Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  String ->
  Proxy env ->
  Proxy s ->
  Number ->
  AudioUnit ch
tPlayBuf_ n _ s = playBuf_ n (reflectSymbol s)

type PlayBuf_Signature (myBuffers :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    String ->
    Proxy s ->
    Number ->
    AudioUnit ch

tPlayBufT ::
  forall (env :: Row Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  Proxy env ->
  Proxy s ->
  AudioParameter ->
  AudioUnit ch
tPlayBufT _ s = playBufT (reflectSymbol s)

type PlayBufTSignature (myBuffers :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    Proxy s ->
    AudioParameter ->
    AudioUnit ch

tPlayBufT_ ::
  forall (env :: Row Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  String ->
  Proxy env ->
  Proxy s ->
  AudioParameter ->
  AudioUnit ch
tPlayBufT_ n _ s = playBufT_ n (reflectSymbol s)

type PlayBufT_Signature (myBuffers :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    String ->
    Proxy s ->
    AudioParameter ->
    AudioUnit ch

tLoopBuf ::
  forall (env :: Row Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  Proxy env ->
  Proxy s ->
  Number ->
  Number ->
  Number ->
  AudioUnit ch
tLoopBuf _ s = loopBuf (reflectSymbol s)

type LoopBufSignature (myBuffers :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    Proxy s ->
    Number ->
    Number ->
    Number ->
    AudioUnit ch

tLoopBuf_ ::
  forall (env :: Row Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  String ->
  Proxy env ->
  Proxy s ->
  Number ->
  Number ->
  Number ->
  AudioUnit ch
tLoopBuf_ n _ s = loopBuf_ n (reflectSymbol s)

type LoopBuf_Signature (myBuffers :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    String ->
    Proxy s ->
    Number ->
    Number ->
    Number ->
    AudioUnit ch

tLoopBufT ::
  forall (env :: Row Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  Proxy env ->
  Proxy s ->
  AudioParameter ->
  Number ->
  Number ->
  AudioUnit ch
tLoopBufT _ s = loopBufT (reflectSymbol s)

type LoopBufTSignature (myBuffers :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    Proxy s ->
    AudioParameter ->
    Number ->
    Number ->
    AudioUnit ch

tLoopBufT_ ::
  forall (env :: Row Type) (s :: Symbol) ch.
  Pos ch =>
  HasBuffer env s =>
  IsSymbol s =>
  String ->
  Proxy env ->
  Proxy s ->
  AudioParameter ->
  Number ->
  Number ->
  AudioUnit ch
tLoopBufT_ n _ s = loopBufT_ n (reflectSymbol s)

type LoopBufT_Signature (myBuffers :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasBuffer myBuffers s =>
    Cons s a t myBuffers =>
    IsSymbol s =>
    String ->
    Proxy s ->
    AudioParameter ->
    Number ->
    Number ->
    AudioUnit ch

class HasPeriodicWave (env :: Row Type) (s :: Symbol)

instance hasPeriodicWave ::
  ( Cons s _0 _1 periodicWaves
  , Homogeneous periodicWaves BrowserPeriodicWave
  ) =>
  HasPeriodicWave periodicWaves s

tPeriodicOsc ::
  forall (env :: Row Type) (s :: Symbol).
  HasPeriodicWave env s =>
  IsSymbol s =>
  Proxy env ->
  Proxy s ->
  Number ->
  AudioUnit D1
tPeriodicOsc _ s = periodicOsc (reflectSymbol s)

type PeriodicOscSignature (periodicWaves :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasPeriodicWave periodicWaves s =>
    Cons s a t periodicWaves =>
    IsSymbol s =>
    Proxy s ->
    Number ->
    AudioUnit ch

tPeriodicOsc_ ::
  forall (env :: Row Type) (s :: Symbol).
  HasPeriodicWave env s =>
  IsSymbol s =>
  String ->
  Proxy env ->
  Proxy s ->
  Number ->
  AudioUnit D1
tPeriodicOsc_ n _ s = periodicOsc_ n (reflectSymbol s)

type PeriodicOsc_Signature (periodicWaves :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasPeriodicWave periodicWaves s =>
    Cons s a t periodicWaves =>
    IsSymbol s =>
    String ->
    Proxy s ->
    Number ->
    AudioUnit ch

tPeriodicOscT ::
  forall (env :: Row Type) (s :: Symbol).
  HasPeriodicWave env s =>
  IsSymbol s =>
  Proxy env ->
  Proxy s ->
  AudioParameter ->
  AudioUnit D1
tPeriodicOscT _ s = periodicOscT (reflectSymbol s)

type PeriodicOscTSignature (periodicWaves :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasPeriodicWave periodicWaves s =>
    Cons s a t periodicWaves =>
    IsSymbol s =>
    Proxy s ->
    AudioParameter ->
    AudioUnit ch

tPeriodicOscT_ ::
  forall (env :: Row Type) (s :: Symbol).
  HasPeriodicWave env s =>
  IsSymbol s =>
  String ->
  Proxy env ->
  Proxy s ->
  AudioParameter ->
  AudioUnit D1
tPeriodicOscT_ n _ s = periodicOscT_ n (reflectSymbol s)

type PeriodicOscT_Signature (periodicWaves :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasPeriodicWave periodicWaves s =>
    Cons s a t periodicWaves =>
    IsSymbol s =>
    String ->
    Proxy s ->
    AudioParameter ->
    AudioUnit ch

class HasFloatArray (env :: Row Type) (s :: Symbol)

instance hasFloatArray ::
  ( Cons s _0 _1 floatArrays
  , Homogeneous floatArrays BrowserFloatArray
  ) =>
  HasFloatArray floatArrays s

type WaveShaperSignature (myFloatArrays :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasFloatArray myFloatArrays s =>
    Cons s a t myFloatArrays =>
    IsSymbol s =>
    Proxy s ->
    Oversample ->
    AudioUnit ch ->
    AudioUnit ch

tWaveShaper ::
  forall (env :: Row Type) (s :: Symbol) ch.
  Pos ch =>
  HasFloatArray env s =>
  IsSymbol s =>
  Proxy env ->
  Proxy s ->
  Oversample ->
  AudioUnit ch ->
  AudioUnit ch
tWaveShaper _ s = waveShaper (reflectSymbol s)

type WaveShaper_Signature (myFloatArrays :: Row Type)
  = forall ch s a t.
    Pos ch =>
    HasFloatArray myFloatArrays s =>
    Cons s a t myFloatArrays =>
    IsSymbol s =>
    String ->
    Proxy s ->
    Oversample ->
    AudioUnit ch ->
    AudioUnit ch

tWaveShaper_ ::
  forall (env :: Row Type) (s :: Symbol) ch.
  Pos ch =>
  HasFloatArray env s =>
  IsSymbol s =>
  String ->
  Proxy env ->
  Proxy s ->
  Oversample ->
  AudioUnit ch ->
  AudioUnit ch
tWaveShaper_ n _ s = waveShaper_ n (reflectSymbol s)

data SymbolList

foreign import data ConsSymbol :: Symbol -> SymbolList -> SymbolList

infixr 4 type ConsSymbol as :$

foreign import data NilSymbol :: SymbolList

type NilS
  = NilSymbol

-- A spec for an audio worklet
-- The name
-- The url
-- A list of params
foreign import data WorkletSpec :: Symbol -> Symbol -> SymbolList -> Type

class WorkletUrls :: forall k1 . Row k1 -> Row Type -> Constraint
class WorkletUrls spec urls where
  toUrlArray :: Proxy spec -> {|urls}  -> Array String

class HasAllKeys :: forall k1 k2. RowList k1 -> RowList k2 -> Constraint
class HasAllKeys needles haystack

instance hasAllKeysNil :: HasAllKeys Nil a

instance hasAllKeysCons ::
  ( Compare s0 s1 Ordering.EQ
  , HasAllKeys tail0 tail1
  ) =>
  HasAllKeys (Cons s0 _0 tail0) (Cons s1 _1 tail1)

class IsEq (o :: Ordering.Ordering) (b :: Boolean) | o -> b

instance isEqEq :: IsEq Ordering.EQ True
else instance isEqOther :: IsEq a False

class HasASymbol :: forall k. Symbol -> RowList k -> Boolean -> Constraint
class HasASymbol needle haystack b | needle haystack -> b

instance hasASymbolsNil :: HasASymbol s Nil False

instance hasASymbolCons ::
  ( Compare s h e
  , IsEq e b0
  , HasASymbol s tail b1
  , Or b0 b1 b
  ) =>
  HasASymbol s (Cons h _0 tail) b

class HasAllSymbols :: forall k. SymbolList -> RowList k -> Boolean -> Constraint
class HasAllSymbols needles haystack b | needles haystack -> b

instance hasAllSymbolsNil :: HasAllSymbols NilSymbol a True

instance hasAllSymbolsCons ::
  ( HasASymbol s0 haystack b0
  , HasAllSymbols tail0 haystack b1
  , And b0 b1 b
  ) =>
  HasAllSymbols (ConsSymbol s0 tail0) haystack b

instance workletUrls ::
  ( RowToList spec specAsList
  , RowToList urls urlsAsList
  , HasAllKeys specAsList urlsAsList
  , Homogeneous urls String
  ) =>
  WorkletUrls spec urls where
  toUrlArray _ = map snd <<< O.toUnfoldable <<< fromHomogeneous

class HasAudioWorklet (env :: Row Type) (s :: Symbol) (params :: Row Type)

instance hasAudioWorkletProcessor ::
  ( Cons s (SymbolListProxy slist) _1 worklets
  , RowToList params paramsAsList
  , HasAllSymbols slist paramsAsList True
  ) =>
  HasAudioWorklet worklets s params

tAudioWorkletGenerator ::
  forall (env :: Row Type) (s :: Symbol) (params :: Row Type).
  HasAudioWorklet env s params =>
  Homogeneous params Number =>
  IsSymbol s =>
  Proxy env ->
  Proxy s ->
  (Record params) ->
  AudioUnit D1
tAudioWorkletGenerator _ s p = audioWorkletGenerator (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletGenerator_ ::
  forall (env :: Row Type) (s :: Symbol) (params :: Row Type).
  HasAudioWorklet env s params =>
  Homogeneous params Number =>
  IsSymbol s =>
  String ->
  Proxy env ->
  Proxy s ->
  (Record params) ->
  AudioUnit D1
tAudioWorkletGenerator_ n _ s p = audioWorkletGenerator_ n (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletGeneratorT ::
  forall (env :: Row Type) (s :: Symbol) (params :: Row Type).
  HasAudioWorklet env s params =>
  Homogeneous params AudioParameter =>
  IsSymbol s =>
  Proxy env ->
  Proxy s ->
  (Record params) ->
  AudioUnit D1
tAudioWorkletGeneratorT _ s p = audioWorkletGeneratorT (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletGeneratorT_ ::
  forall (env :: Row Type) (s :: Symbol) (params :: Row Type).
  HasAudioWorklet env s params =>
  Homogeneous params AudioParameter =>
  IsSymbol s =>
  String ->
  Proxy env ->
  Proxy s ->
  (Record params) ->
  AudioUnit D1
tAudioWorkletGeneratorT_ n _ s p = audioWorkletGeneratorT_ n (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletProcessor ::
  forall (env :: Row Type) (s :: Symbol) (params :: Row Type) ch.
  Pos ch =>
  HasAudioWorklet env s params =>
  Homogeneous params Number =>
  IsSymbol s =>
  Proxy env ->
  Proxy s ->
  (Record params) ->
  AudioUnit ch ->
  AudioUnit ch
tAudioWorkletProcessor _ s p = audioWorkletProcessor (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletProcessor_ ::
  forall (env :: Row Type) (s :: Symbol) (params :: Row Type) ch.
  Pos ch =>
  HasAudioWorklet env s params =>
  Homogeneous params Number =>
  IsSymbol s =>
  String ->
  Proxy env ->
  Proxy s ->
  (Record params) ->
  AudioUnit ch ->
  AudioUnit ch
tAudioWorkletProcessor_ n _ s p = audioWorkletProcessor_ n (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletProcessorT ::
  forall (env :: Row Type) (s :: Symbol) (params :: Row Type) ch.
  Pos ch =>
  HasAudioWorklet env s params =>
  Homogeneous params AudioParameter =>
  IsSymbol s =>
  Proxy env ->
  Proxy s ->
  (Record params) ->
  AudioUnit ch ->
  AudioUnit ch
tAudioWorkletProcessorT _ s p = audioWorkletProcessorT (reflectSymbol s) (fromHomogeneous p)

tAudioWorkletProcessorT_ ::
  forall (env :: Row Type) (s :: Symbol) (params :: Row Type) ch.
  Pos ch =>
  HasAudioWorklet env s params =>
  Homogeneous params AudioParameter =>
  IsSymbol s =>
  String ->
  Proxy env ->
  Proxy s ->
  (Record params) ->
  AudioUnit ch ->
  AudioUnit ch
tAudioWorkletProcessorT_ n _ s p = audioWorkletProcessorT_ n (reflectSymbol s) (fromHomogeneous p)
