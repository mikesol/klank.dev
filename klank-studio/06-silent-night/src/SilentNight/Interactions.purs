module Klank.Studio.SilentNight.Interactions where

import Prelude
import Data.Array as A
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (wrap)
import Data.String (Pattern(..))
import Data.String as Str
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Event (Event, makeEvent, subscribe)
import Graphics.Painting (Point)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Navigator (userAgent)
import Web.HTML.Window (navigator, toEventTarget)
import Web.TouchEvent.Touch as T
import Web.TouchEvent.TouchEvent (TouchEvent, changedTouches, fromEvent)
import Web.TouchEvent.TouchList as TL
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

newtype Interactions
  = Interactions
  { interactions :: Ref.Ref (InteractionOnsets)
  , nInteractions :: Ref.Ref Int
  , referencePosition :: Ref.Ref (Maybe Point)
  , dispose :: Effect Unit
  }

type InteractionOnsets
  = Array
      { id :: Int
      , x :: Number
      , y :: Number
      }

handleTE :: Int -> Ref.Ref (InteractionOnsets) -> Ref.Ref (Maybe Point) -> TouchEvent -> Effect Unit
handleTE i ref pr te = do
  let
    ts = changedTouches te
  let
    l = TL.length ts
  let
    tlist = map (\t -> { id: i, x: toNumber $ T.clientX t, y: toNumber $ T.clientY t }) (A.catMaybes $ map (\x -> TL.item x ts) (A.range 0 (l - 1)))
  Ref.write (map (\{ x, y } -> { x, y }) (A.head tlist)) pr
  void $ Ref.modify (\ipt -> tlist <> ipt) ref

handleME :: Int -> Ref.Ref (InteractionOnsets) -> Ref.Ref (Maybe Point) -> MouseEvent -> Effect Unit
handleME id ref pr me = do
  let
    x = toNumber $ ME.clientX me
  let
    y = toNumber $ ME.clientY me
  Ref.write (Just { x, y }) pr
  void $ Ref.modify (\ipt -> [ { id, x, y } ] <> ipt) ref

handleTM :: Ref.Ref (Maybe Point) -> TouchEvent -> Effect Unit
handleTM pr te = do
  let
    ts = changedTouches te
  let
    l = TL.length ts
  let
    tlist = map (\t -> { x: toNumber $ T.clientX t, y: toNumber $ T.clientY t }) (A.catMaybes $ map (\x -> TL.item x ts) (A.range 0 (l - 1)))
  Ref.write (map (\{ x, y } -> { x, y }) (A.head tlist)) pr

handleMM :: Ref.Ref (Maybe Point) -> MouseEvent -> Effect Unit
handleMM pr me = do
  let
    x = toNumber $ ME.clientX me
  let
    y = toNumber $ ME.clientY me
  Ref.write (Just { x, y }) pr

getInteractivity :: Effect Interactions
getInteractivity = do
  w <- window
  nav <- navigator w
  ua <- userAgent nav
  let
    mobile = isJust (Str.indexOf (Pattern "iPhone") ua) || isJust (Str.indexOf (Pattern "iPad") ua) || isJust (Str.indexOf (Pattern "Android") ua)
  nInteractions <- Ref.new 0
  referencePosition <- Ref.new Nothing
  totalInteractions <- Ref.new 0
  interactions <- Ref.new []
  target <- toEventTarget <$> window
  touchStartListener <-
    eventListener \e -> do
      fromEvent e
        # traverse_ \me -> do
            void $ Ref.modify (_ + 1) nInteractions
            nt <- Ref.modify (_ + 1) totalInteractions
            handleTE nt interactions referencePosition me
  touchMoveListener <-
    eventListener \e -> do
      fromEvent e
        # traverse_ \me -> do
            handleTM referencePosition me
  touchEndListener <-
    eventListener \e -> do
      fromEvent e
        # traverse_ \me -> do
            void $ Ref.modify (_ - 1) nInteractions
  mouseDownListener <-
    eventListener \e -> do
      ME.fromEvent e
        # traverse_ \me -> do
            void $ Ref.modify (_ + 1) nInteractions
            nt <- Ref.modify (_ + 1) totalInteractions
            handleME nt interactions referencePosition me
  mouseMoveListener <-
    eventListener \e -> do
      ME.fromEvent e
        # traverse_ \me -> do
            handleMM referencePosition me
  mouseUpListener <-
    eventListener \e -> do
      ME.fromEvent e
        # traverse_ \me -> do
            void $ Ref.modify (_ - 1) nInteractions
  if mobile then do
    addEventListener (wrap "touchstart") touchStartListener false target
    addEventListener (wrap "touchmove") touchMoveListener false target
    addEventListener (wrap "touchend") touchEndListener false target
  else do
    addEventListener (wrap "mousedown") mouseDownListener false target
    addEventListener (wrap "mousemove") mouseMoveListener false target
    addEventListener (wrap "mouseup") mouseUpListener false target
  let
    dispose =
      if mobile then do
        removeEventListener (wrap "touchstart") touchStartListener false target
        removeEventListener (wrap "touchmove") touchMoveListener false target
        removeEventListener (wrap "touchend") touchEndListener false target
      else do
        removeEventListener (wrap "mousedown") mouseDownListener false target
        removeEventListener (wrap "mousemove") mouseMoveListener false target
        removeEventListener (wrap "mouseup") mouseUpListener false target
  pure (Interactions { interactions, referencePosition, nInteractions, dispose })

withInteractions ::
  forall a.
  Interactions ->
  Event a ->
  Event { value :: a, interactions :: InteractionOnsets, nInteractions :: Int, referencePosition :: Maybe Point }
withInteractions (Interactions { interactions, nInteractions, referencePosition }) e =
  makeEvent \k ->
    e
      `subscribe`
        \value -> do
          interactionsValue <- Ref.read interactions
          nInteractionsValue <- Ref.read nInteractions
          referencePositionValue <- Ref.read referencePosition
          k { value, interactions: interactionsValue, nInteractions: nInteractionsValue, referencePosition: referencePositionValue }

interactionLog :: Interactions -> Behavior ({ interactions :: InteractionOnsets, nInteractions :: Int, referencePosition :: Maybe Point })
interactionLog m = behavior \e -> map (\{ value, interactions, nInteractions, referencePosition } -> value { interactions, nInteractions, referencePosition }) (withInteractions m e)
