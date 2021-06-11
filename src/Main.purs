module Main(main) where

import Prelude

import Ball (Ball)
import Ball as Ball
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.DateTime (diff)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Foldable (for_)
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Now (now)
import FRP.Event as Event
import FRP.Event.Time (interval)
import Graphics.Canvas as Canvas
import Math (pi)
import Simulate (simulate)

main :: Effect Unit
main = Canvas.getCanvasElementById "canvas" >>= case _ of
  Nothing -> log "no canvas found"
  Just canvas -> do
    let
      width = 500.0
      height = 500.0
    Canvas.setCanvasWidth canvas width
    Canvas.setCanvasHeight canvas height
    balls <- List.replicateM 20 Ball.randomBall
    last <- now
    _ <- simulate {
      init : {balls,last},
      render : _render canvas,
      events: [
        _update {width, height}
      ]
    }
    pure unit


type Member = {
  balls :: List Ball,
  last :: Instant
}

_update :: {width :: Number , height :: Number} -> Event.Event (Member -> Member)
_update wh = ( State.execState <<< hdl) <$> interval 15 where
  hdl :: Instant -> State Member Unit
  hdl now = do
    -- 前回処理からの経過時間
    Milliseconds dt <- State.gets \m -> diff (toDateTime now) (toDateTime m.last)
    -- 現在時刻を保持
    _ <-  State.modify \b -> b{last = now}
    -- 反射処理
    _ <-  State.modify \b -> b{balls = (Ball.reflect dt wh) <$> b.balls}
    -- 衝突処理
    _ <-  State.modify \b -> b{balls = collision dt b.balls}
    -- 移動処理
    _ <-  State.modify \b -> b{balls = (Ball.move dt) <$> b.balls}
    pure unit
    where
      -- すべての組み合わせに対して衝突処理をおこなう
      collision :: Number -> List Ball -> List Ball
      collision dt xs = case List.uncons xs of
        Nothing -> mempty
        Just {head,tail} ->
          let
            fn ys = for ys \v2 -> do
              v1 <- State.get
              let r = Ball.colllisionWith dt {v1,v2}
              State.put r.v1
              pure r.v2
            -- 他のボールと衝突処理後
            Tuple tail' head' = State.runState (fn tail) head
          in
            List.cons head' (collision dt tail')

_render :: Canvas.CanvasElement -> Member -> Effect Unit
_render canvas m = do
  ctx <- Canvas.getContext2D canvas
  width <- Canvas.getCanvasWidth canvas
  height <- Canvas.getCanvasHeight canvas
  Canvas.clearRect ctx {x: 0.0, y: 0.0, width: width, height: height}
  -- ボールの描画処理
  for_ m.balls \b -> do
    Canvas.setFillStyle ctx b.color
    Canvas.beginPath ctx
    Canvas.arc ctx {
      x: b.pos.x,
      y: b.pos.y,
      radius: b.radius,
      start: 0.0,
      end: 2.0 * pi
    }
    Canvas.fill ctx

