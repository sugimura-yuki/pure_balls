module Main(main) where

import Prelude

import Ball as Ball
import Control.Apply (lift2)
import Control.Monad.State (State, StateT, execState, execStateT, gets, modify)
import Data.DateTime (diff)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Foldable (for_)
import Data.List.Lazy (drop, length, replicateM, zip, (..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (toUnfoldable)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import FRP.Event as Event
import FRP.Event.Time (interval)
import Graphics.Canvas as Canvas
import Math (pi)
import Simulate (simulate)
import UUID (UUID, genUUID)

main :: Effect Unit
main = Canvas.getCanvasElementById "canvas" >>= case _ of
  Nothing -> log "no canvas found"
  Just canvas -> do
    let
      width = 300.0
      height = 300.0
    Canvas.setCanvasWidth canvas width
    Canvas.setCanvasHeight canvas height
    balls <- execStateT (replicateM 15 createBall) Map.empty
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
  balls :: Map.Map UUID Ball.Ball,
  last :: Instant
}

createBall :: StateT (Map.Map UUID Ball.Ball) Effect Unit
createBall = do
  uuid <- liftEffect genUUID
  ball <- liftEffect Ball.randomBall
  _ <- modify $ Map.insert uuid ball
  pure unit

_update :: {width :: Number , height :: Number} -> Event.Event (Member -> Member)
_update wh = (execState <<< hdl) <$> interval 15 where
  hdl :: Instant -> State Member Unit
  hdl now = do
    -- 前回処理からの経過時間
    Milliseconds dt <- gets \m -> diff (toDateTime now) (toDateTime m.last)
    -- 現在時刻を保持
    _ <- modify \b -> b{last = now}
    -- 反射処理
    _ <- modify \b -> b{balls = (Ball.reflect dt wh) <$> b.balls}
    -- 衝突処理
    _ <- modify \b -> b{balls = execState (collision dt) b.balls}
    -- 移動処理
    _ <- modify \b -> b{balls = (Ball.move dt) <$> b.balls}
    pure unit
    where
    -- すべての組み合わせに対して衝突処理をおこなう
    collision :: Number -> State (Map.Map UUID Ball.Ball) Unit
    collision dt = do
      balls <- gets (toUnfoldable <<< Map.keys)
      for_ (zip (1 .. length balls) balls) \(Tuple n k1) -> 
        for_ (drop n balls) \k2 -> do
          mv1 <- gets (Map.lookup k1)
          mv2 <- gets (Map.lookup k2)
          case lift2 Tuple mv1 mv2 of
            Nothing -> pure unit
            Just (Tuple v1 v2) -> do
              let next = Ball.colllisionWith dt {v1,v2}
              _ <- modify (Map.insert k1 next.v1)
              _ <- modify (Map.insert k2 next.v2)
              pure unit

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

