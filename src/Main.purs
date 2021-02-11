module Main where

import Prelude

import Control.Monad.State (State, execState, get, modify)
import Data.DateTime (diff)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Foldable (for_)
import Data.List.Lazy (List, replicateM)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Effect.Random (random)
import Graphics.Canvas as Canvas
import Math (abs)
import Math as Math
import Simulate (simulate)

main :: Effect Unit
main = Canvas.getCanvasElementById "canvas" >>= case _ of
  Nothing -> log "no canvas found"
  Just canvas -> do
    let
      width = 300.0
      height = 300.0
    Canvas.setCanvasWidth canvas width
    Canvas.setCanvasHeight canvas height
    balls <- replicateM 100 randomBall
    last <- now
    _ <- simulate {
      init : {balls,last},
      update : update {width,height},
      render : render canvas,
      events: []
    }
    pure unit

type Ball = {x::Number,y::Number,radius :: Number,color :: String,vec :: {x::Number,y::Number}}
type Member = {
  balls :: List Ball,
  last :: Instant
}

randomColor :: Effect String
randomColor = do
  r <- (_ * 255.0) <$> random
  g <- (_ * 255.0) <$> random
  b <- (_ * 255.0) <$> random
  a <- (\x -> 0.5 * x + 0.5 ) <$> random
  pure $ "rgba(" <> show r <> "," <> show g <> "," <> show b <> "," <> show a <> ")"

randomBall :: Effect Ball
randomBall = do
  r <- (_ + 10.0) <$> (_ * 30.0) <$> random
  vx <- (_ / 1000.0) <$> (_ - 200.0) <$> (_ * 400.0) <$> random
  vy <- (_ / 1000.0) <$> (_ - 200.0) <$> (_ * 400.0) <$> random
  color <- randomColor
  pure {x:150.0,y:150.0,radius:r,vec:{x:vx,y:vy},color}

update :: {width :: Number , height :: Number} -> Instant -> Member -> Member
update {width,height} now m = m{balls = balls' , last = now} where
  Milliseconds dt = diff (toDateTime now) (toDateTime m.last)

  balls' :: List Ball
  balls' = map (execState (updateBall dt)) m.balls 
  
  updateBall :: Number -> State Ball Unit
  updateBall dt = do
    -- 移動
    b <- modify \s -> s{
      x = s.x + dt * s.vec.x,
      y = s.y + dt * s.vec.y
    }
    -- 反射処理
    when (b.x <= b.radius) $ void $ modify \s -> s{x = s.radius,vec{x = abs s.vec.x}}
    when (width <= b.x + b.radius) $ void $ modify \s -> s{x = width - s.radius,vec{x = negate abs s.vec.x}}
    -- 上下
    when (b.y <= b.radius) $ void $ modify \s -> s{y = s.radius,vec{y = abs s.vec.y}}
    when (height <= b.y + b.radius) $ void $ modify \s -> s{y = height - s.radius,vec{y = negate abs s.vec.y}}
    pure unit

render :: Canvas.CanvasElement -> Member -> Effect Unit
render canvas m = do
  ctx <- Canvas.getContext2D canvas
  width <- Canvas.getCanvasWidth canvas
  height <- Canvas.getCanvasHeight canvas
  Canvas.clearRect ctx {x: 0.0, y: 0.0, width: width, height: height}
  -- ボールの描画処理
  for_ m.balls \b -> do
    Canvas.setFillStyle ctx b.color
    Canvas.beginPath ctx
    Canvas.arc ctx {
      x: b.x,
      y: b.y,
      radius: b.radius,
      start: 0.0,
      end: 2.0 * Math.pi
    }
    Canvas.fill ctx

