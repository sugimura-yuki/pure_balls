module Main(main) where

import Prelude

import Control.Monad.State (State, StateT, execState, execStateT, get, modify)
import Data.DateTime (diff)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Foldable (foldl, for_)
import Data.Int (floor)
import Data.List.Lazy (drop, length, replicateM, singleton, zip, (..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Effect.Random (random)
import Graphics.Canvas as Canvas
import Math (abs, pi)
import Simulate (simulate)
import Vector (Vec, vAdd, vDirection, vDistance, vDot, vScale, vSub)

main :: Effect Unit
main = Canvas.getCanvasElementById "canvas" >>= case _ of
  Nothing -> log "no canvas found"
  Just canvas -> do
    let
      width = 300.0
      height = 300.0
    Canvas.setCanvasWidth canvas width
    Canvas.setCanvasHeight canvas height
    balls <- execStateT (replicateM 3 createBall) Map.empty
    last <- now
    _ <- simulate {
      init : {balls,last},
      update : _update {width,height},
      render : _render canvas,
      events: []
    }
    pure unit

type UUID = String

type Ball = {
  pos :: Vec,
  move :: Vec,
  radius :: Number,
  color :: String
}

type Member = {
  balls :: Map.Map UUID Ball,
  last :: Instant
}

createBall :: StateT (Map.Map UUID Ball) Effect Unit
createBall = do
  uuid <- liftEffect genUUID
  ball <- liftEffect randomBall
  _ <- modify $ Map.insert uuid ball
  pure unit

genUUID :: Effect UUID
genUUID = do
  x1 <- replicateM 8 x
  x2 <- replicateM 4 x
  x3 <- replicateM 3 x
  z <- (toHex <<< floor) <$> (_ + 8.0) <$> (_ * 4.0) <$> random
  x4 <- replicateM 3 x
  x5 <- replicateM 12 x
  pure $ foldl (<>) "" (x1 <> singleton "-" <> x2 <> singleton "-4" <> x3 <> singleton "-" <> singleton z <> x4 <> singleton "-" <> x5)
  where
    x :: Effect String
    x = (toHex <<< floor) <$> (_ * 16.0) <$> random
    toHex :: Int -> String
    toHex a | a < 10 = show a
    toHex 10 = "a"
    toHex 11 = "b"
    toHex 12 = "c"
    toHex 13 = "d"
    toHex 14 = "e"
    toHex 15 = "f"
    toHex _ = ""

randomColor :: Effect String
randomColor = do
  r <- (_ * 255.0) <$> random
  g <- (_ * 255.0) <$> random
  b <- (_ * 255.0) <$> random
  a <- (\x -> 0.5 * x + 0.5 ) <$> random
  pure $ "rgba(" <> show r <> "," <> show g <> "," <> show b <> "," <> show a <> ")"

randomBall :: Effect Ball
randomBall = do
  x <- (_ + 150.0) <$> (_ * 100.0) <$> random
  y <- (_ + 150.0) <$> (_ * 100.0) <$> random
  radius <- (_ + 10.0) <$> (_ * 30.0) <$> random
  vx <- (_ / 1000.0) <$> (_ - 200.0) <$> (_ * 400.0) <$> random
  vy <- (_ / 1000.0) <$> (_ - 200.0) <$> (_ * 400.0) <$> random
  color <- randomColor
  pure {
    pos:{x,y},
    radius:30.0,
    move:{x:vx,y:vy},
    color
  }

_update :: {width :: Number , height :: Number} -> Instant -> Member -> Member
_update {width, height} now m = execState hdl m where
  Milliseconds dt = diff (toDateTime now) (toDateTime m.last)
  
  hdl :: State Member Unit
  hdl = do
    _ <- modify \b -> b{last = now}
    _ <- modify \b -> b{balls = move <$> b.balls}
    _ <- modify \b -> b{balls = reflect <$> b.balls}
    _ <- modify \b -> b{balls = execState collision b.balls}
    pure unit

  -- | 移動
  move :: Ball -> Ball
  move b = b{pos = vAdd b.pos (vScale dt b.move)}
  
  -- | 反射
  reflect :: Ball -> Ball
  reflect b | b.pos.x - b.radius <= 0.0    = b{pos{x = b.radius}         ,move{x = abs b.move.x}}
            | b.pos.x + b.radius >= width  = b{pos{x = width - b.radius} ,move{x = negate (abs b.move.x)}}
            | b.pos.y - b.radius <= 0.0    = b{pos{y = b.radius}         ,move{y = abs b.move.y}}
            | b.pos.y + b.radius >= height = b{pos{y = height - b.radius},move{y = negate (abs b.move.y)}}
            | otherwise = b

  collision :: State (Map.Map UUID Ball) Unit
  collision = do
    balls <- Map.toUnfoldable <$> get
    for_ (zip (1 .. length balls) balls) \(Tuple n (Tuple k1 v1)) -> 
      for_ (drop n balls) \(Tuple k2 v2) -> do
        let
          dist = vDistance v1.pos v2.pos
          next = vDistance (vAdd v1.pos v1.move) (vAdd v2.pos v2.move)
        when (dist < v1.radius + v2.radius && next < dist) do
          let
            -- １から２に対する力
            d = vDirection v1.pos v2.pos
            p = (vDot v1.move d) - (vDot v2.move d)
            pd = vScale p d
          -- 衝突時のベクトル計算
          _ <- modify $ Map.insert k1 v1{move = v1.move `vSub` pd}
          _ <- modify $ Map.insert k2 v2{move = v2.move `vAdd` pd}
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

