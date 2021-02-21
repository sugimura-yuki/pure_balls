module Ball where

import Prelude

import Effect (Effect)
import Effect.Random (random)
import Math as Math
import Vector (Vec, vAdd, vDirection, vDistance, vDot, vScale, vSub)

type Ball = {
  pos :: Vec,
  move :: Vec,
  radius :: Number,
  color :: String
}
-- | 移動
move :: Number -> Ball -> Ball
move dt b = b{pos = vAdd b.pos (vScale dt b.move)}

-- | 反射
reflect :: Number -> {width :: Number , height :: Number} -> Ball -> Ball
reflect dt {width,height} b = let next = (move dt b).pos in
  -- 左
  if next.x - b.radius <= 0.0 then
    b{
      pos{x = b.radius},
      move{x = Math.abs b.move.x}
    }
  -- 右
  else if next.x + b.radius >= width then
    b{
      pos{x = width - b.radius},
      move{x = negate (Math.abs b.move.x)}
    }
  -- 上
  else if next.y - b.radius <= 0.0 then
    b{
      pos{y = b.radius},
      move{y = Math.abs b.move.y}
    }
  -- 下
  else if next.y + b.radius >= height then
    b{
      pos{y = height - b.radius},
      move{y = negate (Math.abs b.move.y)}
    }
  else b

-- | 衝突
-- http://marupeke296.com/COL_MV_No1_HowToCalcVelocity.html
colllisionWith :: Number -> {v1::Ball,v2::Ball} -> {v1::Ball,v2::Ball}
colllisionWith dt {v1, v2} =
  if dist >= next && next <= v1.radius + v2.radius then
    {
      v1: v1{move = vAdd v1.move (vScale p1 c)},
      v2: v2{move = vAdd v2.move (vScale p2 c)}
    }
  else
    {v1, v2}
  where
    dist = vDistance v1.pos v2.pos
    next = vDistance (move dt v1).pos (move dt v2).pos
    -- それぞれの重さ
    w1 = v1.radius * v1.radius
    w2 = v2.radius * v2.radius
    -- 反発率
    e = 1.0
    -- １から２への向き（標準ベクトル）
    c = vDirection v1.pos v2.pos
    -- １から２への相対速度
    s = vDot (vSub v1.move v2.move) c
    -- 反発後の速度
    p1 = (1.0 + e) * (negate s) * w2 / (w1 + w2)
    p2 = (1.0 + e) * s * w1 / (w1 + w2)

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
  r <- (_ + 5.0) <$> (_ * 30.0) <$> random
  -- let r = 20.0
  vx <- (_ * 0.1) <$> random
  vy <- (_ * 0.1) <$> random
  color <- randomColor
  pure {
    pos: {x: 150.0, y: 150.0},
    radius: r,
    move: {x: vx, y: vy},
    color
  }
