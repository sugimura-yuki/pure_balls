module Vector where

import Prelude
import Math as Math

type Vec
  = { x :: Number, y :: Number }

vAdd :: Vec -> Vec -> Vec
vAdd a b = { x: a.x + b.x, y: a.y + b.y }

vSub :: Vec -> Vec -> Vec
vSub a b = { x: a.x - b.x, y: a.y - b.y }

vLength :: Vec -> Number
vLength a = Math.sqrt $ Math.pow a.x 2.0 + Math.pow a.y 2.0

vNormalize :: Vec -> Vec
vNormalize a = let l = vLength a in { x: a.x / l, y: a.y / l }

vDirection :: Vec -> Vec -> Vec
vDirection a b = vNormalize $ vSub a b

vDistance :: Vec -> Vec -> Number
vDistance a b = vLength $ vSub a b

vScale :: Number -> Vec -> Vec
vScale n a = { x: n * a.x, y: n * a.y }

vDot :: Vec -> Vec -> Number
vDot a b = a.x * b.x + a.y * b.y
