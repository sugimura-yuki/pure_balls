module Tetoris.Mino where

import Prelude
import Data.Int (toNumber)
import Graphics.Canvas (Rectangle)

data MinoType
  = T
  | S
  | O
  | J
  | L
  | I

type Mino
  = { x :: Int
    , y :: Int
    , type :: MinoType
    }

toRectangles :: Mino -> Number -> Array Rectangle
toRectangles mino size =
  let
    base :: Mino -> Rectangle
    base m = { x: toNumber m.x * size, y: toNumber m.y * size, width: size, height: size }
  in
    case mino.type of
      T -> []
      S -> []
      O ->
        [ base mino
        , base mino { x = mino.x + 1 }
        , base mino { y = mino.y + 1 }
        , base mino { x = mino.x + 1, y = mino.y + 1 }
        ]
      J -> []
      L -> []
      I -> []
