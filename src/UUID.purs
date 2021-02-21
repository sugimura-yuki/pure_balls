module UUID where

import Prelude

import Data.Foldable (foldl)
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Random (randomInt)

type UUID = String

genUUID :: Effect UUID
genUUID = do
  x1 <- replicateA 8 x
  x2 <- replicateA 4 x
  x3 <- replicateA 3 x
  z <- toHex <$> randomInt 3 11
  x4 <- replicateA 3 x
  x5 <- replicateA 12 x
  pure $ foldl (<>) "" (x1 <> ["-"] <> x2 <> ["-4"] <> x3 <> ["-"] <> [z] <> x4 <> ["-"] <> x5)
  where
    x :: Effect String
    x = toHex <$> randomInt 0 15
    toHex :: Int -> String
    toHex a | a < 10 = show a
    toHex 10 = "a"
    toHex 11 = "b"
    toHex 12 = "c"
    toHex 13 = "d"
    toHex 14 = "e"
    toHex 15 = "f"
    toHex _ = ""
