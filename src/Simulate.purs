module Simulate where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import FRP.Event (Event, EventIO, create, sampleOn, sampleOn_, subscribe)
import FRP.Event.AnimationFrame (animationFrame)

type SimulateArgs s = {
  render :: s -> Effect Unit,
  init :: s,
  events :: Array (Event (s -> s))
}

simulate :: forall s. SimulateArgs s -> Effect (Effect Unit)
simulate args = do
  -- 受信と送信
  {event: onUpdate, push} <- create :: Effect (EventIO s)

  -- 更新イベント
  for_ args.events
    \event -> subscribe (sampleOn onUpdate event) push

  -- 描画処理
  c <- subscribe (sampleOn_ onUpdate animationFrame) args.render

  -- 初期値送信
  push args.init

  pure c
