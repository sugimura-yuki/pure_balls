module Simulate where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Foldable (for_)
import Effect (Effect)
import FRP.Event (Event, EventIO, create, sampleOn, sampleOn_, subscribe)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Time (withTime)

type SimulateArgs s = {
  render :: s -> Effect Unit,
  init :: s,
  update :: Instant -> s -> s,
  events :: Array (Event (s -> s))
}

simulate :: forall s. SimulateArgs s -> Effect (Effect Unit)
simulate args = do
  -- 受信と送信
  {event: onUpdate, push} <- create :: Effect (EventIO s)

  -- 更新イベント
  for_ args.events
    \event -> subscribe (sampleOn onUpdate event) push

  -- 描画と更新処理
  c <- subscribe (withTime $ sampleOn_ onUpdate animationFrame) \{time,value} -> do
    push (args.update time value)
    args.render value

  -- 初期値送信
  push args.init

  pure c
