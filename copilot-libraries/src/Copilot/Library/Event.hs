module Copilot.Library.Event where

-- | True whenever the given stream goes from False to True. False whenever the
-- values repeat or the current value of the given stream is false.
risingEdge :: Stream Bool -> Stream Bool
risingEdge s = s && not lastS
  where
    -- This is just the value of s delayed by one step (initially False).
    lastS = [False] ++ s

-- | Let only the first True through.
once :: Stream Bool -> Stream Bool
once s = g
  where
    g = if s then now else g

now :: Stream Bool -> Stream Bool
now = [True] ++ false

switch :: Stream a -> Stream Bool -> Stream a -> Stream a
switch s1 c s2 = switch' s1 c' s2
  where
    c' = eventuallyPrev c
    switch' s1' c' s2' = if c' then s2 else s1
