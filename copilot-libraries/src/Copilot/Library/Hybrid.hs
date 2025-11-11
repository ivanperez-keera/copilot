module Copilot.Library.Hybrid where

-- | Module to write Copilot properties involving real clocks.

-- | Discrete clock starting at zero.
discreteClock :: Stream Int64
discreteClock = [0] ++ (1 + discreteClock)

-- | Integration using the square rule.
integrate :: Stream Int64
          -> Stream Int64
          -> Stream Int64
integrate clock input = go
  where
    go = dt * x + ([0] ++ go)
    dt = delta clock

-- | Sum all input values.
accumulate :: Stream Int64 -> Stream Int64
accumulate x = x + ([0] ++ go)

-- | Time since the last time a property became true.
timeSince :: Stream Int64
          -> Stream Bool
          -> Stream Int64
timeSince clock input = clock - lastTrue clock input

-- | Last time a property became true.
lastTrue :: Stream Int64
         -> Stream Bool
         -> Stream Int64
lastTrue clock input = go
  where
    go = if input then clock else ([0] ++ go)

-- | Delta between samples.
--
-- Assumes that the first sample starts at zero.
delta :: (Typed a, Num a) => Stream a -> Stream a
delta x = x - ([0] ++ x)
