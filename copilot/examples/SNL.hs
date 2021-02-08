{-# LANGUAGE CPP #-}
-- | This example demonstrates how it is possible to add new keywords in
-- Copilot, and to create a language that resembles plain language (e.g.,
-- English).
--
-- The goal of this example is two-fold: first, we want to demonstrate that
-- users are not limited by the Copilot language, that it is extensible, and
-- that they can adjust it to fit their needs, and second, we want to
-- demonstrate that users could make properties resemble a language that is
-- most convenient for them.
--
-- It is based on a stragegy of 1) introducing new stream combinators for the
-- keywords in the language, 2) adding CPP define entries to prevent conflicts
-- with the host language, and remove keywords that don't have meaning.
--
-- As an example, we follow the structure of:
-- Malvin et al. "EARS (Easy Approach to Requirements Syntax)", 2009.

import Copilot.Core.Type
import Copilot.Language
import Prelude           hiding ((/=), (<), (>))

-- Language keywords
#define if if_
#define when when_
#define while while_
#define during while_
#define where where_
#define is &
#define shall
#define satisfy
#define the
#define then

-- Components
#define system

-- * Language keywords
when_ :: Stream Bool -> Stream Bool -> Stream Bool
when_ = (==>)
  -- See also edge to treat 'when' as a trigger
  -- where
  --   -- True when a stream goes from false to true.
  --   edge stream = stream && not previousStream
  --     where
  --       previousStream = [False] ++ stream

if_ :: Stream Bool -> Stream Bool -> Stream Bool
if_ = (==>)

while_ :: Stream Bool -> Stream Bool -> Stream Bool
while_ = (==>)

where_ :: Stream Bool -> Stream Bool -> Stream Bool
where_ = (==>)

(&) = flip ($)

-- * Examples

-- ** Ubiquotous requirements

req1 :: Stream Bool
req1 = the system shall satisfy (speed < 10)

-- ** Unwanted behaviors

req2 :: Stream Bool
req2 = if (auto is engaged) then (the system shall satisfy (speed < 10))

-- ** State-driven requirements

req3 :: Stream Bool
req3 = while (auto is engaged) (the system shall satisfy (speed < 10))

req4 :: Stream Bool
req4 = during takeoff (the system shall satisfy (speed < 10))

--
req5 :: Stream Bool
req5 =
  when moving (if (auto is engaged) (the system shall satisfy (speed < 10)))

-- ** Optional features
req6 :: Stream Bool
req6 =
  where gpsAvailable (the system shall satisfy (speed < 10))

-- * External variables

moving :: Stream Bool
moving = true -- define as extern "moving" Nothing

gpsAvailable :: Stream Bool
gpsAvailable = true -- define as extern "gpsAvailable" Nothing

takeoff :: Stream Bool
takeoff = true -- define as extern "takeoff" Nothing

auto :: Stream Int64
auto = 0

engaged :: Stream Int64 -> Stream Bool
engaged x = x /= 0

speed :: Stream Int64
speed = 10
