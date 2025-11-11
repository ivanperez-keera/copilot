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
-- As an example, we follow the structure of:
-- Malvin et al. "EARS (Easy Approach to Requirements Syntax)", 2009.
--
-- For an alternative representation, see the file `SNL.hs`.
import Copilot.Core.Type
import Copilot.Language
import Prelude          hiding ((>))

when :: Stream Bool -> Stream Bool -> Stream Bool
-- when condition s = ifthenElse condition s true
when = (==>)

system :: Stream a -> Stream a
system = id

autoMode :: Stream Bool
autoMode = undefined

always :: Stream Bool -> Stream Bool
always = undefined

shall :: Stream a -> Stream a
shall = id

satisfy :: Stream a -> Stream a
satisfy = id

x :: Stream Int16
x = undefined

y :: Stream Int16
y = undefined

type Requirement = Stream Bool

req :: Requirement
req = when autopilotMode (system (shall (always (satisfy (x > y)))))

u = l
  where
    l = UType { uTypeType = Int8 }
