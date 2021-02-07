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
