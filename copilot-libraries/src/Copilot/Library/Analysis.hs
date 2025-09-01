-- | Analyze streams.
module Copilot.Library.Analysis where

-- External imports.
import Prelude hiding ((&&), not)

-- Internal imports.
import Copilot.Language
import Copilot.Language.Spec

-- trivial propName = triviallyTrue || triviallyFalse

-- | Property that captures whether a stream is trivially true, meaning that
-- there is no way to make it false.
triviallyTrue :: Stream Bool -> Prop Universal
triviallyTrue = forAll . not

-- | Property that captures whether a stream is trivially false, meaning that
-- there is no way to make it true.
triviallyFalse :: Stream Bool -> Prop Universal
triviallyFalse = triviallyTrue . not

-- | Property that captures whether a list of streams is incompatible, meaning
-- that there is no way to make all streams true at the same time, ever.
incompatible :: [Stream Bool] -> Prop Universal
incompatible = forAll . not . foldr (&&) true

-- | Property that captures whether a list of requirements leads to an
-- unimplementable system, meaning that there is no way to make a system that
-- will satisfy all properties at all times. Any realization of the system will
-- invariably lead to requirements being violated at some point.
unimplementable :: [Stream Bool] -> Prop Universal
unimplementable = forAll . not . eventuallyPrev . foldr (&&) true
