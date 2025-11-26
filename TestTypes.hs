{-# LANGUAGE MultiParamTypeClasses #-}

-- External imports
import Prelude hiding (all, mod, not, or, until, (&&), (++), (<), (<=), (==),
                       (>), (>=), (||))

-- Internal imports
import Copilot.Compile.C99
import Language.Copilot    (reify)
import Language.Copilot    hiding (all, or)

main :: IO ()
main = do
  f <- reify spec
  compile "c" f

spec :: Spec
spec = do
  -- Enums. Automatically converted to int64
  trigger "f" true [ arg streamOfAB ]

  -- Newtype. Manually converted to inner type.
  trigger "g" true [ arg streamOfMI ]

-- * Test with enums

data A = A | B
  deriving (Eq, Enum, Show)

instance Typed A where
  typeOf = Enum A

streamOfA :: Stream A
streamOfA = constant A

streamOfAB :: Stream A
streamOfAB = [A, B] ++ streamOfAB

-- * Test with newtypes

newtype MyInt = MyInt { u :: Int8 }
 deriving (Eq, Show)

instance Wraps MyInt Int8 where
  unwrap = u

instance Typed MyInt where
  typeOf = Wrapper Int8

streamOfMI :: Stream MyInt
streamOfMI = constant (MyInt 8)
