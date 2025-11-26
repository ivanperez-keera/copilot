{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)

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

  -- Data. Manually converted to a struct
  trigger "h" true [ arg streamOfData ]

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

data MyType = MyType Int8 Bool
  deriving (Eq, Show)

data MyTypeS = MyTypeS
  { f1 :: Field "f1" Int8
  , f2 :: Field "f2" Bool
  }
  deriving Generic

-- | `Struct` instance for `Volts`.
instance Struct MyTypeS where
  typeName = typeNameDefault
  toValues = toValuesDefault
  -- Note that we do not implement `updateField` here. `updateField` is only
  -- needed to make updates to structs work in the Copilot interpreter, and we
  -- do not use the interpreter in this example. (See
  -- `examples/StructsUpdateField.hs` for an example that does implement
  -- `updateField`.)

-- | `Volts` instance for `Typed`.
instance Typed MyTypeS where
  typeOf = typeOfDefault

instance Typed MyType where
  typeOf = Wrapper (Struct (MyTypeS (Field 0) (Field False)))

instance Wraps MyType MyTypeS where
  unwrap (MyType x y) = MyTypeS (Field x) (Field y)

streamOfData :: Stream MyType
streamOfData = [ MyType 8 False, MyType 9 True ] ++ streamOfData
