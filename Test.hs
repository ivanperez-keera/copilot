{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import FieldProps

data Person = Person { name :: String, age :: Int }
            | Anonymous { nickname :: String }
  deriving (Show, Eq)

instance Arbitrary Person where
  arbitrary = oneof [ Person <$> arbitrary <*> arbitrary
                    , Anonymous <$> arbitrary
                    ]

$(deriveRecordFieldProps ''Person)

main = quickCheck prop_fields_Person
