{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Test copilot-core:Copilot.Core.Type.Array.
module Test.Copilot.Core.Type.Array where

-- External imports
import Data.Int                             (Int64)
import Data.Proxy                           (Proxy (..))
import GHC.TypeNats                         (KnownNat, natVal)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Gen, Property, arbitrary,
                                             chooseInt, expectFailure, forAll,
                                             property, vector, vectorOf)

-- Internal imports: library modules being tested
import Copilot.Core.Type.Array (Array, array, arrayElems)

-- | All unit tests for copilot-core:Copilot.Core.Array.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Core.Type.Array"
    [ testProperty "arrayElems . array (identity; 0)"
        (testArrayElemsLeft (Proxy :: Proxy 0))
    , testProperty "arrayElems . array (identity; 5)"
        (testArrayElemsLeft (Proxy :: Proxy 5))
    , testProperty "arrayElems . array (identity; 200)"
        (testArrayElemsLeft (Proxy :: Proxy 200))
    , testProperty "array of incorrect length"
        testArrayElemsFail
    , testProperty "Show for arrays"
        testShowArray
    , testProperty "arrayElems (arrayUpdate x i v) !! i == v"
        testArrayUpdateElem (Proxy :: Proxy 5)
    , testProperty "arrayUpdate (arrayUpdate x i v1) i v2 == arrayUpdate x i v2"
    , testProperty "arrayUpdate x i ((arrayElems x) ! i) == x"
    , testProperty "i1 /= i2 ==> arrayUpdate (arrayUpdate x i1 v1) i2 v2 == arrayUpdate (arrayUpdate x i2 v2) i1 v1"
    , testProperty "arrayUpdate fails if out of range of array"
    ]

-- * Individual tests

-- | Test that building an array from a list and extracting the elements with
-- the function 'arrayElems' will result in the same list.
testArrayElemsLeft :: forall n . KnownNat n => Proxy n -> Property
testArrayElemsLeft len =
    forAll xsInt64 $ \ls ->
      let array' :: Array n Int64
          array' = array ls
      in arrayElems array' == ls

  where

    -- Generator for lists of Int64 of known length.
    xsInt64 :: Gen [Int64]
    xsInt64 = vectorOf (fromIntegral (natVal len)) arbitrary

-- | Test that arrays cannot be properly evaluated if their length does not
-- match their type.
testArrayElemsFail :: Property
testArrayElemsFail = expectFailure $ forAll (vector 3) $ \l ->
  let v = array l :: Array 4 Int64
  in arrayElems v == l

-- | Test show for arrays.
testShowArray :: Property
testShowArray = forAll (vector 3) $ \l -> property $
  show (array l :: Array 3 Int64) == show (l :: [Int64])

-- | Test that updating an array updates the element appropriately (if we
-- project that element we get the value we put in).
testArrayUpdateElem :: forall n . KnownNat n => Proxy n -> Property
testArrayUpdateElem len =
    forAll xsInt64  $ \ls ->
    forAll position $ \p ->
    forAll xInt64   $ \v ->
      let -- Original array
          array' :: Array n Int64
          array' = array ls

          -- Updated array
          array'' :: Array n Int64
          array'' = arrayUpdate array' p v

      in arrayElems array'' !! p == v

  where

    -- Generator for lists of Int64 of known length.
    xsInt64 :: Gen [Int64]
    xsInt64 = vectorOf (fromIntegral (natVal len)) arbitrary

    -- Generator for element of type Int64.
    xInt64 :: Gen Int64
    xInt64 = arbitrary

    -- Generator for positions within the list.
    position :: Gen Int
    position = chooseInt (0, fromIntegral (natVal len) - 1)
