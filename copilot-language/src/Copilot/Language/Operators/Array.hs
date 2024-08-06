{-# LANGUAGE Safe         #-}

{-# LANGUAGE TypeFamilies #-}

-- | Combinators to deal with streams carrying arrays.
module Copilot.Language.Operators.Array
  ( setArray
  , (.!!)
  ) where

import Copilot.Core             ( Typed
                                , Op2 (Index)
                                , Op3 (SetArray)
                                , typeOf
                                , Array
                                )
import Copilot.Language.Stream  (Stream (..))

import Data.Word                (Word32)
import GHC.TypeLits             (KnownNat)

-- | Create a stream that carries an element of an array in another stream.
--
-- This function implements a projection of the element of an array at a given
-- position, over time. For example, if @s@ is a stream of type @Stream (Array
-- '5 Word8)@, then @s .!! 3@ has type @Stream Word8@ and contains the 3rd
-- element (starting from zero) of the arrays in @s@ at any point in time.
(.!!) :: ( KnownNat n
         , Typed t
         ) => Stream (Array n t) -> Stream Word32 -> Stream t
arr .!! n = Op2 (Index typeOf) arr n

-- | Update a stream of arrays.
setArray :: ( KnownNat n
            , Typed t
            )
         => Stream (Array n t)
         -> Stream Word32
         -> Stream t
         -> Stream (Array n t)
setArray arr n v = Op3 (SetArray typeOf) arr n v
