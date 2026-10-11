{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Vector manipulation in Copilot.
module Copilot.Library.Vector where

-- External imports.
import Data.Proxy   ( Proxy (..) )
import GHC.TypeLits ( KnownNat, natVal )
import Prelude      hiding ( not, sqrt, (!!) )

-- Internal imports.
import Copilot.Language                 ( Array, Stream, Typed, Word32, array,
                                          arrayElems, constant, sqrt )
import Copilot.Language.Operators.Array ( (!!), (!), (=:) )

magnitude :: (Eq a, Num a, Typed a, Floating a) => Stream (Array 3 a) -> Stream a
magnitude v = sqrt (x * x + y * y + z * z)
  where
    x = v ! 0
    y = v ! 1
    z = v ! 2

arrayS :: forall a n . (KnownNat n, Num a, Typed a)
       => [Stream a] -> Stream (Array n a)
arrayS = makeA 0 . array

vector3 :: (Num a, Typed a)
        => Stream a
        -> Stream a
        -> Stream a
        -> Stream (Array 3 a)
vector3 x y z = arrayS [x, y, z]

makeA :: forall a n . (KnownNat n, Typed a) => a -> Array n (Stream a) -> Stream (Array n a)
makeA def xs = foldl updatePos initialArray indexedElems
  where
    updatePos :: Stream (Array n a) -> (Word32, Stream a) -> Stream (Array n a)
    updatePos s (i, x) = s !! constant i =: x

    indexedElems :: [(Word32, Stream a)]
    indexedElems = zip [0..] (arrayElems xs)

    initialArray :: Stream (Array n a)
    initialArray = constant $ array (replicate n def)

    n = fromIntegral $ natVal (Proxy :: Proxy n)

crossProduct :: (Eq a, Num a, Typed a, Floating a)
             => Stream (Array 3 a)
             -> Stream (Array 3 a)
             -> Stream (Array 3 a)
crossProduct a b = arrayS
    [ ay * bz - az * by
    , az * bx - ax * bz
    , ax * by - ay * bx
    ]
  where
    ax = a ! 0
    ay = a ! 1
    az = a ! 2

    bx = b ! 0
    by = b ! 1
    bz = b ! 2

(^-^) :: (Eq a, Num a, Typed a, Floating a)
      => Stream (Array 3 a)
      -> Stream (Array 3 a)
      -> Stream (Array 3 a)
(^-^) a b = arrayS
    [ ax - bx
    , ay - by
    , az - bz
    ]
  where
    ax = a ! 0
    ay = a ! 1
    az = a ! 2

    bx = b ! 0
    by = b ! 1
    bz = b ! 2
