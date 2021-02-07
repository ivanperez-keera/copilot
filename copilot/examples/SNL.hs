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

import Control.Monad (filterM)

-- Language keywords
#define if if_
#define when when_
#define shall
#define satisfy
#define the
#define is &

-- Components
#define system

type Stream a = a

-- External variables

moving    = True
auto      = 0
engaged 0 = True
engaged _ = False
speed     = 10

-- Tempoeral operators
(==>) :: Stream Bool -> Stream Bool -> Stream Bool
(==>) False _ = True
(==>) _     x = x

when_ :: Stream Bool -> Stream Bool -> Stream Bool
when_ = (==>)

if_ :: Stream Bool -> Stream Bool -> Stream Bool
if_ = (==>)

(&) = flip ($)

-- Examples
req1 =
  when moving (if (auto is engaged) (the system shall satisfy (speed < 10)))
