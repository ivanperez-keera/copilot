-- | Auxiliary helper functions to generate C99 code.
module Copilot.Compile.C99.Util
    ( FunEnv
    , funcall
    , statetell
    , streamaccessorname
    , excpyname
    , streamname
    , indexname
    , generatorname
    , guardname
    , argnames
    )
  where

import Control.Monad.State

import Copilot.Core  (Id)
import qualified Language.C99.Simple.AST as C

-- | Auxiliary type used to collect all the declarations of all the variables
-- used in a function to be generated, since variable declarations are always
-- listed first at the top of the function body.
type FunEnv = [C.Decln]

-- | `tell` equivalent for `State`.
statetell :: Monoid m => m -> State m ()
statetell m = modify ((flip mappend) m)

-- | Turn a stream id into a suitable C variable name.
streamname :: Id -> String
streamname sid = "s" ++ show sid

-- | Turn a stream id into the global varname for indices.
indexname :: Id -> String
indexname sid = streamname sid ++ "_idx"

-- | Turn a stream id into the name of its accessor function
streamaccessorname :: Id -> String
streamaccessorname sid = streamname sid ++ "_get"

-- | Add a postfix for copies of external variables the name.
excpyname :: String -> String
excpyname name = name ++ "_cpy"

-- | Turn stream id into name of its generator function.
generatorname :: Id -> String
generatorname sid = streamname sid ++ "_gen"

-- | Turn the name of a trigger into a guard generator.
guardname :: String -> String
guardname name = name ++ "_guard"

-- | Turn a trigger name into a an trigger argument name.
argname :: String -> Int -> String
argname name n = name ++ "_arg" ++ show n

-- | Turn a handler function name into a name for a temporary variable for a
-- handler argument.
argTempName :: String -> Int -> String
argTempName name n = name ++ "_arg_temp" ++ show n

-- | Enumerate all argument names based on trigger name.
argnames :: String -> [String]
argnames base = [aname | n <- [0..], let aname = argname base n]

-- | Enumerate all temporary variable names based on handler function name.
argTempNames :: String -> [String]
argTempNames base = map (argTempName base) [0..]

-- | Define a C expression that calls a function with arguments.
funcall :: C.Ident -> [C.Expr] -> C.Expr
funcall name args = C.Funcall (C.Ident name) args
