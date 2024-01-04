-- | Naming of variables and functions in C.
module Copilot.Compile.C99.Name
    ( argNames
    , argTempNames
    , exCpyName
    , generatorName
    , generatorOutputArgName
    , guardName
    , indexName
    , streamAccessorName
    , streamName
    )
  where

-- External imports: Copilot
import Copilot.Core (Id)

-- | Turn a stream id into a suitable C variable name.
streamName :: Id -> String
streamName sId = "s" ++ show sId

-- | Turn a stream id into the global varname for indices.
indexName :: Id -> String
indexName sId = streamName sId ++ "_idx"

-- | Turn a stream id into the name of its accessor function
streamAccessorName :: Id -> String
streamAccessorName sId = streamName sId ++ "_get"

-- | Add a postfix for copies of external variables the name.
exCpyName :: String -> String
exCpyName name = name ++ "_cpy"

-- | Turn stream id into name of its generator function.
generatorName :: Id -> String
generatorName sId = streamName sId ++ "_gen"

-- | Turn stream id into name of its output argument array.
generatorOutputArgName :: Id -> String
generatorOutputArgName sId = streamName sId ++ "_output"

-- | Turn the name of a trigger into a guard generator.
guardName :: String -> String
guardName name = name ++ "_guard"

-- | Turn a trigger name into a trigger argument name.
argName :: String -> Int -> Int -> String
argName name counter n = name ++ "_" ++ show counter ++ "_arg" ++ show n

-- | Turn a handler function name into a name for a temporary variable for a
-- handler argument.
argTempName :: String -> Int -> Int -> String
argTempName name counter n = name ++ "_" ++ show counter ++ "_arg_temp" ++ show n

-- | Enumerate all argument names based on trigger name.
argNames :: String -> Int -> [String]
argNames base counter = map (argName base counter) [0..]

-- | Enumerate all temporary variable names based on handler function name.
argTempNames :: String -> Int -> [String]
argTempNames base counter = map (argTempName base counter) [0..]
