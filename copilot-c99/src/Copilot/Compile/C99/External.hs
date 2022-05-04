{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

-- | Represent information about externs needed in the generation of C99 code
-- for stream declarations and triggers.
module Copilot.Compile.C99.External where

import Data.List     (nubBy)
import Data.Maybe    (catMaybes)
import Data.Typeable (Typeable)

import Copilot.Core
import Copilot.Compile.C99.Util (excpyname)

-- | Representation of external variables.
data External = forall a. External
  { extname    :: String
  , extcpyname :: String
  , exttype    :: Type a
  }

-- | Collect all external variables from the streams and triggers.
--
-- Although Copilot specifications can contain also properties and theorems,
-- the C99 backend currently only generates code for streams and triggers.
gatherexts :: [Stream] -> [Trigger] -> [External]
gatherexts streams triggers
    = nubBy (\a b -> extname a == extname b)
    $ catMaybes
    $ concat
        [ concatMap (flattenStreams exprext) streams
        , concatMap (flattenTriggers exprext) triggers
        ]
  where
    exprext :: Expr a -> Maybe External
    exprext (ExternVar ty name _) = Just (External name (excpyname name) ty)
    exprext _                     = Nothing

-- * Auxiliary code

-- Traverse a stream and apply a function to all expressions inside, gathering
-- the result.
flattenStreams :: (forall c . Typeable c => Expr c -> b)
               -> Stream
               -> [b]
flattenStreams f (Stream _ _ expr _) = flattenExpr f expr

-- Traverse a trigger and apply a function to all expressions inside, gathering
-- the result.
flattenTriggers :: (forall c . Typeable c => Expr c -> b)
                -> Trigger
                -> [b]
flattenTriggers f (Trigger _ guard args) =
    concat [ guardexts, argexts ]
  where
    guardexts = flattenExpr f guard
    argexts   = concatMap (flattenUExpr f) args

-- Traverse a uexpr and apply a function to all expressions inside, gathering
-- the result.
flattenUExpr :: (forall c . Typeable c => Expr c -> b)
             -> UExpr
             -> [b]
flattenUExpr f (UExpr _ expr) = flattenExpr f expr

-- Traverse an expr and apply a function to all expressions inside, gathering
-- the result.
flattenExpr :: Typeable a
            => (forall c . Typeable c => Expr c -> b)
            -> Expr a
            -> [b]
flattenExpr = go
  where
    go :: Typeable a
       => (forall c . Typeable c => Expr c -> b)
       -> Expr a
       -> [b]
    go f e@(Local _ _ _ e1 e2)   = f e : concat [ go f e1, go f e2 ]
    go f e@(ExternVar ty name _) = [ f e ]
    go f e@(Op1 _ e1)            = f e : go f e1
    go f e@(Op2 _ e1 e2)         = f e : concat [ go f e1, go f e2 ]
    go f e@(Op3 _ e1 e2 e3)      = f e : concat [ go f e1, go f e2, go f e3 ]
    go f e@(Label _ _ e1)        = f e : go f e1
    go f e                       = [ f e ]
