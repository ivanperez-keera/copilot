{-# LANGUAGE ExistentialQuantification #-}

-- | Represent information about externs needed in the generation of C99 code
-- for stream declarations and triggers.
module Copilot.Compile.C99.External where

import Data.List  (unionBy)

import Copilot.Core
import Copilot.Compile.C99.Util (excpyname)

-- | Representation of external variables.
data External = forall a. External
  { extname    :: String
  , extcpyname :: String
  , exttype    :: Type a
  }

-- | Union over lists of External, we solely base the equality on the
-- extname's.
extunion :: [External] -> [External] -> [External]
extunion = unionBy (\a b -> extname a == extname b)

-- | Collect all external variables from the streams and triggers.
--
-- Although Copilot specifications can contain also properties and theorems,
-- the C99 backend currently only generates code for streams and triggers.
gatherexts :: [Stream] -> [Trigger] -> [External]
gatherexts streams triggers = streamsexts `extunion` triggersexts
  where
    streamsexts  = foldr (extunion . streamexts)  mempty streams
    triggersexts = foldr (extunion . triggerexts) mempty triggers

    streamexts :: Stream -> [External]
    streamexts (Stream _ _ expr _) = exprexts expr

    triggerexts :: Trigger -> [External]
    triggerexts (Trigger _ guard args) = guardexts `extunion` argexts
      where
        guardexts = exprexts guard
        argexts   = concatMap uexprexts args

    uexprexts :: UExpr -> [External]
    uexprexts (UExpr _ expr) = exprexts expr

    exprexts :: Expr a -> [External]
    exprexts (Local _ _ _ e1 e2)   = exprexts e1 `extunion` exprexts e2
    exprexts (ExternVar ty name _) = [External name (excpyname name) ty]
    exprexts (Op1 _ e)             = exprexts e
    exprexts (Op2 _ e1 e2)         = exprexts e1 `extunion` exprexts e2
    exprexts (Op3 _ e1 e2 e3)      = exprexts e1 `extunion` exprexts e2
                                       `extunion` exprexts e3
    exprexts (Label _ _ e)         = exprexts e
    exprexts _                     = []
