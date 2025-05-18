-- | Analyze streams.
module Copilot.Library.Analysis
    ( -- * Analysis of individual streams
      triviallyTrue
    , triviallyFalse
    , incompatible
    , unimplementable

      -- * Analysis of specs
    , trivial
    , consistentSpec
    , inconsistentSpec
    , inconsistent
    , consistent
    , inconsistentSubsets
    )
  where

-- External imports.
import           Control.DeepSeq           (rnf)
import           Control.Exception         (evaluate, finally)
import           Control.Monad             (forM_, void)
import           Control.Monad.Writer.Lazy (runWriter)
import           GHC.IO.Handle             (hDuplicate, hDuplicateTo)
import           Prelude                   hiding (div, not, (&&), (<), (==),
                                            (>))
import qualified Prelude
import           System.IO                 (BufferMode (LineBuffering),
                                            SeekMode (AbsoluteSeek),
                                            hGetContents, hSeek, hSetBuffering,
                                            stdout)
import           System.IO.Temp            (withSystemTempFile)

-- Internal imports.
import           Copilot.Core           hiding (Spec, Stream)
import qualified Copilot.Core           as Core
import           Copilot.Language       (Stream, not, true, (&&))
import           Copilot.Language.Reify (reify)
import           Copilot.Language.Spec  hiding (Exists, Forall, Property,
                                         Trigger)
import qualified Copilot.Language.Spec  as S
import           Copilot.Library.PTLTL  (eventuallyPrev)
import           Copilot.Theorem.Kind2  (def, kind2Prover)
import qualified Copilot.Theorem.Prove  as Prove
import           Copilot.Theorem.What4  (SatResult (Invalid, Unknown, Valid),
                                         Solver (Z3), prove)

-- | Property that captures whether a stream is trivially true, meaning that
-- there is no way to make it false.
triviallyTrue :: Stream Bool -> S.Prop Universal
triviallyTrue = forAll . not

-- | Property that captures whether a stream is trivially false, meaning that
-- there is no way to make it true.
triviallyFalse :: Stream Bool -> S.Prop Universal
triviallyFalse = triviallyTrue . not

-- | Trivial if all triggers in a specification are trivial (trivially true or
-- trivially false).
trivial :: Spec -> IO ()
trivial spec = do
  spec' <- reify spec
  let f t = (triggerName t, triggerGuard t)
  mapM_ (uncurry (trivialExpr spec') . f) $ specTriggers spec'

-- | Trivial if the expression with the given name is either trivially true or
-- trivially false within the spec.
trivialExpr :: Core.Spec -> Name -> Expr Bool -> IO ()
trivialExpr spec name expr = do
  let prop1 = Property ("trivial (" Prelude.++ name Prelude.++ ")") (Forall expr)
  let spec1 = spec { specProperties = prop1 : specProperties spec }

  -- Use Z3 to prove the properties.
  results <- prove Z3 spec1

  -- Print the results.
  forM_ results $ \(nm, res) -> do
    putStr $ nm <> ": "
    case res of
      Valid   -> putStrLn "trivially true"
      Invalid -> putStrLn "not trivially true"
      Unknown -> putStrLn "unknown"

  let prop2 = Property
                ("trivial (" Prelude.++ name Prelude.++ ")")
                (Exists (Op1 Not expr))

  let spec2 = spec { specProperties = prop2 : specProperties spec }

  -- Use Z3 to prove the properties.
  results2 <- prove Z3 spec2

  -- Print the results.
  forM_ results2 $ \(nm, res) -> do
    putStr $ nm <> ": "
    case res of
      Valid   -> putStrLn "trivially false"
      Invalid -> putStrLn "not trivially false"
      Unknown -> putStrLn "unknown"

-- | Property that captures whether a list of streams is incompatible, meaning
-- that there is no way to make all streams true at the same time, ever.
incompatible :: [Stream Bool] -> S.Prop Universal
incompatible = forAll . not . foldr (&&) true

-- | Property that captures whether a list of requirements leads to an
-- unimplementable system, meaning that there is no way to make a system that
-- will satisfy all properties at all times. Any realization of the system will
-- invariably lead to requirements being violated at some point.
unimplementable :: [Stream Bool] -> S.Prop Universal
unimplementable = forAll . not . eventuallyPrev . foldr (&&) true

-- | True if all trigger conditions are consistent.
consistentSpec :: Spec -> IO Bool
consistentSpec spec = do
    r <- detectResult $ void $ reify $ do
      spec
      void $ theorem
               "consistent"
               (exists allTogether)
               (Prove.check (kind2Prover def))
    return (r Prelude.== V)
  where
    allTogether = foldr (&&) true props
    props       = map not guards

    guards = map (\(S.Trigger _ g _) -> g) ts
    ts     = specTriggers' spec

-- | True if trigger conditions are inconsistent.
inconsistentSpec :: Spec -> IO Bool
inconsistentSpec spec = do
  let ts     = specTriggers' spec
      guards = map (\(S.Trigger n g _) -> (n, g)) ts
  inconsistent spec guards

-- | True if the given labelled streams are consistent in the spec.
consistent :: Spec -> [(String, Stream Bool)] -> IO Bool
consistent spec ls = Prelude.not <$> inconsistent spec ls

-- | True if the given labelled streams are inconsistent in the spec.
inconsistent :: Spec -> [(String, Stream Bool)] -> IO Bool
inconsistent spec ls = do
  r <- detectResult $ void $ reify $ do
         let guards      = map snd ls
             props       = map not guards
             allTogether = foldr (&&) true props
         spec
         void $ theorem
                  "consistent"
                  (exists allTogether)
                  (Prove.check (kind2Prover def))

  return (r Prelude.== I)

-- | Return the subsets of spec triggers that are mutually inconsistent.
inconsistentSubsets :: Spec -> IO [[(String, Stream Bool)]]
inconsistentSubsets spec = do
  let ts     = specTriggers' spec
      guards = map (\(S.Trigger n g _) -> (n, g)) ts
  minimalFailingSubsets (consistent spec) guards

-- | Find all minimal failing subsets.
minimalFailingSubsets :: ([(String, Stream Bool)] -> IO Bool)
                      -> [(String, Stream Bool)]
                      -> IO [[(String, Stream Bool)]]
minimalFailingSubsets p xs =
    go [] (filter (Prelude.not . null) $ subsets xs)
  where
    go acc [] = pure acc
    go acc (s:ss)
      | any (\a -> subsetOfWith sameKey a s) acc = go acc ss
      | otherwise = do
          ok <- p s
          if ok
            then go acc ss
            else go (s:acc) ss

-- | Result of calling Kind2.
data Result = V | I | U
  deriving (Eq, Show)

-- | Detect whether "valid" or "invalid" appears in stdout of an action.
--
-- Used to process the result of calling Kind2.
detectResult :: IO () -> IO Result
detectResult action = do
  out <- captureStdout action
  let ws = words out
  pure $
    if "invalid" `elem` ws then I
    else if "valid" `elem` ws then V
    else U

-- * Auxiliary functions

-- | True if two values share the same first component.
sameKey :: Eq a => (a, b) -> (a, b) -> Bool
sameKey x1 x2 = fst x1 Prelude.== fst x2

-- | Check whether the first subset is contained in the second, using an
-- auxiliary function to check for equality.
subsetOfWith :: (a -> a -> Bool) -> [a] -> [a] -> Bool
subsetOfWith f a b = all (\k -> any (f k) b) a

-- | Triggers of a specification.
specTriggers' :: Spec -> [S.Trigger]
specTriggers' = triggers . snd . runWriter

-- | Generate all subsets of a list.
subsets :: [a] -> [[a]]
subsets []     = [[]]
subsets (x:xs) = ss Prelude.++ map (x:) ss
  where
    ss = subsets xs

-- | Capture stdout produced by an IO action.
--
-- This action prints information from the action in a file in a temporary
-- directory. The file should have permissions set so that only the current
-- user can read it, and be deleted right after. However, if an intruder has
-- permissions to see all users' temporary files, when they may be able to
-- inspect this file.
captureStdout :: IO a -> IO String
captureStdout action =
  withSystemTempFile "copilot-stdout.txt" $ \_ h -> do
    hSetBuffering h LineBuffering
    old <- hDuplicate stdout
    hDuplicateTo h stdout
    _ <- action `finally` hDuplicateTo old stdout
    hSeek h AbsoluteSeek 0
    out <- hGetContents h
    -- force read
    evaluate (rnf out)
    pure out
