-- | Test copilot-c99:Copilot.Compile.C99.
module Test.Copilot.Compile.C99 where

-- External imports
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Gen, Property, elements,
                                             forAllBlind, shuffle, (==>), ioProperty, (===))

import System.Directory (setCurrentDirectory, removeFile, getTemporaryDirectory, setCurrentDirectory)
import System.Process (callProcess)

import Copilot.Core hiding (Property)
import Copilot.Compile.C99

-- | All unit tests for copilot-core:Copilot.Core.Type.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Compile.C99"
    [ testProperty "Can compile specification"               testCompile
--    , testProperty "Can compile specification in custom dir" testCompile
--    , testProperty "Compiling ID works correctly"            testId
    ]

-- | Test compile.
testCompile :: Property
testCompile = ioProperty $ do
    dir <- getTemporaryDirectory
    setCurrentDirectory dir
    compile "copilot_test" spec
    -- compileC "copilot_test"
    -- removeFile "copilot_test.c"
    -- removeFile "copilot_test.h"
    -- removeFile "copilot_test_types.h"
    return $ True === True
  where
    spec = Spec streams observers triggers properties

    streams    = [ Stream 0 [1] (Const Int8 1) Int8]
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "void"

    guard = Const Bool True
    -- guard = (Op2 (Eq Int8) (Drop Int8 0 0) (Const Int8 2))

    args = []

compileC :: String -> IO ()
compileC specName = do
  callProcess "gcc" [ "-c", "copilot_test.c" ]

-- -- | Test compile.
-- testCompileCustomDir :: Property
-- testCompileCustomDir = do
--   dir <- getTempDir
--   compileInDirectory dir spec
--   compileCInDirectory dir specName
--   removeGeneratedFiles specName
--   remove generated files
--
-- -- | Test id.
-- testId :: Property
-- testId = do
--   dir <- getTempDir
--   compileInDirectory dir spec
--   compileCInDirectory dir specName
--   runAndCompare program (zip stream expected)

-- -- | Test Op1.
-- testOp1 :: Property
-- testOp1 = do
--   dir <- getTempDir
--   compileInDirectory dir spec
--   compileCInDirectory dir specName
--   runAndCompare program (zip stream expected)
--
-- -- | Test Op2.
-- testOp2 :: Property
-- testOp2 = do
--   dir <- getTempDir
--   compileInDirectory dir spec
--   compileCInDirectory dir specName
--   runAndCompare steps specName stream expected
--
-- -- | Test Op3.
-- testOp3 :: Property
-- testOp3 = do
--   dir <- getTempDir
--   compileInDirectory dir spec
--   compileCInDirectory dir specName
--   runAndCompare steps specName stream expected
--
-- -- | Test Op3.
-- testAppend :: Property
-- testAppend = do
--   dir <- getTempDir
--   compileInDirectory dir spec
--   compileCInDirectory dir specName
--   runAndCompare steps specName stream expected
--
-- -- | Test Delay.
-- testDelay :: Property
-- testDelay = do
--   dir <- getTempDir
--   compileInDirectory dir spec
--   compileCInDirectory dir specName
--   runAndCompare steps specName stream expected

-- -- | Run a program, feeding inputs to it and comparing the output to an expected
-- -- output.
-- runAndCompare :: (Show a, Read b)
--               => String           -- ^ Program
--               -> [(a, b)]         -- ^ Input and expected output
--               -> IO Bool
-- runAndCompare program pairs = do
--   p <- forkAndRun $ program
--   runAndCompareLoop p pairs
--
-- runAndCompareLoop => Process  -- ^ Process to which the input must be fed
--                   -> [(a, b)] -- ^ Input and output pairs
--                   -> IO Bool
-- runAndCompareLoop p [] = do
--   processIsAlive p
-- runAndCompareLoop p ((a, b):ps) = do
--   feed a to p
--   expectedOutput <- read output from p
--   check exit status of p
--   if output /= expected output
--     then kill p
--          return False
--     else if p is dead
--            then return False
--            else runAndCompareLoop p ps
