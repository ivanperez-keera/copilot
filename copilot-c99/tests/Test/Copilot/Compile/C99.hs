-- | Test copilot-c99:Copilot.Compile.C99.
module Test.Copilot.Compile.C99 where

-- External imports
import Control.Exception                    (IOException, catch)
import System.Directory                     (doesFileExist,
                                             getTemporaryDirectory,
                                             removeDirectory, removeFile,
                                             setCurrentDirectory)
import System.IO                            (hPutStrLn, stderr)
import System.Posix.Temp                    (mkdtemp)
import System.Process                       (callProcess)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Gen, Property, elements,
                                             forAllBlind, ioProperty, shuffle,
                                             (===), (==>))

-- External imports: Copilot
import Copilot.Core hiding (Property)

-- External imports: Modules being tested
import Copilot.Compile.C99

-- | All unit tests for copilot-core:Copilot.Core.Type.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Compile.C99"
    [ testProperty "Can compile specification"               testCompile
    , testProperty "Can compile specification in custom dir" testCompileCustomDir
--    , testProperty "Compiling ID works correctly"            testId
    ]

-- | Test compile.
testCompile :: Property
testCompile = ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "copilot_test_"
    setCurrentDirectory testDir

    compile "copilot_test" spec
    r <- compileC "copilot_test"

    -- Remove file produced by GCC
    removeFile "copilot_test.o"

    -- Remove files produced by Copilot
    removeFile "copilot_test.c"
    removeFile "copilot_test.h"
    removeFile "copilot_test_types.h"

    setCurrentDirectory tmpDir
    removeDirectory testDir

    return r

  where

    spec = Spec streams observers triggers properties

    streams    = [ Stream 0 [1] (Const Int8 1) Int8]
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "func"

    guard = Const Bool True
    -- guard = (Op2 (Eq Int8) (Drop Int8 0 0) (Const Int8 2))

    args = []

compileC :: String -> IO Bool
compileC specName = do
  result <- catch (do callProcess "gcc" [ "-c", specName ++ ".c" ]
                      return True
                  )
                  (\e -> do
                     hPutStrLn stderr $
                       "copilot-c99: error: compileC: cannot compile "
                         ++ specName ++ ".c with gcc"
                     hPutStrLn stderr $
                       "copilot-c99: exception: " ++ show (e :: IOException)
                     return False
                  )
  if result
    then doesFileExist $ specName ++ ".o"
    else return False

-- | Test compile.
testCompileCustomDir :: Property
testCompileCustomDir = ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "copilot_test_"

    compileWith (mkDefaultCSettings { cSettingsOutputDirectory = testDir })
                "copilot_test"
                spec

    setCurrentDirectory testDir
    r <- compileC "copilot_test"

    -- Remove file produced by GCC
    removeFile "copilot_test.o"

    -- Remove files produced by Copilot
    removeFile "copilot_test.c"
    removeFile "copilot_test.h"
    removeFile "copilot_test_types.h"

    setCurrentDirectory tmpDir
    removeDirectory testDir

    return r

  where

    spec = Spec streams observers triggers properties

    streams    = [ Stream 0 [1] (Const Int8 1) Int8]
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "func"

    guard = Const Bool True
    -- guard = (Op2 (Eq Int8) (Drop Int8 0 0) (Const Int8 2))

    args = []

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
