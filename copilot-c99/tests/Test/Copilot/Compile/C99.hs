-- | Test copilot-c99:Copilot.Compile.C99.
module Test.Copilot.Compile.C99 where

-- External imports
import Control.Exception                    (IOException, catch)
import Data.List                            (intersperse)
import System.Directory                     (doesFileExist,
                                             getTemporaryDirectory,
                                             removeDirectory, removeFile,
                                             setCurrentDirectory)
import System.IO                            (hPutStrLn, stderr)
import System.Posix.Temp                    (mkdtemp)
import System.Process                       (callProcess, readProcess)
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
    , testProperty "Can compile and run specification"       testCompileAndRun
    , testProperty "Compiling plusOne works correctly"       testPlusOne
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

    function = "nop"

    guard = Const Bool True
    -- guard = (Op2 (Eq Int8) (Drop Int8 0 0) (Const Int8 2))

    args = []

-- | Test compile.
testCompileAndRun :: Property
testCompileAndRun = ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "copilot_test_"
    setCurrentDirectory testDir

    compile "copilot_test" spec
    r <- compileC "copilot_test"

    let cProgram = unlines
          [ "#include \"copilot_test.h\""
          , ""
          , "void nop () {"
          , "}"
          , ""
          , "void main () {"
          , "  step();"
          , "}"
          ]

    writeFile "main.c" cProgram

    -- Compile a main program
    r2 <- compileExecutable "main" [ "copilot_test.o" ]
    callProcess "./main" []

    -- Remove file produced by GCC
    removeFile "copilot_test.o"
    removeFile "main"

    -- Remove files produced "by hand"
    removeFile "main.c"

    -- Remove files produced by Copilot
    removeFile "copilot_test.c"
    removeFile "copilot_test.h"
    removeFile "copilot_test_types.h"

    setCurrentDirectory tmpDir
    removeDirectory testDir

    return $ r && r2

  where

    spec = Spec streams observers triggers properties

    streams    = [ Stream 0 [1] (Const Int8 1) Int8]
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "nop"

    guard = Const Bool True
    -- guard = (Op2 (Eq Int8) (Drop Int8 0 0) (Const Int8 2))

    args = []

-- | Test id.
testPlusOne :: Property
testPlusOne = ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "copilot_test_"
    setCurrentDirectory testDir

    compile "copilot_test" spec
    r <- compileC "copilot_test"

    let cProgram = unlines
          [ "#include <stdio.h>"
          , "#include <stdint.h>"
          , "#include \"copilot_test.h\""
          , ""
          , "int NUM_INPUTS = " ++ numInputsStr ++ ";"
          , "int8_t inputs[] = {" ++ inputsStr ++ "};"
          , ""
          , "int8_t input = 0;"
          , ""
          , "void printBack (int8_t num) {"
          , "  printf(\"%d\\n\", num);"
          , "}"
          , ""
          , "int main () {"
          , "  int i = 0;"
          , "  for (i = 0; i < NUM_INPUTS; i++) {"
          , "    input = inputs[i];"
          , "    step();"
          , "  }"
          , "  return 0;"
          , "}"
          ]

    writeFile "main.c" cProgram

    -- Compile a main program
    r2 <- compileExecutable "main" [ "copilot_test.o" ]

    print r2
    print testDir

    out <- readProcess "./main" [] ""

    let ls   = lines out
        nums = fmap read ls

        comparison = nums == fmap (+1) input

    -- Remove file produced by GCC
    removeFile "copilot_test.o"
    removeFile "main"

    -- Remove files produced "by hand"
    removeFile "main.c"

    -- Remove files produced by Copilot
    removeFile "copilot_test.c"
    removeFile "copilot_test.h"
    removeFile "copilot_test_types.h"

    setCurrentDirectory tmpDir
    removeDirectory testDir

    return $ r && r2 && comparison

  where

    numInputsStr = show numInputs
    numInputs    = 10

    inputsStr = concat $ intersperse ", " $ fmap show input

    input = [1..10]

    spec = Spec streams observers triggers properties

    streams    = []
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "printBack"

    guard = Const Bool True

    args = [UExpr Int8 (Op2 (Add Int8) (ExternVar Int8 "input" Nothing) (Const Int8 1))]

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

-- * Auxiliary functions

-- | Compile a C file given its basename.
compileC :: String -> IO Bool
compileC baseName = do
  result <- catch (do callProcess "gcc" [ "-c", baseName ++ ".c" ]
                      return True
                  )
                  (\e -> do
                     hPutStrLn stderr $
                       "copilot-c99: error: compileC: cannot compile "
                         ++ baseName ++ ".c with gcc"
                     hPutStrLn stderr $
                       "copilot-c99: exception: " ++ show (e :: IOException)
                     return False
                  )
  if result
    then doesFileExist $ baseName ++ ".o"
    else return False

-- | Compile a C file into an executable, given its basename and files to link
-- with it.
compileExecutable :: String -> [String] -> IO Bool
compileExecutable baseName linked = do
  result <- catch (do callProcess "gcc" $ [ baseName ++ ".c" ]
                                          ++ linked
                                          ++ [ "-o", baseName ]
                      return True
                  )
                  (\e -> do
                     hPutStrLn stderr $
                       "copilot-c99: error: compileExecutable: cannot compile "
                         ++ baseName ++ ".c with gcc"
                     hPutStrLn stderr $
                       "copilot-c99: exception: " ++ show (e :: IOException)
                     return False
                  )
  if result
    then doesFileExist baseName
    else return False
