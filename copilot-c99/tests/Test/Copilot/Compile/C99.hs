-- | Test copilot-c99:Copilot.Compile.C99.
module Test.Copilot.Compile.C99 where

-- External imports
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Gen, Property, elements,
                                             forAllBlind, shuffle, (==>))

-- | All unit tests for copilot-core:Copilot.Core.Type.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Compile.C99"
    [ testProperty "Can compile specification"
        testCompile
    ]

-- | Test compile.
testCompile :: Property
testCompile = do
    dir <- getTempDir
    cd dir
    compile spec
    compileC specName
    removeGeneratedFiles specName
  where
    spec = Spec streams observers triggers properties

    streams    = []
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "print"

    guard = _

    args = _

-- | Test compile.
testCompileCustomDir :: Property
testCompileCustomDir = do
  dir <- getTempDir
  compileInDirectory dir spec
  compileCInDirectory dir specName
  removeGeneratedFiles specName
  remove generated files

-- | Test id.
testId :: Property
testId = do
  dir <- getTempDir
  compileInDirectory dir spec
  compileCInDirectory dir specName
  run C code with random stream as input
  compare output to random stream

-- | Test Op1.
testOp1 :: Property
testOp1 = do
  dir <- getTempDir
  compileInDirectory dir spec
  compileCInDirectory dir specName
  runAndCompare steps specName stream expected

-- | Test Op2.
testOp2 :: Property
testOp2 = do
  dir <- getTempDir
  compileInDirectory dir spec
  compileCInDirectory dir specName
  runAndCompare steps specName stream expected

-- | Test Op3.
testOp3 :: Property
testOp3 = do
  dir <- getTempDir
  compileInDirectory dir spec
  compileCInDirectory dir specName
  runAndCompare steps specName stream expected

-- | Test Op3.
testAppend :: Property
testAppend = do
  dir <- getTempDir
  compileInDirectory dir spec
  compileCInDirectory dir specName
  runAndCompare steps specName stream expected

-- | Test Delay.
testDelay :: Property
testDelay = do
  dir <- getTempDir
  compileInDirectory dir spec
  compileCInDirectory dir specName
  runAndCompare steps specName stream expected

-- | Run a program, feeding inputs to it and comparing the output to an expected
-- output.
runAndCompare :: (Show a, Read b)
              => String           -- ^ Program
              -> [(a, b)]         -- ^ Input and expected output
              -> IO Bool
runAndCompare program pairs = do
  p <- forkAndRun $ program
  runAndCompareLoop p pairs

runAndCompareLoop => Process  -- ^ Process to which the input must be fed
                  -> [(a, b)] -- ^ Input and output pairs
                  -> IO Bool
runAndCompareLoop p [] = do
  processIsAlive p
runAndCompareLoop p ((a, b):ps) = do
  feed a to p
  expectedOutput <- read output from p
  check exit status of p
  if output /= expected output
    then kill p
         return False
    else if p is dead
           then return False
           else runAndCompareLoop p ps
