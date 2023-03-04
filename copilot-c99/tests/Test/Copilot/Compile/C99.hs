-- | Test copilot-c99:Copilot.Compile.C99.
module Test.Copilot.Compile.C99 where

-- External imports
import Control.Exception                    (IOException, catch)
import Control.Monad                        (when)
import Data.List                            (intersperse)
import Data.Typeable                        (Typeable)
import System.Directory                     (doesFileExist,
                                             getTemporaryDirectory,
                                             removeDirectory, removeFile,
                                             setCurrentDirectory)
import System.IO                            (hPutStrLn, stderr)
import System.Posix.Temp                    (mkdtemp)
import System.Process                       (callProcess, readProcess)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Gen, Property, arbitrary, elements,
                                             forAll, forAllBlind, getPositive,
                                             ioProperty, vectorOf, (.&&.),
                                             (===), (==>))
import Test.QuickCheck.Gen                  (chooseUpTo, chooseBoundedIntegral)

-- External imports: Copilot
import Copilot.Core hiding (Property)

-- External imports: Modules being tested
import Copilot.Compile.C99

-- * Test cases

-- | All unit tests for copilot-core:Copilot.Core.Type.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Compile.C99"
    [ testProperty "Can compile specification"               testCompile
    , testProperty "Can compile specification in custom dir" testCompileCustomDir
    , testProperty "Can compile and run specification"       testCompileAndRun
    , testProperty "Compiling plusOne works correctly"       testRunCompare
    ]

-- ** Test: Compile a generated spec

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

    args = []

-- ** Test: Compile a generated spec in a custom dir

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

    args = []

-- ** Test: Compile a generated spec and run it but ignore the result

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

    args = []

-- ** Test: Compile a generated spec, run it and compare the results

-- | Test running compiled C programs and comparing the results.
testRunCompare :: Property
testRunCompare = testRunCompare1 opsInt8
            .&&. testRunCompare1 opsInt16
            .&&. testRunCompare1 opsInt32
            .&&. testRunCompare1 opsInt64
            .&&. testRunCompare1 opsWord8
            .&&. testRunCompare1 opsWord16
            .&&. testRunCompare1 opsWord32
            .&&. testRunCompare1 opsWord64

-- | Test running a compiled C program and comparing the results.
testRunCompare1 :: (Show a, Read b, Eq b) => Gen (TestCase a b) -> Property
testRunCompare1 ops =
  forAllBlind ops $ \testCase ->
    let (TestCase copilotSpec haskellFun inputVar outputVar) = testCase
        (cTypeInput, cInputName, gen) = inputVar

    in forAll (getPositive <$> arbitrary) $ \len ->

         forAll (vectorOf len gen) $ \nums -> do

         let inputs  = [ (cTypeInput, fmap show nums, cInputName) ]
             outputs = haskellFun nums

         testRunCompareArg inputs len outputs copilotSpec outputVar

-- | Test running a compiled C program and comparing the results, when the
-- program produces one output as an argument to a trigger that always fires.
--
-- PRE: all lists (second argument) of inputs have the length given as second
-- argument.
--
-- PRE: the monitoring code this is linked against uses the function
-- @printBack@ with exactly one argument to pass the results.
testRunCompareArg :: (Read b, Eq b)
                  => [(String, [String], String)]
                  -> Int
                  -> [b]
                  -> Spec
                  -> (String, String)
                  -> Property
testRunCompareArg inputs numInputs nums spec outputVar =
  ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    -- Operate in temporary directory
    testDir <- mkdtemp "copilot_test_"
    setCurrentDirectory testDir

    -- Produce copilot monitoring code
    compile "copilot_test" spec
    r <- compileC "copilot_test"

    -- Produce wrapper program
    let cProgram = testRunCompareArgCProgram inputs numInputs outputVar
    writeFile "main.c" cProgram

    -- Compile main program
    r2 <- compileExecutable "main" [ "copilot_test.o" ]

    -- Print result so far (for debugging purposes only)
    print r2
    print testDir

    -- Run program and compare result
    out <- readProcess "./main" [] ""
    let outNums = fmap read $ lines out
        comparison = outNums == nums

    -- Only clean up if the test succeeded; otherwise, we want to inspect it.
    when comparison $ do
      -- Remove file produced by GCC
      removeFile "copilot_test.o"
      removeFile "main"

      -- Remove files produced "by hand"
      removeFile "main.c"

      -- Remove files produced by Copilot
      removeFile "copilot_test.c"
      removeFile "copilot_test.h"
      removeFile "copilot_test_types.h"

      -- Remove temporary directory
      setCurrentDirectory tmpDir
      removeDirectory testDir

    return $ r && r2 && comparison

-- | Return a wrapper C program that runs a loop for a number of iterations,
-- putting values in global variables at every step, running the monitors, and
-- publishing the results of any outputs.
testRunCompareArgCProgram :: [(String, [String], String)]
                          -> Int
                          -> (String, String)
                          -> String
testRunCompareArgCProgram inputs numSteps outputVar = unlines $
    [ "#include <stdio.h>"
    , "#include <stdint.h>"
    , "#include <inttypes.h>"
    , "#include \"copilot_test.h\""
    , ""
    ]
    ++ varDecls ++
    [ ""
    , "void printBack (" ++ cTypeRes ++ " num) {"
    , "  printf(\"" ++ cStr ++ "\\n\", num);"
    , "}"
    , ""
    , "int main () {"
    , "  int i = 0;"
    , "  for (i = 0; i < " ++ maxInputsName ++ "; i++) {"
    ]
    ++ inputUpdates ++
    [ ""
    , "    step();"
    , "  }"
    , "  return 0;"
    , "}"
    ]

  where

    varDecls :: [String]
    varDecls =
      [ "int " ++ maxInputsName ++ " = " ++ show numSteps ++ ";" ]
      ++ inputVarDecls

    inputVarDecls :: [String]
    inputVarDecls = concatMap (\(ctype, varName, arrVar, arrVals) ->
      let inputsStr = concat $ intersperse ", " (arrVals :: [String]) in
      [ ctype ++ " " ++ arrVar ++ "[] = {" ++ inputsStr ++ "};"
      , ""
      , ctype ++ " " ++ varName ++ ";"
      ])
      vars

    inputUpdates :: [String]
    inputUpdates = concatMap (\(ctype, varName, arrVar, arrVals) ->
      [ "    " ++ varName ++ " = " ++ arrVar ++ "[i];"
      ])
      vars

    (cTypeRes, cStr) = outputVar

    vars = map oneInput inputs
    oneInput (cTypeInput, inputVals, cInputName) =
        (cTypeInput, inputVarName, inputArrVarName, inputVals)
      where
        inputVarName    = cInputName
        inputArrVarName = cInputName ++ "_s"

    maxInputsName = "MAX_STEPS"

-- *** Wrap expression tested, equivalent Haskell function, and C information

data TestCase a b = TestCase
  { wrapExpr   :: Spec
  , wrapFun    :: [a] -> [b]
  , wrapCopInp :: (String, String, Gen a)
  , wrapCopOut :: (String, String)
  }

-- *** Specific test cases

opsInt8 :: Gen (TestCase Int8 Int8)
opsInt8 = elements
  [ TestCase
      ( alwaysTriggerArg1 $ UExpr Int8 (ExternVar Int8 "input" Nothing) )
      ( fmap id :: [Int8] -> [Int8])
      ( "int8_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "int8_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Int8 (Op2 (Add Int8) (ExternVar Int8 "input" Nothing) (Const Int8 1)) )
      ( fmap (+1) :: [Int8] -> [Int8] )
      ( "int8_t", "input", chooseBoundedIntegral (minBound, maxBound - 1) )
      ( "int8_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Int8 (Op2 (Sub Int8) (ExternVar Int8 "input" Nothing) (Const Int8 64)) )
      ( fmap (\x -> x - 64) :: [Int8] -> [Int8] )
      ( "int8_t", "input", chooseBoundedIntegral (minBound + 64, maxBound) )
      ( "int8_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Int8 (Op2 (Sub Int8) (ExternVar Int8 "input" Nothing) (ExternVar Int8 "input" Nothing)) )
      ( fmap (const 0) :: [Int8] -> [Int8] )
      ( "int8_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "int8_t", "%d" )

  , TestCase
      ( sometimesTriggerArg1 (Op2 (Eq Int8) (Op2 (Mod Int8) (ExternVar Int8 "input" Nothing) (Const Int8 2)) (Const Int8 0))
                             (UExpr Int8 (ExternVar Int8 "input" Nothing)) )
      ( filter even :: [Int8] -> [Int8])
      ( "int8_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "int8_t", "%d" )
  ]

opsInt16 :: Gen (TestCase Int16 Int16)
opsInt16 = elements
  [ TestCase
      ( alwaysTriggerArg1 $ UExpr Int16 (ExternVar Int16 "input" Nothing) )
      ( fmap id :: [Int16] -> [Int16] )
      ( "int16_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "int16_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Int16 (Op2 (Add Int16) (ExternVar Int16 "input" Nothing) (Const Int16 1)) )
      ( fmap (+1) :: [Int16] -> [Int16] )
      ( "int16_t", "input", chooseBoundedIntegral (minBound, maxBound - 1) )
      ( "int16_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Int16 (Op2 (Sub Int16) (ExternVar Int16 "input" Nothing) (Const Int16 128)) )
      ( fmap (\x -> x - 128) :: [Int16] -> [Int16] )
      ( "int16_t", "input", chooseBoundedIntegral (minBound + 128, maxBound) )
      ( "int16_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Int16 (Op2 (Sub Int16) (ExternVar Int16 "input" Nothing) (ExternVar Int16 "input" Nothing)) )
      ( fmap (const 0) :: [Int16] -> [Int16] )
      ( "int16_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "int16_t", "%d" )
  ]

opsInt32 :: Gen (TestCase Int32 Int32)
opsInt32 = elements
  [ TestCase
      ( alwaysTriggerArg1 $ UExpr Int32 (ExternVar Int32 "input" Nothing) )
      ( fmap id :: [Int32] -> [Int32] )
      ( "int32_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "int32_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Int32 (Op2 (Add Int32) (ExternVar Int32 "input" Nothing) (Const Int32 1)) )
      ( fmap (+1) :: [Int32] -> [Int32] )
      ( "int32_t", "input", chooseBoundedIntegral (minBound, maxBound - 1) )
      ( "int32_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Int32 (Op2 (Sub Int32) (ExternVar Int32 "input" Nothing) (Const Int32 128)) )
      ( fmap (\x -> x - 128) :: [Int32] -> [Int32] )
      ( "int32_t", "input", chooseBoundedIntegral (minBound + 128, maxBound) )
      ( "int32_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Int32 (Op2 (Sub Int32) (ExternVar Int32 "input" Nothing) (ExternVar Int32 "input" Nothing)) )
      ( fmap (const 0) :: [Int32] -> [Int32] )
      ( "int32_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "int32_t", "%d" )
  ]

opsInt64 :: Gen (TestCase Int64 Int64)
opsInt64 = elements
  [ TestCase
      ( alwaysTriggerArg1 $ UExpr Int64 (ExternVar Int64 "input" Nothing) )
      ( fmap id :: [Int64] -> [Int64] )
      ( "int64_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "int64_t", "%\" PRId64 \"" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Int64 (Op2 (Add Int64) (ExternVar Int64 "input" Nothing) (Const Int64 1)) )
      ( fmap (+1) :: [Int64] -> [Int64] )
      ( "int64_t", "input", chooseBoundedIntegral (minBound, maxBound - 1) )
      ( "int64_t", "%\" PRId64 \"" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Int64 (Op2 (Sub Int64) (ExternVar Int64 "input" Nothing) (Const Int64 128)) )
      ( fmap (\x -> x - 128) :: [Int64] -> [Int64] )
      ( "int64_t", "input", chooseBoundedIntegral (minBound + 128, maxBound) )
      ( "int64_t", "%\" PRId64 \"" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Int64 (Op2 (Sub Int64) (ExternVar Int64 "input" Nothing) (ExternVar Int64 "input" Nothing)) )
      ( fmap (const 0) :: [Int64] -> [Int64] )
      ( "int64_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "int64_t", "%\" PRId64 \"" )
  ]

opsWord8 :: Gen (TestCase Word8 Word8)
opsWord8 = elements
  [ TestCase
      ( alwaysTriggerArg1 $ UExpr Word8 (ExternVar Word8 "input" Nothing) )
      ( fmap id :: [Word8] -> [Word8] )
      ( "uint8_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "uint8_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Word8 (Op2 (Add Word8) (ExternVar Word8 "input" Nothing) (Const Word8 1)) )
      ( fmap (+1) :: [Word8] -> [Word8] )
      ( "uint8_t", "input", chooseBoundedIntegral (minBound, maxBound - 1) )
      ( "uint8_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Word8 (Op2 (Sub Word8) (ExternVar Word8 "input" Nothing) (Const Word8 64)) )
      ( fmap (\x -> x - 64) :: [Word8] -> [Word8] )
      ( "uint8_t", "input", chooseBoundedIntegral (minBound + 128, maxBound) )
      ( "uint8_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Word8 (Op2 (Sub Word8) (ExternVar Word8 "input" Nothing) (ExternVar Word8 "input" Nothing)) )
      ( fmap (const 0) :: [Word8] -> [Word8] )
      ( "uint8_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "uint8_t", "%d" )
  ]

opsWord16 :: Gen (TestCase Word16 Word16)
opsWord16 = elements
  [ TestCase
      ( alwaysTriggerArg1 $ UExpr Word16 (ExternVar Word16 "input" Nothing) )
      ( fmap id :: [Word16] -> [Word16] )
      ( "uint16_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "uint16_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Word16 (Op2 (Add Word16) (ExternVar Word16 "input" Nothing) (Const Word16 1)) )
      ( fmap (+1) :: [Word16] -> [Word16] )
      ( "uint16_t", "input", chooseBoundedIntegral (minBound, maxBound - 1) )
      ( "uint16_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Word16 (Op2 (Sub Word16) (ExternVar Word16 "input" Nothing) (Const Word16 128)) )
      ( fmap (\x -> x - 128) :: [Word16] -> [Word16] )
      ( "uint16_t", "input", chooseBoundedIntegral (minBound + 128, maxBound) )
      ( "uint16_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Word16 (Op2 (Sub Word16) (ExternVar Word16 "input" Nothing) (ExternVar Word16 "input" Nothing)) )
      ( fmap (const 0) :: [Word16] -> [Word16] )
      ( "uint16_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "uint16_t", "%d" )
  ]

opsWord32 :: Gen (TestCase Word32 Word32)
opsWord32 = elements
  [ TestCase
      ( alwaysTriggerArg1 $ UExpr Word32 (ExternVar Word32 "input" Nothing) )
      ( fmap id :: [Word32] -> [Word32] )
      ( "uint32_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "uint32_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Word32 (Op2 (Add Word32) (ExternVar Word32 "input" Nothing) (Const Word32 1)) )
      ( fmap (+1) :: [Word32] -> [Word32] )
      ( "uint32_t", "input", chooseBoundedIntegral (minBound, maxBound - 1) )
      ( "uint32_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Word32 (Op2 (Sub Word32) (ExternVar Word32 "input" Nothing) (Const Word32 128)) )
      ( fmap (\x -> x - 128) :: [Word32] -> [Word32] )
      ( "uint32_t", "input", chooseBoundedIntegral (minBound + 128, maxBound) )
      ( "uint32_t", "%d" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Word32 (Op2 (Sub Word32) (ExternVar Word32 "input" Nothing) (ExternVar Word32 "input" Nothing)) )
      ( fmap (const 0) :: [Word32] -> [Word32] )
      ( "uint32_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "uint32_t", "%d" )
  ]

opsWord64 :: Gen (TestCase Word64 Word64)
opsWord64 = elements
  [ TestCase
      ( alwaysTriggerArg1 $ UExpr Word64 (ExternVar Word64 "input" Nothing) )
      ( fmap id :: [Word64] -> [Word64] )
      ( "uint64_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "uint64_t", "%\" PRIu64 \"" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Word64 (Op2 (Add Word64) (ExternVar Word64 "input" Nothing) (Const Word64 1)) )
      ( fmap (+1) :: [Word64] -> [Word64] )
      ( "uint64_t", "input", chooseBoundedIntegral (minBound, maxBound - 1) )
      ( "uint64_t", "%\" PRIu64 \"" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Word64 (Op2 (Sub Word64) (ExternVar Word64 "input" Nothing) (Const Word64 128)) )
      ( fmap (\x -> x - 128) :: [Word64] -> [Word64] )
      ( "uint64_t", "input", chooseBoundedIntegral (minBound + 128, maxBound) )
      ( "uint64_t", "%\" PRIu64 \"" )

  , TestCase
      ( alwaysTriggerArg1 $ UExpr Word64 (Op2 (Sub Word64) (ExternVar Word64 "input" Nothing) (ExternVar Word64 "input" Nothing)) )
      ( fmap (const 0) :: [Word64] -> [Word64] )
      ( "uint64_t", "input", chooseBoundedIntegral (minBound, maxBound) )
      ( "uint64_t", "%\" PRIu64 \"" )
  ]

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

-- ** Spec builders

-- | Build a 'Spec' that triggers at every step, passing the given expression
-- as argument, and execution a function 'printBack'
alwaysTriggerArg1 :: UExpr -> Spec
alwaysTriggerArg1 = sometimesTriggerArg1 (Const Bool True)

-- | Build a 'Spec' that triggers based on a given boolean stream, passing the
-- given expression as argument, and execution a function 'printBack'
sometimesTriggerArg1 :: Expr Bool -> UExpr -> Spec
sometimesTriggerArg1 guard expr =
    Spec streams observers triggers properties
  where

    streams    = []
    observers  = []
    properties = []

    triggers = [ Trigger function guard args ]
    function = "printBack"
    args     = [ expr ]
