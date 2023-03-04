{-# LANGUAGE ExistentialQuantification #-}
-- | Test copilot-c99:Copilot.Compile.C99.
module Test.Copilot.Compile.C99 where

-- External imports
import Control.Exception                    (IOException, catch)
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
import Test.QuickCheck                      (Gen, Property, elements,
                                             forAllBlind, ioProperty, shuffle,
                                             (===), (==>), forAll, listOf, (.&&.))
import Test.QuickCheck.Gen                  (chooseUpTo, elements)

-- External imports: Copilot
import qualified Copilot.Core as CC
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
    , testProperty "Compiling plusOne works correctly"       testRunCompare
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

    args = []

-- | Test running a compiled C program and comparing the results.
testRunCompare :: Property
testRunCompare = testRunCompare' opsInt8
           .&&.  testRunCompare' opsInt16

testRunCompare' :: Gen Wrapper -> Property
testRunCompare' ops =
  forAllBlind ops $ \testCase ->
    let (Wrapper copilotUExpr haskellFun inputVar outputVar name) = testCase
        (cTypeInput, gen, cInputName) = inputVar
    in
      forAll (listOf gen) $ \nums -> do
        let inputs = [ (cTypeInput, fmap show nums, cInputName) ]
        f inputs nums copilotUExpr haskellFun outputVar name

f :: (Show a, Read b, Eq b)
  => [(String, [String], String)]
  -> [a]
  -> UExpr
  -> (a -> b)
  -> (String, String)
  -> String
  -> Property
f inputs nums copilotUExpr haskellFun outputVar name = do
        let (cTypeRes, cStr) = outputVar

        let numSteps      = length nums
            maxInputsName = "MAX_STEPS"

        let vars = map oneInput inputs
            oneInput (cTypeInput, inputVals, cInputName) =
                (cTypeInput, inputVarName, inputArrVarName, inputVals)
              where
                inputVarName    = cInputName
                inputArrVarName = cInputName ++ "_s"

        ioProperty $ do

          tmpDir <- getTemporaryDirectory
          setCurrentDirectory tmpDir

          testDir <- mkdtemp "copilot_test_"
          setCurrentDirectory testDir

          hPutStrLn stderr $ "Testing\t" ++ name ++
                             -- " :: " ++ cTypeInput ++ " -> " ++ cTypeRes ++
                             "\t with inputs: " ++ show nums

          let spec = Spec streams observers triggers properties

              streams    = []
              observers  = []
              properties = []

              triggers = [ Trigger function guard args ]
              function = "printBack"
              guard    = Const Bool True
              args     = [copilotUExpr]

          compile "copilot_test" spec
          r <- compileC "copilot_test"

          let varDecls :: [String]
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

          let cProgram = unlines $
                [ "#include <stdio.h>"
                , "#include <stdint.h>"
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

          writeFile "main.c" cProgram

          -- Compile a main program
          r2 <- compileExecutable "main" [ "copilot_test.o" ]

          print r2
          print testDir

          out <- readProcess "./main" [] ""

          let ls   = lines out
              outNums = fmap read ls

              comparison = outNums == fmap haskellFun nums

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

opsInt8 :: Gen Wrapper
opsInt8 = elements
  [ Wrapper
      ( UExpr Int8 (Op2 (Add Int8) (ExternVar Int8 "input" Nothing) (Const Int8 1)) )
      ( (+1) :: Int8 -> Int8 )
      ( "int8_t", arbInt8 (maxBound - 1, minBound), "input" )
      ( "int8_t", "%d" )
      ( "plusOne" )

  , Wrapper
      ( UExpr Int8 (ExternVar Int8 "input" Nothing) )
      ( id :: Int8 -> Int8 )
      ( "int8_t", arbInt8 (maxBound - 1, minBound), "input")
      ( "int8_t", "%d" )
      ( "identity" )
  ]

opsInt16 :: Gen Wrapper
opsInt16 = elements
  [ Wrapper
      ( UExpr Int16 (Op2 (Add Int16) (ExternVar Int16 "input" Nothing) (Const Int16 1)) )
      ( (+1) :: Int16 -> Int16 )
      (  "int16_t", arbInt16 (maxBound - 1, minBound), "input")
      (  "int16_t", "%d" )
      ( "plusOne" )
  , Wrapper
      ( UExpr Int16 (Op2 (Sub Int16) (ExternVar Int16 "input" Nothing) (Const Int16 128)) )
      ( (\x -> x - 128) :: Int16 -> Int16 )
      ( "int16_t", arbInt16 (maxBound, minBound + 128), "input")
      ( "int16_t", "%d" )
      ( "minusOne" )
  ]

data Wrapper = forall a b . (Read a, Show b, Eq b) => Wrapper
  { wrapExpr   :: UExpr
  , wrapFun    :: a -> b
  , wrapCopInp :: (String, Gen a, String)
  , wrapCopOut :: (String, String)
  , wrapName   :: String
  }

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

-- * Generators

arbInts8 :: (Int8, Int8) -> Gen [Int8]
arbInts8 = listOf . arbInt8

arbInt8 :: (Int8, Int8) -> Gen Int8
arbInt8 (hi, lo) = do
    w <- chooseUpTo (fromIntegral hi - fromIntegral lo)
    return $ fromIntegral (w + fromIntegral lo)

arbInt16 :: (Int16, Int16) -> Gen Int16
arbInt16 (hi, lo) = do
    w <- chooseUpTo (fromIntegral hi - fromIntegral lo)
    return $ fromIntegral (w + fromIntegral lo)
