import           Control.Monad (forM_, void)
import           Data.List     (nub, sort)
import           Prelude       hiding (all, mod, not, or, until, (&&), (++),
                                (<), (<=), (==), (>), (>=), (||))
import qualified Prelude       as P

import Copilot.Library.StateMachines
import Copilot.Theorem.What4
import Language.Copilot              (reify)
import Language.Copilot              hiding (all, or)

main :: IO ()
main = do
  spec' <- reify spec

  -- Use Z3 to prove the properties.
  results <- prove Z3 spec'

  -- Print the results.
  forM_ results $ \(nm, res) -> do
    putStr $ nm <> ": "
    case res of
      Valid   -> putStrLn "valid"
      Invalid -> putStrLn "invalid"
      Unknown -> putStrLn "unknown"

spec :: Spec
spec = do
  void $ prop "Example 1" (forAll true)
  void $ prop "Example 2" (forAll (checkDeterministic staticMachine))
  void $ prop "Example 3" (forAll (completeMachine staticMachine))

type StateMachineW8 = StateMachine Word8

stateMachine1 :: Stream Word8
stateMachine1 = stateMachine staticMachine

staticMachine :: StateMachineW8
staticMachine = (initialState, finalState, noInput, transitions, badState)

initialState :: Word8
initialState = 0
finalState :: Word8
finalState = 2

externalState :: Stream Word8
externalState = extern "state" Nothing

input :: Stream Word8
input = extern "input" Nothing

noInput :: Stream Bool
noInput = false

badState :: Word8
badState = 3

-- transitions = [ (0, input >= 120 && input <= 180, 0)
--               , (0, input > 180,  1)
--               , (1, input <= 180, 0)
--               , (1, input > 180,  1)
--               , (0, input < 120,  2)
--               , (2, input >= 120, 0)
--               , (2, input < 120,  2)
--               ]
transitions = [ (0, input > 180,  1)
              , (1, input <= 180, 0)
              , (1, input > 180,  1)
              , (0, input < 181,  2)
              , (2, input >= 120, 0)
              , (2, input < 120,  2)
              ]


-- | Given a list of transitions, and a current state, and a list of possible
-- destination states, produce a list of booleans indicating if a transition to
-- each of the destination states would be valid.
checkValidTransitions :: [(Word8, Stream Bool, Word8)]
                      -> Stream Word8
                      -> [Word8]
                      -> [Stream Bool]
checkValidTransitions transitions curState destinations =
  map (checkValidTransition transitions curState) destinations

-- | Given a list of transitions, and a current state, and destination states,
-- produce a list of booleans indicating if a transition to each of the
-- destination states would be valid.
checkValidTransition :: [(Word8, Stream Bool, Word8)]
                     -> Stream Word8
                     -> Word8
                     -> Stream Bool
checkValidTransition []                 _   _   = true
checkValidTransition ((so1, c, sd1):sx) so2 sd2 =
  ifThenElse
    ((constant so1 == so2) && (constant sd1 == constant sd2))
    c
    (checkValidTransition sx so2 sd2)

checkDeterministic :: Ord a => StateMachine a -> Stream Bool
checkDeterministic (_, _, _, ts, _) =
    all $ map (\s -> all (map not $ pairwise $ transitionsFrom s)) states
  where
    states            = nub $ sort $ concat $ map (\(s1, _, s2) -> [s1, s2]) ts
    transitionsFrom s = [ t | (s1, t, _) <- ts, s P.== s1 ]

completeMachine :: Ord a => StateMachine a -> Stream Bool
completeMachine (_, _, _, ts, _) =
    all $ map (\s -> or $ transitionsFrom s) states
  where
    states            = nub $ sort $ concat $ map (\(s1, _, s2) -> [s1, s2]) ts
    transitionsFrom s = [ t | (s1, t, _) <- ts, s P.== s1 ]

all :: [Stream Bool] -> Stream Bool
all [] = true
all (x:xs) = x && all xs

or :: [Stream Bool] -> Stream Bool
or [] = false
or (x:xs) = x || or xs

pairwise :: [Stream Bool] -> [Stream Bool]
pairwise []      = []
pairwise (_:[])  = []
pairwise (x1:xs) = (map (\x2 -> (x1 && x2)) xs) P.++ pairwise xs
