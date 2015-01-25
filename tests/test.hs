import Test.Tasty
import Test.Tasty.HUnit
import Control.Applicative

import Backgammon

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "initial white pip count is 167" $
      pipCount White newGame @?= 167

  , testCase "initial black pip count is 167" $
      pipCount Black newGame @?= 167

  , testCase "initial game state is 'players to throw initial'" $
      gameState newGame @?= PlayersToThrowInitial

  , testCase "when white wins initial throw it is to move with thrown dice" $
      gameState <$> (performAction (InitialThrows 2 1) newGame) @?= (Right (ToMove White (2, 1)))

  , testCase "when black wins initial throw it is to move with thrown dice" $
      gameState <$> (performAction (InitialThrows 1 2) newGame) @?= (Right (ToMove Black (2, 1)))

  , testCase "when initial throw is a tie the state is again 'players to throw initial'" $
      gameState <$> (performAction (InitialThrows 3 3) newGame) @?= (Right PlayersToThrowInitial)
  ]

