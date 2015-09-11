module Backgammon.ModelTests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Applicative ((<$>))

import Backgammon
import Backgammon.Format

unitTests :: TestTree
unitTests = testGroup "Backgammon.Model unit tests"
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

  -- TODO: all other actions on new game: error

  , testCase "before any doubles, after a move, the other player can double" $
      gameState <$> (performActions [ (InitialThrows 3 1)
                                    , (PlayerAction (Moves [Move White 8 5, Move White 6 5]))] newGame) @?=
      (Right (ToDouble Black))

  , testCase "after a move, the other player can throw instead of doubling" $
      gameState <$> (performActions [ (InitialThrows 3 1)
                                    , (PlayerAction (Moves [Move White 8 5, Move White 6 5]))
                                    , (PlayerAction (Throw (3, 5)))] newGame) @?=
      (Right (ToMove Black (5, 3)))

  , testCase "board is updated after move" $
      gameBoard <$> (performActions [ (InitialThrows 3 1)
                                    , (PlayerAction (Moves [Move White 8 5, Move White 6 5]))] newGame) @?=
      (Right (fromRight (parseBoard "|b2...w2w4|.w2...b5|w5...b3.|b5....w2|")))

  ]

fromRight :: Show a => Either a b -> b
fromRight (Right v) = v
fromRight (Left v) = error ("expected Right but got Left " ++ show v)
