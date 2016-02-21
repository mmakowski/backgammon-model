module Backgammon.ModelTests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Applicative ((<$>))

import Backgammon
import Backgammon.Format

import Data.Set (Set)
import qualified Data.Set as Set

unitTests :: TestTree
unitTests = testGroup "Backgammon.Model unit tests"
  [ pipCountUnitTests
  , legalMovesUnitTests
  , gameStateAndActionsUnitTests
  ]

pipCountUnitTests :: TestTree
pipCountUnitTests = testGroup "pip count"
  [ testCase "initial white pip count is 167" $
      pipCount White (gameBoard newGame) @?= 167

  , testCase "initial black pip count is 167" $
      pipCount Black (gameBoard newGame) @?= 167

  , testCase "pip count is reduced by 4 after 3-1 move" $
      pipCount White (gameBoard gameAfterInitialWhite31) @?= 163

  , testCase "bar is included in pip count" $
      pipCount White (fromRight (parseBoard "|b2....w5|.w3...b5|w5...b3.|b5....w1|w1")) @?= 168
  ]

legalMovesUnitTests :: TestTree
legalMovesUnitTests = testGroup "legal moves"
  [ testCase "legal moves include permutations" $
      legalMoves White (fromRight (parseBoard "|b1.....|......|......|.....w1|")) (2, 1) @?=
        Set.fromList [[Move 24 22, Move 22 21], [Move 24 23, Move 23 21]]
  ]

gameStateAndActionsUnitTests :: TestTree
gameStateAndActionsUnitTests = testGroup "game state and actions"
  [ newGameUnitTests
  , movingUnitTests
  , doublingUnitTests
  , testCase "board is updated after move" $
      gameBoard gameAfterInitialWhite31 @?=
      fromRight (parseBoard "|b2...w2w4|.w2...b5|w5...b3.|b5....w2|")
  ]

newGameUnitTests :: TestTree
newGameUnitTests = testGroup "new game"
  [ testCase "initial game state is 'players to throw initial'" $
      gameState newGame @?= PlayersToThrowInitial

  , testCase "when white wins initial throw it is to move with thrown dice" $
      gameState <$> performAction (InitialThrows 2 1) newGame @?= Right (ToMove White (2, 1))

  , testCase "when black wins initial throw it is to move with thrown dice" $
      gameState <$> performAction (InitialThrows 1 2) newGame @?= Right (ToMove Black (2, 1))

  , testCase "when initial throw is a tie the state is again 'players to throw initial'" $
      gameState <$> performAction (InitialThrows 3 3) newGame @?= Right PlayersToThrowInitial

  -- TODO: all other actions on new game: error
  ]

movingUnitTests :: TestTree
movingUnitTests = testGroup "moving"
  [ testCase "player cannot move opponent's pieces" $
      let badMove = Moves [Move 24 23, Move 12 10]
      in performAction (PlayerAction White badMove) whiteToMove21 @?=
         Left (InvalidPlayerDecision whiteToMove21 badMove (MovedOpponentsPieces (Move 12 10)))

  , testCase "player must move by the thrown number of pips" $
      let badMove = Moves [Move 24 23, Move 24 21]
      in performAction (PlayerAction White badMove) whiteToMove21 @?=
         Left (InvalidPlayerDecision whiteToMove21 badMove (NoSuchNumberThrown (Move 24 21) [2]))

  , testCase "player must make two moves when no double is rolled and not blocked" $
      let badMove = Moves [Move 24 23]
      in performAction (PlayerAction White badMove) whiteToMove21 @?=
         Left (InvalidPlayerDecision whiteToMove21 badMove MoreMovesPossible)

  -- TODO: cannot move onto occupied point
  -- TODO: must move the correct number of moves
  ]

doublingUnitTests :: TestTree
doublingUnitTests = testGroup "doubling"
  [ testCase "before any doubles, after a move, the other player can double" $
      gameState gameAfterInitialWhite31 @?= ToDouble Black

  , testCase "after a move the other player can throw instead of doubling" $
      gameState <$> performAction (PlayerAction Black (Throw (3, 5))) gameAfterInitialWhite31 @?=
      Right (ToMove Black (5, 3))

  , testCase "after doubling the other player must respond" $
      gameState <$> performAction (PlayerAction Black Double) gameAfterInitialWhite31 @?=
      Right (ToRespondToDouble White)

  , testCase "after double is accepted the doubling cube is updated" $
      (\dc -> (doublingCubeOwner dc, doublingCubeValue dc)) <$> gameDoublingCube 
                                                            <$> performActions [PlayerAction Black Double, PlayerAction White AcceptDouble] gameAfterInitialWhite31 @?=
      Right (Just White, 2)

  , testCase "after double is accepted the player must throw" $
      gameState <$> performActions [PlayerAction Black Double, PlayerAction White AcceptDouble] gameAfterInitialWhite31 @?=
      Right (ToThrow Black)

  , testCase "after double is rejected the game ends" $
      gameState <$> performActions [PlayerAction Black Double, PlayerAction White RejectDouble] gameAfterInitialWhite31 @?=
      Right (GameFinished Black 1)
      
  , testCase "after double is accepted and the player throws, they must move" $
      gameState <$> performActions [PlayerAction Black Double, PlayerAction White AcceptDouble, PlayerAction Black (Throw (2, 1))] gameAfterInitialWhite31 @?=
      Right (ToMove Black (2, 1))

  , testCase "the owner of the cube can double before move" $
      gameState <$> performActions [ PlayerAction Black Double
                                   , PlayerAction White AcceptDouble
                                   , PlayerAction Black (Throw (2, 1))
                                   , PlayerAction Black (Moves [Move 1 2, Move 1 3])
                                   ] gameAfterInitialWhite31 @?=
      Right (ToDouble White)

  , testCase "the player cannot double if opponent owns the cube" $
      gameState <$> performActions [ PlayerAction Black Double
                                   , PlayerAction White AcceptDouble
                                   , PlayerAction Black (Throw (2, 1))
                                   , PlayerAction Black (Moves [Move 1 2, Move 1 3])
                                   , PlayerAction White (Throw (2, 1))
                                   , PlayerAction White (Moves [Move 24 23, Move 24 22])
                                   ] gameAfterInitialWhite31 @?=
      Right (ToThrow Black)

  ]

gameAfterInitialWhite31 = fromRight $ performActions 
  [ InitialThrows 3 1
  , PlayerAction White (Moves [Move 8 5, Move 6 5])
  ] newGame

whiteToMove21 = fromRight (performAction (InitialThrows 2 1) newGame)

fromRight :: Show a => Either a b -> b
fromRight (Right v) = v
fromRight (Left v) = error ("expected Right but got Left " ++ show v)
