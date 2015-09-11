module Backgammon.FormatTests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Backgammon (gameBoard, newGame)
import Backgammon.Format (parseBoard)


unitTests :: TestTree
unitTests = testGroup "Backgammon.Format unit tests"
  [ testCase "initial board can be parsed" $
      parseBoard "|b2....w5|.w3...b5|w5...b3.|b5....w2|" @?= Right (gameBoard newGame)
  ]
