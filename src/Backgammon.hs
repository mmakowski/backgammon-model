module Backgammon
( Side (White, Black)
, Game
, DoublingCube
, GameAction (..)
, GameState (..)
, PlayerDecision (..)
, Move (..)
, newGame
, gameBoard
, gameDoublingCube
, gameState
, doublingCubeOwner
, doublingCubeValue
, performAction
, performActions
, pipCount
)
where

import Backgammon.Model
