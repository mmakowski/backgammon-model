module Backgammon
( Side (White, Black)
, Game
, GameAction (..)
, GameState (..)
, PlayerDecision (..)
, Move (..)
, newGame
, gameBoard
, gameState
, performAction
, performActions
, pipCount
)
where

import Backgammon.Model
