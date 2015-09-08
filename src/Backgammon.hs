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
, parseBoard
, pipCount
)
where

import Control.Monad (foldM)
import Data.List (foldl')

type Pos = Int

type Die = Int

type Dice = (Die, Die)

type DoublingCubeValue = Int

data Side = White | Black
  deriving (Eq, Show)

data Move = Move Side Pos Pos
          | Enter Side Pos
          | TakeOff Side Pos
  deriving (Eq, Show)

data Game = Game { gameBoard :: Board
                 , _gameActions :: [GameAction]
                 , _gameDoublingCube :: DoublingCube
                 , gameState :: GameState
                 }
  deriving (Eq, Show)

data Board = Board [Maybe (Side, Int)] Int Int
  deriving (Eq, Show)

data DoublingCube = DoublingCube (Maybe Side) DoublingCubeValue
  deriving (Eq, Show)

data Result = Normal | Gammon | Backgammon
  deriving (Eq, Show)

data GameAction = PlayerAction PlayerDecision
                | Throw Dice
                | InitialThrows Die Die
  deriving (Eq, Show)

data PlayerDecision = Moves [Move]
                    | Double
                    | AcceptDouble
                    | RejectDouble
                    | Resign Result
                    | AcceptResign
                    | RejectResign
  deriving (Eq, Show)

data GameState = PlayersToThrowInitial
               | ToMove Side Dice
               | ToDouble Side
  deriving (Eq, Show)

data InvalidDecision = InvalidDecision Game PlayerDecision InvalidDecisionType
  deriving (Eq, Show)

data InvalidDecisionType = MustEnterBeforeMoving
                         | MovePossible
                         | NoPieces Pos
  deriving (Eq, Show)

data InvalidAction = ActionInvalidForState GameState GameAction
                   | InvalidPlayerDecision InvalidDecision
  deriving (Eq, Show)

newGame :: Game
newGame = Game initialBoard [] initialDoublingCube PlayersToThrowInitial

initialBoard :: Board
initialBoard = Board [ Just (Black, 2), Nothing, Nothing, Nothing, Nothing, Just (White, 5), Nothing, Just (White, 3), Nothing, Nothing, Nothing, Just (Black, 5)
                     , Just (White, 5), Nothing, Nothing, Nothing, Just (Black, 3), Nothing, Just (Black, 5), Nothing, Nothing, Nothing, Nothing, Just (White, 2)
                     ]
                     0
                     0

initialDoublingCube :: DoublingCube
initialDoublingCube = DoublingCube Nothing 1

normDice :: Dice -> Dice
normDice (d1, d2) = if d1 > d2 then (d1, d2) else (d2, d1)

opposite :: Side -> Side
opposite White = Black
opposite Black = White

move :: Board -> Move -> Either InvalidDecisionType Board
move b@(Board board _ _) (Move side from to) = 
  if pieceCount b from side == 0 then Left (NoPieces from) 
  else error "TODO: move" 

pieceCount :: Board -> Pos -> Side -> Int
pieceCount (Board b _ _) pos side = 
  case b !! pos of
    Just (s, n) -> if s == side then n else 0
    Nothing     -> 0

performAction :: GameAction -> Game -> Either InvalidAction Game
performAction a@(InitialThrows white black) game =
  Right $ game { _gameActions = _gameActions game ++ [a]
               , gameState    = if white /= black then ToMove side (normDice (white, black))
                                else                   PlayersToThrowInitial
               }
  where
    side = if white > black then White else Black
performAction a@(PlayerAction d@(Moves moves)) game =
  case gameState game of
    ToMove side _ ->
      updatedBoard >>= \b ->
        Right $ game { _gameActions = _gameActions game ++ [a]
                     , gameBoard    = b
                     , gameState    = ToDouble (opposite side) -- TODO: only if owns the cube
                     }
    s             ->
      Left (ActionInvalidForState s a)
  where
    updatedBoard = first (InvalidPlayerDecision . InvalidDecision game d) (foldM move (gameBoard game) moves)

performActions :: [GameAction] -> Game -> Either InvalidAction Game
performActions actions game = foldl' (\eg a -> eg >>= performAction a) (Right game) actions -- TODO: use foldM?

parseBoard :: String -> Maybe Board
parseBoard = error "TODO: parse"

pipDists :: Side -> [Int]
pipDists White = [1..24]
pipDists Black = reverse [1..24]

pipCount :: Side -> Game -> Int
pipCount side (Game (Board poss _ _) _ _ _) = sum $ zipWith count (pipDists side) poss
  where
    count dist (Just (s, n)) | s == side = n * dist
    count _    _                         = 0

-- TODO: replace with Data.Bifunctor (first) once we move to base 4.8+
first :: (a -> c) -> Either a b -> Either c b
first f (Left l)  = Left (f l)
first _ (Right r) = Right r

{-
perform :: PlayerDecision -> Game -> Either InvalidDecision Game
perform = error "TODO"

resultMultiplier :: Result -> Int
resultMultiplier Normal = 1
resultMultiplier Gammon = 2
resultMultiplier Backgammon = 3
-}

