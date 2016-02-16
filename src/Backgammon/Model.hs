module Backgammon.Model
( Side (White, Black)
, Game
, Board (..) -- TODO: smart constructor and accessors
, DoublingCube
, GameAction (..)
, GameState (..)
, PlayerDecision (..)
, Move (..)
, InvalidAction (..)
, InvalidDecisionType (..)
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

import Control.Monad (foldM)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

type Pos = Int

type Die = Int

type Dice = (Die, Die)

type DoublingCubeValue = Int

data Side = White | Black
  deriving (Eq, Show)

data Move = Move Pos Pos
          | Enter Pos
          | TakeOff Pos
  deriving (Eq, Show)

data Game = Game { gameBoard :: Board
                 , _gameActions :: [GameAction]
                 , gameDoublingCube :: DoublingCube
                 , gameState :: GameState
                 }
  deriving (Eq, Show)

data Board 
  -- | fields, bar white, bar black
  = Board [Maybe (Side, Int)] Int Int 
  deriving (Eq, Show)

data DoublingCube = DoublingCube { doublingCubeOwner :: Maybe Side
                                 , doublingCubeValue :: DoublingCubeValue
                                 }
  deriving (Eq, Show)

data Result = Normal | Gammon | Backgammon
  deriving (Eq, Show)

data GameAction = PlayerAction Side PlayerDecision
                | InitialThrows Die Die
  deriving (Eq, Show)

data PlayerDecision = Moves [Move]
                    | Throw Dice
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
               | ToRespondToDouble Side
               | ToThrow Side
               -- | winner, points scored
               | GameFinished Side Int
  deriving (Eq, Show)

data InvalidDecisionType = MustEnterBeforeMoving
                         | MovePossible
                         | NoPieces Pos
                         | MustMoveOwnPieces Side
  deriving (Eq, Show)

data InvalidAction = ActionInvalidForState GameState GameAction
                   | InvalidPlayerDecision Game PlayerDecision InvalidDecisionType
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

move :: Side -> Board -> Move -> Either InvalidDecisionType Board
move side board (Move from to) = 
  case pieces board from of
    Just (s, _) -> if s == side then decPieces from board >>= incPieces to side 
                                else Left (MustMoveOwnPieces side)
    Nothing     -> Left (NoPieces from) -- TODO: test this

decPieces :: Pos -> Board -> Either InvalidDecisionType Board
decPieces pos board@(Board b bw bb) =
  case pieces board pos of
    Just (s, n) -> Right (Board (take (pos-1) b ++ [Just (s, n-1)] ++ drop pos b) bw bb)
    Nothing     -> Left (NoPieces pos)

incPieces :: Pos -> Side -> Board -> Either InvalidDecisionType Board
incPieces pos side board@(Board b bw bb) =
  -- TODO: Left when owner of target is different from side
  Right (Board (take (pos-1) b ++ [Just (side, updatedCount)] ++ drop pos b) bw bb)
  where
    updatedCount =
      case pieces board pos of
        Just (_, n) -> n+1
        Nothing     -> 1

pieces :: Board -> Pos -> Maybe (Side, Int)
pieces (Board board _ _) pos = board !! (pos-1)

performAction :: GameAction -> Game -> Either InvalidAction Game
performAction a@(InitialThrows white black)    game@(Game { gameState = PlayersToThrowInitial }) =
  success game (if white /= black then ToMove side (normDice (white, black))
                else                   PlayersToThrowInitial) a
  where
    side = if white > black then White else Black
performAction a@(PlayerAction aSide d@(Moves moves)) game@(Game { gameState = ToMove side _ })          | aSide == side =
  updatedBoard >>= \b ->
    success (game { gameBoard = b }) (nextState (opposite side)) a
  where
    -- TODO: verify that moves own pieces
    -- TODO: verify that moves by the right numbers
    updatedBoard = first (InvalidPlayerDecision game d) (foldM (move side) (gameBoard game) moves)
    nextState = if not (ownsCube side) then ToDouble else ToThrow
    ownsCube side = (doublingCubeOwner . gameDoublingCube) game == Just side 
performAction a@(PlayerAction aSide (Throw dice))    game@(Game { gameState = ToDouble side })          | aSide == side =
  success game (ToMove side (normDice dice)) a
performAction a@(PlayerAction aSide Double)          game@(Game { gameState = ToDouble side })          | aSide == side =
  success game (ToRespondToDouble (opposite side)) a
performAction a@(PlayerAction aSide AcceptDouble)    game@(Game { gameState = ToRespondToDouble side }) | aSide == side =
  success (game { gameDoublingCube = acceptDouble side (gameDoublingCube game) }) (ToThrow (opposite side)) a
performAction a@(PlayerAction aSide RejectDouble)    game@(Game { gameState = ToRespondToDouble side }) | aSide == side =
  success game (GameFinished (opposite side) ((doublingCubeValue . gameDoublingCube) game)) a
performAction a@(PlayerAction aSide (Throw dice))    game@(Game { gameState = ToThrow side })           | aSide == side =
  success game (ToMove side (normDice dice)) a
performAction action                                 game =
  Left (ActionInvalidForState (gameState game) action)

performActions :: [GameAction] -> Game -> Either InvalidAction Game
performActions actions game = foldl' (\eg a -> eg >>= performAction a) (Right game) actions -- TODO: use foldM?

success :: Game -> GameState -> GameAction -> Either InvalidAction Game
success game state action =
  Right ((appendAction action . moveToState state) game)

moveToState :: GameState -> Game -> Game
moveToState state game = game { gameState = state }

appendAction :: GameAction -> Game -> Game
appendAction action game = game { _gameActions = _gameActions game ++ [action] }

pipDists :: Side -> [Int]
pipDists White = [1..24]
pipDists Black = reverse [1..24]

pipCount :: Side -> Board -> Int
pipCount side (Board poss barWhite barBlack) = bar + sum (zipWith count (pipDists side) poss)
  where
    count dist (Just (s, n)) | s == side = n * dist
    count _    _                         = 0
    bar = 
      25 * case side of
        White -> barWhite
        Black -> barBlack

acceptDouble :: Side -> DoublingCube -> DoublingCube
acceptDouble side (DoublingCube _ value) = DoublingCube (Just side) (value * 2)

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

