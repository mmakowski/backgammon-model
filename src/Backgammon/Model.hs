module Backgammon.Model
( Side (White, Black)
, Game
, Board (..) -- TODO: smart constructor and accessors
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

data GameAction = PlayerAction PlayerDecision
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
  if pieceCount b from side == 0 then Left (NoPieces from) -- TODO: test this
  else decPieces from b >>= incPieces to side

decPieces :: Pos -> Board -> Either InvalidDecisionType Board
decPieces pos board@(Board b bw bb) =
  case pieces board pos of
    Just (s, n) -> Right (Board (take (pos-1) b ++ [Just (s, n-1)] ++ drop pos b) bw bb)
    Nothing     -> Left (NoPieces pos)

incPieces :: Pos -> Side -> Board -> Either InvalidDecisionType Board
incPieces pos side board@(Board b bw bb) = 
  Right (Board (take (pos-1) b ++ [Just (side, updatedCount)] ++ drop pos b) bw bb)
  where 
    updatedCount =
      case pieces board pos of
        Just (_, n) -> n+1
        Nothing     -> 1

pieceCount :: Board -> Pos -> Side -> Int
pieceCount board pos side = 
  case pieces board pos of
    Just (s, n) -> if s == side then n else 0
    Nothing     -> 0

pieces :: Board -> Pos -> Maybe (Side, Int)
pieces (Board board _ _) pos = board !! (pos-1)

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
performAction a@(PlayerAction (Throw dice)) game =
  case gameState game of
    ToDouble side ->
      Right $ game { _gameActions = _gameActions game ++ [a]
                   , gameState    = ToMove side (normDice dice)
                   }
    s             ->
      Left (ActionInvalidForState s a)
performAction a@(PlayerAction Double) game =
  case gameState game of
    ToDouble side ->
      Right $ game { _gameActions = _gameActions game ++ [a]
                   , gameState    = ToRespondToDouble (opposite side)
                   }
    s              ->
      Left (ActionInvalidForState s a)
performAction a@(PlayerAction AcceptDouble) game =
  case gameState game of
    ToRespondToDouble side ->
      Right $ game { _gameActions = _gameActions game ++ [a]
                   , gameDoublingCube = acceptDouble side (gameDoublingCube game)
                   , gameState    = ToThrow (opposite side)
                   }
    s              ->
      Left (ActionInvalidForState s a)

performActions :: [GameAction] -> Game -> Either InvalidAction Game
performActions actions game = foldl' (\eg a -> eg >>= performAction a) (Right game) actions -- TODO: use foldM?

pipDists :: Side -> [Int]
pipDists White = [1..24]
pipDists Black = reverse [1..24]

pipCount :: Side -> Board -> Int
pipCount side (Board poss barWhite barBlack) = bar + (sum (zipWith count (pipDists side) poss))
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

