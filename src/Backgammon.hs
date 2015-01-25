module Backgammon
( Side (White, Black)
, Game
, GameAction (..)
, GameState (..)
, newGame
, gameState
, performAction
, pipCount
)
where

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

data Game = Game { _gameBoard :: Board
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
  deriving (Eq, Show)

data InvalidDecision = InvalidDecision Game PlayerDecision InvalidDecisionType
  deriving (Eq, Show)

data InvalidDecisionType = MustEnterBeforeMoving
                         | MovePossible
  deriving (Eq, Show)

data InvalidAction = ActionInvalidForState GameState GameAction
                   | InvalidPlayerDecision InvalidDecision
  deriving (Eq, Show)

newGame :: Game
newGame = Game initialBoard [] initialDoublingCube PlayersToThrowInitial

initialBoard :: Board
initialBoard = Board [ Just (Black, 2), Nothing, Nothing, Nothing, Nothing, Just (White, 5), Nothing, Just ( White, 3), Nothing, Nothing, Nothing, Just (Black, 5)
                     , Just (White, 5), Nothing, Nothing, Nothing, Just (Black, 3), Nothing , Just (Black, 5), Nothing, Nothing, Nothing, Nothing, Just (White, 2)
                     ]
                     0
                     0

initialDoublingCube :: DoublingCube
initialDoublingCube = DoublingCube Nothing 1

normDice :: Dice -> Dice
normDice (d1, d2) = if d1 > d2 then (d1, d2) else (d2, d1)

performAction :: GameAction -> Game -> Either InvalidAction Game
performAction a@(InitialThrows white black) game =
  Right $ game { _gameActions = _gameActions game ++ [a]
               , gameState    = ToMove side (normDice (white, black))
               }
  where
    side = if white > black then White else Black

pipDists :: Side -> [Int]
pipDists White = [1..24]
pipDists Black = reverse [1..24]

pipCount :: Side -> Game -> Int
pipCount side (Game (Board poss _ _) _ _ _) = sum $ zipWith count (pipDists side) poss
  where
    count dist (Just (s, n)) | s == side = n * dist
    count _    _                         = 0

{-
perform :: PlayerDecision -> Game -> Either InvalidDecision Game
perform = error "TODO"

resultMultiplier :: Result -> Int
resultMultiplier Normal = 1
resultMultiplier Gammon = 2
resultMultiplier Backgammon = 3
-}

