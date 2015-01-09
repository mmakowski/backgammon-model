module Backgammon
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

data Game = Game Board [GameAction] Dice DoublingCube
  deriving (Eq, Show)

data Board = Board [Maybe (Side, Int)]
  deriving (Eq, Show)

data DoublingCube = DoublingCube (Maybe Side) DoublingCubeValue
  deriving (Eq, Show)

data Result = Normal | Gammon | Backgammon
  deriving (Eq, Show)

data GameAction = PlayerAction PlayerDecision
                | Throw Dice
                | InitialThrow Side Die
  deriving (Eq, Show)

data PlayerDecision = Moves [Move]
                    | Double
                    | AcceptDouble
                    | RejectDouble
                    | Resign Result
                    | AcceptResign
                    | RejectResign
  deriving (Eq, Show)

data InvalidDecision = InvalidDecision Game PlayerDecision InvalidDecisionType
  deriving (Eq, Show)

data InvalidDecisionType = MustEnterBeforeMoving
                         | MovePossible
  deriving (Eq, Show)

newGame :: Game
newGame = Game initialBoard [] (1,1) initialDoublingCube

initialBoard :: Board
initialBoard = Board [ Just (Black, 5), Nothing, Nothing, Nothing, Just (White, 3), Nothing , Just (White, 5), Nothing, Nothing, Nothing, Nothing, Just (Black, 2)
                     , Just (White, 5), Nothing, Nothing, Nothing, Just (Black, 3), Nothing , Just (Black, 5), Nothing, Nothing, Nothing, Nothing, Just (White, 2)
                     ]

initialDoublingCube :: DoublingCube
initialDoublingCube = DoublingCube Nothing 1

pipDists :: Side -> [Int]
pipDists White = reverse [1..12] ++ [13..24]
pipDists Black = [13..24] ++ reverse [1..12]

pipCount :: Side -> Game -> Int
pipCount side (Game (Board poss) _ _ _) = sum $ zipWith count (pipDists side) poss
  where
    count dist (Just (s, n)) | s == side = n * dist
    count _    _                         = 0

perform :: PlayerDecision -> Game -> Either InvalidDecision Game
perform = error "TODO"

resultMultiplier :: Result -> Int
resultMultiplier Normal = 1
resultMultiplier Gammon = 2
resultMultiplier Backgammon = 3


