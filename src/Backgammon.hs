module Backgammon
where

type Pos = Int

type Die = Int

type Dice = (Die, Die)

type DoublingCubeValue = Int

data Side = While | Black
  deriving (Eq, Show)

data Move = Move Pos Pos
          | Enter Pos
          | TakeOff Pos
  deriving (Eq, Show)

data Game = Game Board [GameAction] Dice DoublingCube
  deriving (Eq, Show)

data Board = Board [Int]
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

perform :: PlayerDecision -> Game -> Either InvalidDecision Game
perform = error "TODO"

resultMultiplier :: Result -> Int
resultMultiplier Normal = 1
resultMultiplier Gammon = 2
resultMultiplier Backgammon = 3


