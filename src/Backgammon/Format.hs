module Backgammon.Format
( BoardParserState (..)
, BoardParseError (..)
, BoardParseErrorType (..)
, parseBoard
)
where

import Backgammon.Model

data BoardParserState = Start String Int
                      | Contents [Maybe (Side, Int)] String Int
                      | PieceCount [Maybe (Side, Int)] Side String Int
                      | PieceCountCont [Maybe (Side, Int)] Side String String Int
                      | Done [Maybe (Side, Int)]
                      | Error BoardParseErrorType
  deriving (Eq, Show)

data BoardParseError = BoardParseError String BoardParseErrorType
  deriving (Eq, Show)

data BoardParseErrorType = InvalidParserStateAtEnd BoardParserState
                         | UnexpectedCharacter Int Char [Char] -- pos, actual, expected
                         | UnexpectedEndOfInput Int BoardParserState -- pos, state at end
                         | InsufficientPointsSpecified Int Int -- pos, actual points
  deriving (Eq, Show)

-- TODO: docs
parseBoard :: String -> Either BoardParseError Board
parseBoard str = 
  case parse (Start str 1) of
    Done b  -> Right (Board b 0 0)
    Error t -> Left (BoardParseError str t)
    state   -> Left (BoardParseError str (InvalidParserStateAtEnd state))
  where
    parse :: BoardParserState -> BoardParserState
    parse (Start (h:t) pos) = 
      case h of
        '|' -> parse (Contents [] t (pos+1))
        c   -> Error (UnexpectedCharacter pos c "|")
    parse (Contents board [] pos) =
      if length board == 24 then Done board
      else                       Error (InsufficientPointsSpecified pos (length board))
    parse (Contents board (h:t) pos) =
      case h of
        '|' -> parse (Contents board t (pos+1))
        '.' -> parse (Contents (board ++ [Nothing]) t (pos+1))
        'w' -> parse (PieceCount board White t (pos+1))
        'b' -> parse (PieceCount board Black t (pos+1))
        c   -> Error (UnexpectedCharacter pos c ".wb")
    parse state@(PieceCount _ _ [] pos) =
      Error (UnexpectedEndOfInput pos state)
    parse (PieceCount board side (h:t) pos) =
      if isDigit h then parse (PieceCountCont board side [h] t (pos+1))
      else              Error (UnexpectedCharacter pos h digits)
    parse state@(PieceCountCont _ _ _ [] pos) =
      Error (UnexpectedEndOfInput pos state)
    parse (PieceCountCont board side acc (h:t) pos) =
      if isDigit h            then parse (PieceCountCont board side (acc ++ [h]) t (pos+1))
      else if any (== h) ".|wb" then parse (Contents (board ++ [Just (side, read acc)]) (h:t) pos)
      else              Error (UnexpectedCharacter pos h (digits ++ ".|wb"))
    digits = "0123456789"
    isDigit c = any (== c) digits -- TODO: use ascii codes (more efficient)
