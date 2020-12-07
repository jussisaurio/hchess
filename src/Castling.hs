module Castling (tryCastle, castlingAttempt, rookCastlingMove) where

import Board (Board, Move (..), allPieces, canReachSquare)
import Pieces

data Side = LeftSide | RightSide

tryCastle :: Board -> Piece -> [Move] -> Side -> Either String Piece
tryCastle board (Piece color pt) moves side
  | kingHasMoved moves color = Left "king has already moved, cannot castle"
  | rookhasMoved moves color side = Left "rook has already moved, cannot castle"
  | any (\(p, s) -> any (flip (canReachSquare board) s) criticalSquares) opponentPieces = Left "opponent is threatening square, cannot castle"
  | otherwise = Right piece
  where
    piece = Piece color pt
    opponentPieces = allPieces board $ otherColor color
    criticalSquares = castlingSpan color side

kingHasMoved :: [Move] -> Color -> Bool
kingHasMoved moves color =
  let startingSquare = if color == White then 4 else 60
   in any ((==) startingSquare . src) moves

castlingAttempt :: (Eq a1, Eq a2, Num a1, Num a2) => Piece -> a1 -> a2 -> Maybe Side
castlingAttempt (Piece color pt) src dst =
  if pt /= King
    then Nothing
    else case (color, src, dst) of
      (White, 4, 2) -> Just LeftSide
      (White, 4, 6) -> Just RightSide
      (Black, 60, 58) -> Just LeftSide
      (Black, 60, 62) -> Just RightSide
      _ -> Nothing

rookCastlingMove :: (Num a, Num b) => Color -> Side -> (a, b)
rookCastlingMove color side =
  case (color, side) of
    (White, LeftSide) -> (0, 3)
    (White, RightSide) -> (7, 5)
    (Black, LeftSide) -> (56, 59)
    (Black, RightSide) -> (63, 61)

rookhasMoved :: [Move] -> Color -> Side -> Bool
rookhasMoved moves color side = any ((==) startingSquare . src) moves
  where
    startingSquare = case (color, side) of
      (White, LeftSide) -> 0
      (White, RightSide) -> 7
      (Black, LeftSide) -> 56
      (Black, RightSide) -> 63

castlingSpan :: (Num a, Enum a) => Color -> Side -> [a]
castlingSpan color side =
  case (color, side) of
    (White, LeftSide) -> [0 .. 4]
    (White, RightSide) -> [4 .. 7]
    (Black, LeftSide) -> [56 .. 60]
    (Black, RightSide) -> [60 .. 63]