module Pieces where

data Color = Black | White deriving (Eq, Show)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)

data Piece = Piece Color PieceType deriving (Eq, Show)

otherColor :: Color -> Color
otherColor color = if color == White then Black else White

isOwnPiece :: Color -> Piece -> Either String Piece
isOwnPiece player (Piece color pt) = if player == color then Right (Piece color pt) else Left "Cannot move opponent's piece"