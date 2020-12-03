module Pieces where

data Color = Black | White deriving (Eq, Show)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)

data Piece = Piece Color PieceType deriving (Eq, Show)