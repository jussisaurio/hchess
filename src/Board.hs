module Board (Board, startingBoard) where

import qualified Data.Vector as Vector
import Pieces
  ( Color (..),
    Piece (..),
    PieceType (Bishop, King, Knight, Pawn, Queen, Rook),
  )

type Board = Vector.Vector (Maybe Piece)

bigPiecesRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

pawnsRow = replicate 8 Pawn

startingBoard :: Board
startingBoard = Vector.fromList $ map (Just . Piece White) bigPiecesRow ++ map (Just . Piece White) pawnsRow ++ replicate 32 Nothing ++ map (Just . Piece Black) pawnsRow ++ map (Just . Piece Black) bigPiecesRow