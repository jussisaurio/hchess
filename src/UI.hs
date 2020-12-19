module UI (askSourceSquare, askDestinationSquare, askPromotion, printError, printBoard) where

import Data.Char (toUpper)
import qualified Data.Vector as Vector
import Pieces
  ( Color (..),
    Piece (..),
    PieceType (Bishop, King, Knight, Pawn, Queen, Rook),
  )

printBoard board = mapM_ (print . getPrintableRow) (getRowsTopFirst board)

askSourceSquare player = getInputWithPrompt $ show player ++ ", which square to move: "

askDestinationSquare player = getInputWithPrompt $ show player ++ ", target square: "

askPromotion player = getInputWithPrompt $ show player ++ ", choose what to promote to (Q, R, B, K): "

printError err = print $ "Error: " ++ err

getInputWithPrompt prompt = putStrLn prompt >> getLine

getCharForPiece mp =
  case mp of
    Nothing -> '.'
    Just (Piece color pt) ->
      let upperWhite clr ch = if clr == White then toUpper ch else ch
       in upperWhite color $ case pt of
            Pawn -> 'p'
            Rook -> 'r'
            Knight -> 'n'
            Bishop -> 'b'
            Queen -> 'q'
            King -> 'k'

getPrintableRow = map getCharForPiece

getRowsTopFirst board =
  let getRow n = Vector.toList $ Vector.slice (n * 8) 8 board
   in reverse $ map getRow [0 .. 7]