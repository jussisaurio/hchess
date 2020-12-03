module LegalMoves where

import Pieces

oneDown src dst = src - dst == 8

diagonalOneDown src dst = src `div` 8 - dst `div` 8 == 1 && abs (src `mod` 8 - dst `mod` 8) == 1

diagonalOneUp src dst = src `div` 8 - dst `div` 8 == -1 && abs (src `mod` 8 - dst `mod` 8) == 1

twoDownBlackPawn src dst = src `div` 8 == 6 && src - dst == 16

oneUp src dst = dst - src == 8

twoUpWhitePawn src dst = src `div` 8 == 1 && dst - src == 16

horizontalMove src dst = src `div` 8 == dst `div` 8

verticalMove src dst = src `mod` 8 == dst `mod` 8

diagonalMove src dst = src `mod` 8 - dst `mod` 8 == src `div` 8 - dst `div` 8

kingMove src dst = src /= dst && abs (src `div` 8 - dst `div` 8) < 2 && abs (src `mod` 8 - dst `mod` 8) < 2

knightMove src dst = abs (src `div` 8 - dst `div` 8) == 2 && abs (src `mod` 8 - dst `mod` 8) == 1 || abs (src `div` 8 - dst `div` 8) == 1 && abs (src `mod` 8 - dst `mod` 8) == 2

pawnCapture (Piece color pt) src dst = pt == Pawn && if color == White then diagonalOneUp src dst else diagonalOneDown src dst