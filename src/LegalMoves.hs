{-# LANGUAGE LambdaCase #-}

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

diagonalMove src dst = abs (src `mod` 8 - dst `mod` 8) == abs (src `div` 8 - dst `div` 8)

kingMove src dst = src /= dst && abs (src `div` 8 - dst `div` 8) < 2 && abs (src `mod` 8 - dst `mod` 8) < 2

knightMove src dst = abs (src `div` 8 - dst `div` 8) == 2 && abs (src `mod` 8 - dst `mod` 8) == 1 || abs (src `div` 8 - dst `div` 8) == 1 && abs (src `mod` 8 - dst `mod` 8) == 2

pawnCapture (Piece color pt) src dst = pt == Pawn && if color == White then diagonalOneUp src dst else diagonalOneDown src dst

getUnitVector src dst = (if xdiff == 0 then xdiff else xdiff `div` abs xdiff, if ydiff == 0 then ydiff else ydiff `div` abs ydiff)
  where
    xdiff = dst `mod` 8 - src `mod` 8
    ydiff = dst `div` 8 - src `div` 8

combineTuple (f1, s1) (f2, s2) = (f1 + f2, s1 + s2)

upUV = (0, 1)

downUV = (0, -1)

leftUV = (0, -1)

rightUV = (0, 1)

neUV = combineTuple leftUV upUV

nwUV = combineTuple rightUV upUV

seUV = combineTuple leftUV downUV

swUV = combineTuple rightUV downUV

diagonalUVs = [neUV, nwUV, seUV, swUV]

horizontalUVs = [leftUV, rightUV]

verticalUVs = [upUV, downUV]

horizontalUnitVector = (1, 0)

takeStep src dst = let (xstep, ystep) = getUnitVector src dst in src + xstep + ystep * 8

steps src dst =
  let go s t = if s == dst then t else go (takeStep s dst) (t + 1) in go src 0

legalCapturePatterns (Piece c t) =
  case (c, t) of
    (White, Pawn) -> [diagonalOneUp]
    (Black, Pawn) -> [diagonalOneDown]
    (_, Rook) -> [horizontalMove, verticalMove]
    (_, Knight) -> [knightMove]
    (_, Bishop) -> [diagonalMove]
    (_, Queen) -> [horizontalMove, verticalMove, diagonalMove]
    (_, King) -> [kingMove]