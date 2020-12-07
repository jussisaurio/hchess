{-# LANGUAGE LambdaCase #-}

module LegalMoves where

import Pieces

oneDown :: (Eq a, Num a) => a -> a -> Bool
oneDown src dst = src - dst == 8

diagonalOneDown :: Integral a => a -> a -> Bool
diagonalOneDown src dst = src `div` 8 - dst `div` 8 == 1 && abs (src `mod` 8 - dst `mod` 8) == 1

diagonalOneUp :: Integral a => a -> a -> Bool
diagonalOneUp src dst = src `div` 8 - dst `div` 8 == -1 && abs (src `mod` 8 - dst `mod` 8) == 1

twoDownBlackPawn :: Integral a => a -> a -> Bool
twoDownBlackPawn src dst = src `div` 8 == 6 && src - dst == 16

oneUp :: (Eq a, Num a) => a -> a -> Bool
oneUp src dst = dst - src == 8

twoUpWhitePawn :: Integral a => a -> a -> Bool
twoUpWhitePawn src dst = src `div` 8 == 1 && dst - src == 16

horizontalMove :: Integral a => a -> a -> Bool
horizontalMove src dst = src `div` 8 == dst `div` 8

verticalMove :: Integral a => a -> a -> Bool
verticalMove src dst = src `mod` 8 == dst `mod` 8

diagonalMove :: Integral a => a -> a -> Bool
diagonalMove src dst = abs (src `mod` 8 - dst `mod` 8) == abs (src `div` 8 - dst `div` 8)

kingMove :: Integral a => a -> a -> Bool
kingMove src dst = src /= dst && abs (src `div` 8 - dst `div` 8) < 2 && abs (src `mod` 8 - dst `mod` 8) < 2

knightMove :: Integral a => a -> a -> Bool
knightMove src dst = abs (src `div` 8 - dst `div` 8) == 2 && abs (src `mod` 8 - dst `mod` 8) == 1 || abs (src `div` 8 - dst `div` 8) == 1 && abs (src `mod` 8 - dst `mod` 8) == 2

legalPawnNonCaptureMove :: Integral a => Piece -> a -> a -> Bool
legalPawnNonCaptureMove (Piece color Pawn) src dst = if color == White then oneUp src dst || twoUpWhitePawn src dst else oneDown src dst || twoDownBlackPawn src dst

pawnCapture :: Integral a => Piece -> a -> a -> Bool
pawnCapture (Piece color pt) src dst = pt == Pawn && if color == White then diagonalOneUp src dst else diagonalOneDown src dst

getUnitVector :: Integral a => a -> a -> (a, a)
getUnitVector src dst = (if xdiff == 0 then xdiff else xdiff `div` abs xdiff, if ydiff == 0 then ydiff else ydiff `div` abs ydiff)
  where
    xdiff = dst `mod` 8 - src `mod` 8
    ydiff = dst `div` 8 - src `div` 8

combineTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
combineTuple (f1, s1) (f2, s2) = (f1 + f2, s1 + s2)

upUV :: (Int, Int)
upUV = (0, 1)

downUV :: (Int, Int)
downUV = (0, -1)

leftUV :: (Int, Int)
leftUV = (-1, 0)

rightUV :: (Int, Int)
rightUV = (1, 0)

nwUV :: (Int, Int)
nwUV = combineTuple leftUV upUV

neUV :: (Int, Int)
neUV = combineTuple rightUV upUV

swUV :: (Int, Int)
swUV = combineTuple leftUV downUV

seUV :: (Int, Int)
seUV = combineTuple rightUV downUV

diagonalUVs :: [(Int, Int)]
diagonalUVs = [neUV, nwUV, seUV, swUV]

horizontalUVs :: [(Int, Int)]
horizontalUVs = [leftUV, rightUV]

verticalUVs :: [(Int, Int)]
verticalUVs = [upUV, downUV]

knightUVs :: [(Int, Int)]
knightUVs = [(-2, 1), (-2, -1), (2, 1), (2, -1), (1, -2), (1, 2), (-1, -2), (-1, 2)]

unitVectorsFor :: Piece -> Bool -> ([(Int, Int)], Bool)
unitVectorsFor (Piece color pt) isUnmovedPawn =
  case (color, pt) of
    (White, Pawn) -> if not isUnmovedPawn then ([upUV, neUV, nwUV], False) else ([upUV, neUV, nwUV, (0, 2)], False)
    (Black, Pawn) -> if not isUnmovedPawn then ([downUV, seUV, swUV], False) else ([downUV, seUV, swUV, (0, -2)], False)
    (_, Rook) -> (horizontalUVs ++ verticalUVs, True)
    (_, Knight) -> (knightUVs, False)
    (_, Bishop) -> (diagonalUVs, True)
    (_, Queen) -> (horizontalUVs ++ verticalUVs ++ diagonalUVs, True)
    (_, King) -> (horizontalUVs ++ verticalUVs ++ diagonalUVs, False)

takeStep :: Integral a => a -> a -> a
takeStep src dst = let (xstep, ystep) = getUnitVector src dst in src + xstep + ystep * 8

steps :: (Num t1, Integral t2) => t2 -> t2 -> t1
steps src dst =
  let go s t = if s == dst then t else go (takeStep s dst) (t + 1) in go src 0

legalMovementPatterns :: Integral a => Piece -> [a -> a -> Bool]
legalMovementPatterns (Piece c t) =
  case (c, t) of
    (White, Pawn) -> [diagonalOneUp, oneUp, twoUpWhitePawn]
    (Black, Pawn) -> [diagonalOneDown, oneDown, twoDownBlackPawn]
    (_, Rook) -> [horizontalMove, verticalMove]
    (_, Knight) -> [knightMove]
    (_, Bishop) -> [diagonalMove]
    (_, Queen) -> [horizontalMove, verticalMove, diagonalMove]
    (_, King) -> [kingMove]

legalCapturePatterns :: Integral a => Piece -> [a -> a -> Bool]
legalCapturePatterns (Piece c t) =
  case (c, t) of
    (White, Pawn) -> [diagonalOneUp]
    (Black, Pawn) -> [diagonalOneDown]
    (_, Rook) -> [horizontalMove, verticalMove]
    (_, Knight) -> [knightMove]
    (_, Bishop) -> [diagonalMove]
    (_, Queen) -> [horizontalMove, verticalMove, diagonalMove]
    (_, King) -> [kingMove]