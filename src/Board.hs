{-# LANGUAGE LambdaCase #-}

module Board (Board, Move (..), nothingIsInTheWay, enpassant, canReachSquare, startingBoard, allPieces, findOpponentsThreateningSquare, isCheckOn, maybeCheck, destinationNotOccupiedByOwnPiece) where

import Data.Either (isRight)
import Data.List (find)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as Vector
import LegalMoves (legalCapturePatterns, steps, takeStep)
import Pieces
  ( Color (..),
    Piece (..),
    PieceType (Bishop, King, Knight, Pawn, Queen, Rook),
    otherColor,
  )

type Board = Vector.Vector (Maybe Piece)

data Move = Move
  { pc :: Piece,
    src :: Int,
    dst :: Int
  }
  deriving (Eq, Show)

startingBoard :: Board
startingBoard = Vector.fromList $ map (Just . Piece White) bigPiecesRow ++ map (Just . Piece White) pawnsRow ++ replicate 32 Nothing ++ map (Just . Piece Black) pawnsRow ++ map (Just . Piece Black) bigPiecesRow

allPieces :: Board -> Color -> [(Piece, Int)]
allPieces board color = map (\i -> (fromJust $ board ! i, i)) . Vector.toList $ Vector.findIndices (\mp -> isJust mp && let (Piece color' _) = fromJust mp in color == color') board

canReachSquare :: Board -> Int -> Int -> Bool
canReachSquare board dst idx = hasEligiblePattern && isRight (nothingIsInTheWay board idx dst piece)
  where
    piece = fromJust (board ! idx)
    capturePatterns = legalCapturePatterns piece
    hasEligiblePattern = any (\p -> p idx dst) capturePatterns

nothingIsInTheWay :: Board -> Int -> Int -> Piece -> Either [Char] Piece
nothingIsInTheWay board src dst (Piece color pt) =
  case pt of
    Knight -> Right piece
    _ -> if walk steps' src then Right piece else Left "Path is blocked"
  where
    piece = Piece color pt
    steps' = steps src dst - 1
    walk st cur = st == 0 || let next = takeStep cur dst in isNothing (board ! next) && walk (st - 1) next

enpassant :: Move -> Bool
enpassant move = pt == Pawn && pawnFirstMove && pawnDidDoubleMove
  where
    (Piece color pt) = pc move
    pawnFirstMove = src move `div` 8 == if color == White then 1 else 6
    pawnDidDoubleMove = pawnFirstMove && dst move `div` 8 == if color == White then 3 else 4

findOpponentsThreateningSquare :: Board -> Int -> [Piece]
findOpponentsThreateningSquare board sqr = map (fromJust . (!) board) . filter (canReachSquare board sqr) $ map snd opponentPieces
  where
    (Piece kingColor _) = fromJust $ board ! sqr
    opponentColor = otherColor kingColor
    opponentPieces = allPieces board opponentColor

destinationNotOccupiedByOwnPiece :: Vector (Maybe Piece) -> Int -> Piece -> Either [Char] Piece
destinationNotOccupiedByOwnPiece board dst (Piece color pt) =
  let _notOcc = \case
        Nothing -> Right (Piece color pt)
        Just (Piece color2 pt2) -> if color == color2 then Left $ "Square occupied by " ++ show (Piece color2 pt2) else Right (Piece color pt)
   in maybe (Left "off the board") _notOcc (board !? dst)

isCheckOn :: Board -> Color -> Bool
isCheckOn board player =
  (Just player ==) $ (\(Piece c _) -> c) . fromJust . (!) board <$> Vector.find ((<) 0 . length . findOpponentsThreateningSquare board) kingLocations
  where
    kingLocations = Vector.findIndices (\it -> isJust it && let (Piece c t) = fromJust it in t == King) board

maybeCheck :: Board -> Maybe Color
maybeCheck board = find (isCheckOn board) [White, Black]

bigPiecesRow :: [PieceType]
bigPiecesRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

pawnsRow :: [PieceType]
pawnsRow = replicate 8 Pawn