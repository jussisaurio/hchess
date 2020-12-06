{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Chess where

import Board (Board, startingBoard)
import Control.Monad.Except (MonadIO (liftIO))
import qualified Control.Monad.State as S
import Control.Monad.Trans.State (StateT, evalStateT, put)
import Data.Either (isRight)
import Data.List (find)
import Data.Maybe
import Data.Vector ((!), (!?))
import qualified Data.Vector as Vector
import Helpers
import LegalMoves
import Pieces
  ( Color (..),
    Piece (..),
    PieceType (King, Knight, Pawn),
  )
import Text.Read (readEither)
import qualified UI as UI

legalMovementPattern src dst piece
  | src == dst = Left "Cannot choose not to move"
  | dst < 0 || dst > 63 = Left "off the board"
  | any (\p -> p src dst) $ legalMovementPatterns piece = Right piece
  | otherwise = Left $ "illegal movement pattern for " ++ show piece

nothingIsInTheWay board src dst (Piece color pt) =
  case pt of
    Knight -> Right piece
    _ -> if walk steps' src then Right piece else Left "Path is blocked"
  where
    piece = Piece color pt
    steps' = steps src dst - 1
    walk st cur = st == 0 || let next = takeStep cur dst in isNothing (board ! next) && walk (st - 1) next

destinationNotOccupiedByOwnPiece board dst (Piece color pt) =
  let _notOcc = \case
        Nothing -> Right (Piece color pt)
        Just (Piece color2 pt2) -> if color == color2 then Left $ "Square occupied by " ++ show (Piece color2 pt2) else Right (Piece color pt)
   in maybe (Left "off the board") _notOcc (board !? dst)

isOwnPiece player (Piece color pt) = if player == color then Right (Piece color pt) else Left "Cannot move opponent's piece"

canReachSquare :: Board -> Int -> Int -> Bool
canReachSquare board dst idx = hasEligiblePattern && isRight (nothingIsInTheWay board idx dst piece)
  where
    piece = fromJust $ (board ! idx)
    capturePatterns = legalCapturePatterns piece
    hasEligiblePattern = any (\p -> p idx dst) capturePatterns

findOpponentsThreateningSquare :: Board -> Int -> [Piece]
findOpponentsThreateningSquare board sqr = Vector.toList $ Vector.map (fromJust . (!) board) . Vector.filter (canReachSquare board sqr) $ opponentIndices
  where
    (Piece kingColor _) = fromJust $ board ! sqr
    opponentColor = if kingColor == White then Black else White
    opponentIndices = Vector.findIndices (\sq -> isJust sq && let (Piece c t) = fromJust sq in c == opponentColor) board

isCheckOn :: Board -> Color -> Bool
isCheckOn board player =
  (Just player ==) $ (\(Piece c _) -> c) . fromJust . (!) board <$> Vector.find ((<) 0 . length . findOpponentsThreateningSquare board) kingLocations
  where
    kingLocations = Vector.findIndices (\it -> isJust it && let (Piece c t) = fromJust it in t == King) board

maybeCheck :: Board -> Maybe Color
maybeCheck board = find (isCheckOn board) [White, Black]

-- TODO Dirty, but it works for now. Refactor later
canMoveSomewhere board player = Vector.any (\src -> any (isRight . _move board player src) allSquares) pieceIndices
  where
    pieceIndices = Vector.findIndices (\mp -> isJust mp && let (Piece color _) = fromJust mp in color == player) board
    allSquares = [0 .. 63]

otherColor color = if color == White then Black else White

checkmate board = maybeCheck board >>= \player -> if not $ canMoveSomewhere board player then Just $ otherColor player else Nothing

stalemate board player = case maybeCheck board of
  (Just _) -> False
  Nothing -> not $ canMoveSomewhere board player

-- todo enpassant
canCapture board src dst (Piece color pt1) =
  let _canCapture = \case
        Nothing -> if pt1 /= Pawn || legalPawnNonCaptureMove (Piece color pt1) src dst then Right (Piece color pt1, Nothing) else Left "illegal move"
        Just (Piece color2 pt2) ->
          if pt2 == King
            then Left "Programmer error, should not be able to capture king in-game"
            else
              if pt1 /= Pawn || pawnCapture (Piece color pt1) src dst
                then Right (Piece color pt1, Just $ Piece color2 pt2)
                else Left "illegal capture"
   in maybe (Left "off the board") _canCapture (board !? dst)

moveDoesNotLeaveKingExposed board player src dst piece =
  if isCheckOn newBoard player
    then Left $ if isCheckOn board player then "There is a check on your king, you must neutralize the threat" else "Illegal move: player's own king would be exposed"
    else Right newBoard
  where
    newBoard = Vector.update board (Vector.fromList [(src, Nothing), (dst, Just piece)])

_move board player src dst =
  maybe (Left "illegal") Right (board !? src)
    >>= maybe (Left "no piece") Right
    >>= isOwnPiece player
    >>= legalMovementPattern src dst
    >>= nothingIsInTheWay board src dst
    >>= destinationNotOccupiedByOwnPiece board dst
    >>= canCapture board src dst
    >>= \(piece, maybeCapture) ->
      moveDoesNotLeaveKingExposed board player src dst piece
        >>= \newBoard -> Right (newBoard, maybeCapture)

toIndex sq =
  if length sq < 2
    then Left $ "illegal square " ++ sq
    else do
      col <- letterToNumber $ head sq
      row <- readEither (tail sq) >>= \r -> if r > 0 && r < 9 then Right r else Left $ "Invalid row " ++ show r
      pure ((row - 1) * 8 + col)

move board player src dst = do
  src' <- toIndex src
  dst' <- toIndex dst
  _move board player src' dst'

data Status = Winner Color | Stalemate | InProgress deriving (Eq, Show)

data GameState = GS {whoseTurn :: Color, board :: Board, result :: Status, currentTurn :: Integer, captured :: [Piece]} deriving (Eq, Show)

newGame = GS {whoseTurn = White, board = startingBoard, result = InProgress, currentTurn = 0, captured = []}

gameLoop :: StateT GameState IO Status
gameLoop = do
  state <- S.get
  _ <- liftIO $ UI.printBoard (board state)
  case checkmate (board state) of
    Just player -> return $ Winner player
    Nothing ->
      if stalemate (board state) (whoseTurn state)
        then return Stalemate
        else do
          src <- liftIO $ UI.askSourceSquare (whoseTurn state)
          dst <- liftIO $ UI.askDestinationSquare (whoseTurn state)
          case move (board state) (whoseTurn state) src dst of
            Left err -> do
              _ <- liftIO $ UI.printError err
              gameLoop
            Right (newBoard, maybeCapture) -> do
              put $ state {board = newBoard, whoseTurn = otherColor (whoseTurn state), captured = maybe (captured state) (: captured state) maybeCapture}
              gameLoop

play = evalStateT gameLoop newGame >>= print