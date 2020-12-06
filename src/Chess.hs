{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Chess where

import Board (Board, startingBoard)
import Control.Applicative (Alternative ((<|>)))
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
import qualified UI

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

tryMove state piece src unrestricted (xdiff, ydiff) =
  let go dst' acc
        | dst' < 0 || dst' > 63 = acc
        | isRight $ _move state src dst' =
          if unrestricted
            then go (dst' + (ydiff * 8) + xdiff) ((piece, src, dst') : acc)
            else [(piece, src, dst')]
        | otherwise = acc
   in go (src + (ydiff * 8) + xdiff) []

possibleMovesFor state (Piece color pt) src =
  let (vectors, unrestricted) = unitVectorsFor piece isUnmovedPawn
      isUnmovedPawn = pt == Pawn && src `div` 8 == if color == White then 1 else 6
      piece = Piece color pt
      moves = concatMap (tryMove state piece src unrestricted) vectors
   in moves

-- TODO Dirty, but it works for now. Refactor later
canMoveSomewhere state = (<) 0 . length $ Vector.filter (\(pc, src) -> not $ null $ possibleMovesFor state pc src) piecesAndSquares
  where
    board' = board state
    player' = whoseTurn state
    piecesAndSquares = Vector.map (\i -> (fromJust $ board' ! i, i)) . Vector.findIndices (\mp -> isJust mp && let (Piece color _) = fromJust mp in color == player') $ board'

otherColor color = if color == White then Black else White

checkmate state = maybeCheck (board state) >>= \player -> if not $ canMoveSomewhere state then Just $ otherColor player else Nothing

stalemate state = case maybeCheck (board state) of
  (Just _) -> False
  Nothing -> not $ canMoveSomewhere state

tryEnpassant (Piece color pt1) maybeEnpassant src' dst' =
  case maybeEnpassant of
    Nothing -> Left "enpassant not possible"
    Just mov ->
      let acceptedPattern = if color == White then diagonalOneUp src' dst' else diagonalOneDown src' dst'
          correctLocation = if color == White then oneUp (dst mov) dst' else oneDown (dst mov) dst'
       in if acceptedPattern && correctLocation then Right (Piece color pt1, Just (pc mov, dst mov)) else Left "enpassant not possible"

canCapture board maybeEnpassant src dst (Piece color pt1) =
  let _canCapture = \case
        Nothing ->
          if pt1 /= Pawn || legalPawnNonCaptureMove (Piece color pt1) src dst
            then Right (Piece color pt1, Nothing)
            else tryEnpassant (Piece color pt1) maybeEnpassant src dst <|> Left "illegal move"
        Just (Piece color2 pt2) ->
          if pt2 == King
            then Left "Programmer error, should not be able to capture king in-game"
            else
              if pt1 /= Pawn || pawnCapture (Piece color pt1) src dst
                then Right (Piece color pt1, Just (Piece color2 pt2, dst))
                else Left "illegal capture"
   in maybe (Left "off the board") _canCapture (board !? dst)

moveDoesNotLeaveKingExposed board player src dst piece =
  if isCheckOn newBoard player
    then Left $ if isCheckOn board player then "There is a check on your king, you must neutralize the threat" else "Illegal move: player's own king would be exposed"
    else Right newBoard
  where
    newBoard = Vector.update board (Vector.fromList [(src, Nothing), (dst, Just piece)])

enpassant move = pt == Pawn && pawnFirstMove && pawnDidDoubleMove
  where
    (Piece color pt) = pc move
    pawnFirstMove = src move `div` 8 == if color == White then 1 else 6
    pawnDidDoubleMove = pawnFirstMove && dst move `div` 8 == if color == White then 3 else 4

ensureCapturedPieceRemoved board maybeCapture =
  case maybeCapture of
    Nothing -> board
    Just (piece, ind) -> if (board ! ind) == Just piece then Vector.update board (Vector.fromList [(ind, Nothing)]) else board

_move state src dst =
  let board' = board state
      player = whoseTurn state
      enpassantPossibleOn = if null (moves state) || not (enpassant $ head $ moves state) then Nothing else Just $ head $ moves state
   in maybe (Left "illegal") Right (board' !? src)
        >>= maybe (Left "no piece") Right
        >>= isOwnPiece player
        >>= legalMovementPattern src dst
        >>= nothingIsInTheWay board' src dst
        >>= destinationNotOccupiedByOwnPiece board' dst
        >>= canCapture board' enpassantPossibleOn src dst
        >>= \(piece, maybeCapture) ->
          moveDoesNotLeaveKingExposed board' player src dst piece
            >>= \newBoard -> Right (ensureCapturedPieceRemoved newBoard maybeCapture, Move {pc = piece, src = src, dst = dst}, maybeCapture)

toIndex sq =
  if length sq < 2
    then Left $ "illegal square " ++ sq
    else do
      col <- letterToNumber $ head sq
      row <- readEither (tail sq) >>= \r -> if r > 0 && r < 9 then Right r else Left $ "Invalid row " ++ show r
      pure ((row - 1) * 8 + col)

move state src dst = do
  src' <- toIndex src
  dst' <- toIndex dst
  _move state src' dst'

data Move = Move
  { pc :: Piece,
    src :: Int,
    dst :: Int
  }
  deriving (Eq, Show)

data Status = Winner Color | Stalemate | InProgress deriving (Eq, Show)

data GameState = GS {whoseTurn :: Color, board :: Board, result :: Status, currentTurn :: Integer, moves :: [Move], captured :: [Piece]} deriving (Eq, Show)

newGame = GS {whoseTurn = White, board = startingBoard, result = InProgress, currentTurn = 0, moves = [], captured = []}

gameLoop :: StateT GameState IO Status
gameLoop = do
  state <- S.get
  _ <- liftIO $ UI.printBoard (board state)
  case checkmate state of
    Just player -> return $ Winner player
    Nothing ->
      if stalemate state
        then return Stalemate
        else do
          src <- liftIO $ UI.askSourceSquare (whoseTurn state)
          dst <- liftIO $ UI.askDestinationSquare (whoseTurn state)
          case move state src dst of
            Left err -> do
              _ <- liftIO $ UI.printError err
              gameLoop
            Right (newBoard, move, maybeCapture) -> do
              put $ state {board = newBoard, whoseTurn = otherColor (whoseTurn state), moves = move : moves state, captured = maybe (captured state) ((: captured state) . fst) maybeCapture}
              gameLoop

play = evalStateT gameLoop newGame >>= print