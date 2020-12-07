{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Chess where

import Board (Board, Move (..), destinationNotOccupiedByOwnPiece, enpassant, isCheckOn, maybeCheck, nothingIsInTheWay, startingBoard)
import Castling
import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Except (MonadIO (liftIO))
import qualified Control.Monad.State as S
import Control.Monad.Trans.State (StateT, evalStateT, put)
import Data.Either (isRight)
import Data.Maybe
import Data.Vector ((!), (!?))
import qualified Data.Vector as Vector
import Helpers
import LegalMoves
import Pieces
  ( Color (..),
    Piece (..),
    PieceType (King, Pawn, Rook),
    isOwnPiece,
    otherColor,
  )
import qualified UI

legalMovementPattern :: Board -> [Move] -> Int -> Int -> Piece -> Either String Piece
legalMovementPattern board moves src dst piece
  | isJust castling = tryCastle board piece moves (fromJust castling)
  | src == dst = Left "Cannot choose not to move"
  | dst < 0 || dst > 63 = Left "off the board"
  | any (\p -> p src dst) $ legalMovementPatterns piece = Right piece
  | otherwise = Left $ "illegal movement pattern for " ++ show piece
  where
    castling = castlingAttempt piece src dst

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
          if pt1 == King && isJust (castlingAttempt (Piece color pt1) src dst)
            then Left "Cannot capture while castling"
            else
              if pt2 == King
                then Left "Programmer error, should not be able to capture king in-game"
                else
                  if pt1 /= Pawn || pawnCapture (Piece color pt1) src dst
                    then Right (Piece color pt1, Just (Piece color2 pt2, dst))
                    else Left "illegal capture"
   in maybe (Left "off the board") _canCapture (board !? dst)

moveDoesNotLeaveKingExposed board player src dst (Piece color pt) =
  if isCheckOn newBoard player
    then Left $ if isCheckOn board player then "There is a check on your king, you must neutralize the threat" else "Illegal move: player's own king would be exposed"
    else Right newBoard
  where
    pc = Piece color pt
    castling = castlingAttempt pc src dst
    newBoard = Vector.update board (Vector.fromList newPositions)
      where
        newPositions = case castling of
          Nothing -> [(src, Nothing), (dst, Just pc)]
          Just side -> let (rsrc, rdst) = rookCastlingMove color side in [(src, Nothing), (dst, Just pc), (rsrc, Nothing), (rdst, Just $ Piece color Rook)]

ensureCapturedPieceRemoved board maybeCapture =
  case maybeCapture of
    Nothing -> board
    Just (piece, ind) -> if (board ! ind) == Just piece then Vector.update board (Vector.fromList [(ind, Nothing)]) else board

_move state src dst =
  let board' = board state
      player = whoseTurn state
      moves' = moves state
      enpassantPossibleOn = if null (moves state) || not (enpassant $ head $ moves state) then Nothing else Just $ head $ moves state
   in maybe (Left "illegal") Right (board' !? src)
        >>= maybe (Left "no piece") Right
        >>= isOwnPiece player
        >>= legalMovementPattern board' moves' src dst
        >>= nothingIsInTheWay board' src dst
        >>= destinationNotOccupiedByOwnPiece board' dst
        >>= canCapture board' enpassantPossibleOn src dst
        >>= \(piece, maybeCapture) ->
          moveDoesNotLeaveKingExposed board' player src dst piece
            >>= \newBoard -> Right (ensureCapturedPieceRemoved newBoard maybeCapture, Move {pc = piece, src = src, dst = dst}, maybeCapture)

move state src dst = do
  src' <- toIndex src
  dst' <- toIndex dst
  _move state src' dst'

data Status = Winner Color | Stalemate | InProgress deriving (Eq, Show)

data GameState = GS {whoseTurn :: Color, board :: Board, result :: Status, currentTurn :: Integer, moves :: [Move], captured :: [Piece]} deriving (Eq, Show)

newGame = GS {whoseTurn = White, board = startingBoard, result = InProgress, currentTurn = 0, moves = [], captured = []}

interactive i a b = if i then liftIO a else pure b

type HumanReadableMove = (String, String)

gameLoop :: Maybe [HumanReadableMove] -> StateT GameState IO Status
gameLoop maybeMovelist = do
  let isInteractive = isNothing maybeMovelist
  let ifInteractive = interactive isInteractive
  state <- S.get
  _ <- ifInteractive (UI.printBoard (board state)) ()
  case checkmate state of
    Just player -> return $ Winner player
    Nothing ->
      if stalemate state
        then return Stalemate
        else
          if not isInteractive && Just [] == maybeMovelist
            then return InProgress
            else do
              src <- ifInteractive (UI.askSourceSquare (whoseTurn state)) (fst . head . fromJust $ maybeMovelist)
              dst <- ifInteractive (UI.askDestinationSquare (whoseTurn state)) (snd . head . fromJust $ maybeMovelist)
              case move state src dst of
                Left err -> do
                  _ <- liftIO $ UI.printError err
                  if not isInteractive then return InProgress else gameLoop Nothing
                Right (newBoard, move, maybeCapture) -> do
                  put $ state {board = newBoard, whoseTurn = otherColor (whoseTurn state), moves = move : moves state, captured = maybe (captured state) ((: captured state) . fst) maybeCapture}
                  gameLoop (tail <$> maybeMovelist)

play = evalStateT (gameLoop Nothing) newGame >>= print

playWithPremoves moves = evalStateT (gameLoop $ Just moves) newGame >>= print