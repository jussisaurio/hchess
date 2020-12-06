{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Chess where

import Control.Monad.Except (MonadIO (liftIO))
import qualified Control.Monad.State as S
import Control.Monad.Trans.State (StateT, evalStateT, put)
import Data.Char (toUpper)
import Data.Either (isRight)
import Data.List (find)
import Data.Maybe
import Data.Vector ((!), (!?))
import qualified Data.Vector as Vector
import Debug.Trace
import LegalMoves
  ( diagonalMove,
    diagonalOneDown,
    diagonalOneUp,
    horizontalMove,
    kingMove,
    knightMove,
    legalCapturePatterns,
    legalMovementPatterns,
    legalPawnNonCaptureMove,
    oneDown,
    oneUp,
    pawnCapture,
    steps,
    takeStep,
    twoDownBlackPawn,
    twoUpWhitePawn,
    verticalMove,
  )
import Pieces
  ( Color (..),
    Piece (..),
    PieceType (Bishop, King, Knight, Pawn, Queen, Rook),
  )
import Text.Read (readEither)

type Board = Vector.Vector (Maybe Piece)

bigPiecesRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

pawnsRow = replicate 8 Pawn

startingBoard :: Board
startingBoard = Vector.fromList $ map (Just . Piece White) bigPiecesRow ++ map (Just . Piece White) pawnsRow ++ replicate 32 Nothing ++ map (Just . Piece Black) pawnsRow ++ map (Just . Piece Black) bigPiecesRow

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

stalemate board = maybe False (const False) $ maybeCheck board -- TODO

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

moveWouldNeutralizeCheckIfThereIsOne board player src dst piece =
  if isCheckOn board player && isCheckOn newBoard player
    then Left "There is a check on your king, you must neutralize the threat"
    else Right ()
  where
    newBoard = Vector.update board (Vector.fromList [(src, Nothing), (dst, Just piece)])

moveWontCauseCheckOnOwnKing board player src dst piece =
  if isCheckOn newBoard player then Left "Illegal move: player's own king would be exposed" else Right newBoard
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
      moveWouldNeutralizeCheckIfThereIsOne board player src dst piece
        >> moveWontCauseCheckOnOwnKing board player src dst piece
          >>= \newBoard -> Right (newBoard, maybeCapture)

within min max n = n >= min && n <= max

letterToNumber c = let column = flip (-) 65 . fromEnum $ toUpper c in if within 0 7 column then Right column else Left $ "Unknown column " ++ [c]

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

data Move = Move Color Integer Integer deriving (Eq, Show)

data GameState = GS {whoseTurn :: Color, board :: Board, result :: Status, currentTurn :: Integer, moves :: [Move], captured :: [Piece]} deriving (Eq, Show)

newGame = GS {whoseTurn = White, board = startingBoard, result = InProgress, currentTurn = 0, moves = [], captured = []}

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

gameLoop :: StateT GameState IO Status
gameLoop = do
  state <- S.get
  _ <- liftIO $ mapM_ (print . getPrintableRow) (getRowsTopFirst (board state))
  case checkmate (board state) of
    Just player -> return $ Winner player
    Nothing ->
      if stalemate (board state)
        then return Stalemate
        else do
          src <- liftIO $ putStrLn ((show $ whoseTurn state) ++ ", which square to move: ") >> getLine
          dst <- liftIO $ putStrLn ((show $ whoseTurn state) ++ ", target square: ") >> getLine
          case move (board state) (whoseTurn state) src dst of
            Left err -> do
              _ <- liftIO $ print $ "Error: " ++ err
              gameLoop
            Right (newBoard, maybeCapture) -> do
              put $ state {board = newBoard, whoseTurn = if whoseTurn state == White then Black else White, captured = maybe (captured state) (: captured state) maybeCapture}
              gameLoop

play = evalStateT gameLoop newGame >>= print