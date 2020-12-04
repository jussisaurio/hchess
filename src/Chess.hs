{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Chess where

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadIO (liftIO))
import qualified Control.Monad.State as S
import Control.Monad.Trans.State (StateT, evalStateT, put)
import Data.Char (toUpper)
import Data.Either (isRight)
import Data.Maybe
import Data.Vector ((!), (!?))
import qualified Data.Vector as Vector
import LegalMoves
  ( diagonalMove,
    diagonalOneDown,
    diagonalOneUp,
    getUnitVector,
    horizontalMove,
    kingMove,
    knightMove,
    legalCapturePatterns,
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

legalMovementPattern
  src
  dst
  piece =
    if dst < 0 || dst > 63
      then Left "off the board"
      else
        let illegalMovementPattern = Left $ "illegal movement pattern for " ++ show piece
         in let test p = if p src dst then Right piece else illegalMovementPattern
             in case piece of
                  Piece Black Pawn -> test oneDown <|> test diagonalOneDown <|> test twoDownBlackPawn
                  Piece White Pawn -> test oneUp <|> test diagonalOneUp <|> test twoUpWhitePawn
                  Piece _ Rook -> test horizontalMove <|> test verticalMove
                  Piece _ Knight -> test knightMove
                  Piece _ Bishop -> test diagonalMove
                  Piece _ Queen -> test horizontalMove <|> test verticalMove <|> test diagonalMove
                  Piece _ King -> test kingMove -- todo castling

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

isCheck :: Board -> Maybe Color
isCheck board = if null kingLocations then Nothing else (\(Piece c _) -> c) . fromJust . (!) board <$> Vector.find ((<) 0 . length . findOpponentsThreateningSquare board) kingLocations
  where
    kingLocations = Vector.findIndices (\it -> isJust it && let (Piece c t) = fromJust it in t == King) startingBoard

-- todo enpassant
canCapture board src dst (Piece color pt1) =
  let _canCapture = \case
        Nothing -> Right (Piece color pt1, Nothing)
        Just (Piece color2 pt2) -> if pt1 /= Pawn || pawnCapture (Piece color pt1) src dst then Right (Piece color pt1, Just $ Piece color2 pt2) else Left "illegal capture"
   in maybe (Left "off the board") _canCapture (board !? dst)

moveWouldNeutralizeCheckIfThereIsOne board player src dst piece =
  maybe (Right ()) checkNeutralize (isCheck board)
  where
    checkNeutralize color =
      if color /= player
        then Right ()
        else maybe (Right ()) doesNewBoardHaveCheck (isCheck newBoard)
    doesNewBoardHaveCheck color =
      if color /= player then Right () else Left "There is a check on your king, you must neutralize the threat"
    newBoard = Vector.update board (Vector.fromList [(src, Nothing), (dst, Just piece)])

moveWontCauseCheckOnOwnKing board player src dst piece =
  maybe (Right newBoard) (\clr -> if clr == player then Left "Illegal move: player's own king would be exposed" else Right newBoard) $ isCheck newBoard
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