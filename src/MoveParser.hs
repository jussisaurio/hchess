{-# LANGUAGE LambdaCase #-}
module MoveParser where

import Parser
import Pieces
import Control.Applicative (Alternative((<|>)))
import Data.Maybe (isJust)
import Data.Bool (bool)

data Move = Move { pt :: PieceType, fromFile :: Maybe Char, fromRank :: Maybe Int, capture :: Bool, toFile :: Char, toRank :: Int } deriving (Eq, Show)

data PawnMove = PawnMove { mv :: Move, promote :: Maybe PieceType } deriving (Eq, Show)

data KingMove = NormalKingMove Move | Castle { kingSide :: Bool } deriving (Eq, Show)

parseCastle :: Parser Char KingMove
parseCastle = Castle <$> ((False <$ prefix "O-O-O") <|> (True <$ prefix "O-O")) <* terminate

parseFile :: Parser Char Char
parseFile = anyOf "abcdefgh"
parseRank :: Parser Char Int
parseRank = read . (:[]) <$> anyOf "12345678"

parseCapture :: Parser Char Bool
parseCapture = isJust <$> try (char 'x')

parseKingMove :: Parser Char KingMove
parseKingMove = NormalKingMove <$> (Move King <$ char 'K' <*> skip <*> skip <*> parseCapture <*> parseFile <*> parseRank <* terminate)

charToPiece :: Maybe Char -> Maybe PieceType
charToPiece char = 
    char >>= \case
            'R' -> Just Rook
            'N' -> Just Knight
            'B' -> Just Bishop
            'Q' -> Just Queen
            'K' -> Just King
            _ ->  Nothing

maybeParser :: (t -> Bool) -> Maybe t -> Parser a t
maybeParser mapper mebe = Parser func
    where
        func input = (\x -> if mapper x then Just (x, input) else Nothing) =<< mebe

parseMove :: Parser Char Move
parseMove = Move <$> ((charToPiece <$> try (anyOf "RNBQK")) >>= maybeParser (/= King)) <*> try parseFile <*> try parseRank <*> parseCapture <*> parseFile <*> parseRank <* terminate
        <|> Move <$> ((charToPiece <$> try (anyOf "RNBQK")) >>= maybeParser (/= King)) <*> skip <*> skip <*> parseCapture <*> parseFile <*> parseRank <* terminate