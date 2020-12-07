module Helpers where

import Data.Char (toUpper)
import Text.Read (readEither)

within :: Ord a => a -> a -> a -> Bool
within min max n = n >= min && n <= max

letterToNumber :: Char -> Either String Int
letterToNumber c = let column = flip (-) 65 . fromEnum $ toUpper c in if within 0 7 column then Right column else Left $ "Unknown column " ++ [c]

toIndex :: String -> Either String Int
toIndex sq =
  if length sq < 2
    then Left $ "illegal square " ++ sq
    else do
      col <- letterToNumber $ head sq
      row <- readEither (tail sq) >>= \r -> if r > 0 && r < 9 then Right r else Left $ "Invalid row " ++ show r
      pure ((row - 1) * 8 + col)