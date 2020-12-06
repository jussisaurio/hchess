module Helpers where

import Data.Char (toUpper)

within min max n = n >= min && n <= max

letterToNumber c = let column = flip (-) 65 . fromEnum $ toUpper c in if within 0 7 column then Right column else Left $ "Unknown column " ++ [c]