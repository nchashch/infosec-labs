module Caesar (caesar) where

import qualified Data.Text as T (Text, map, filter, toLower)
import Data.Char (chr, ord, isSpace)

normalize :: T.Text -> T.Text
normalize = T.toLower . (T.filter isLatinLetter)

isLatinLetter :: Char -> Bool
isLatinLetter c = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')) || isSpace c

-- key is supposed to be a number between 0 and 25, if it is not key `mod` 26
-- will be used
caesar :: Int -> T.Text -> T.Text
caesar key = (T.map encrypt) . normalize
  where
    -- NOTE: This function assumes that the latin alphabet is continuous and is
    -- in order in the encoding used, which holds for UTF-8
    encrypt :: Char -> Char
    encrypt char = if isSpace char then char else chr $ (ord char - ord 'a' + shift) `mod` 26 + ord 'a'
      where
        shift = key `mod` 26
