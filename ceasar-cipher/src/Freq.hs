module Freq where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMS
import Data.Char (chr, ord, isSpace)
import Data.Sort
import Data.List (group, sort)

-- Ciphertext -> Key
freqCount :: T.Text -> FreqDict
freqCount text = T.foldr freqInc (HMS.empty :: FreqDict) text

freqInc :: Char -> FreqDict -> FreqDict
freqInc ch freqDict = if isSpace ch then freqDict else HMS.insertWith (\new old -> new + old) ch 1 freqDict

type FreqDict = HMS.HashMap Char Int

freqOrder = "etaoinshrdlcumwfgypbvkjxqz"

freqRank :: T.Text -> [Char]
freqRank = reverse.(map fst).(sortOn snd).(HMS.toList).freqCount

breakCaesar :: T.Text -> [Int]
breakCaesar cipherText = zipWith (\a b -> ord a - ord b) freqOrder (freqRank cipherText)

breakCaesarMode :: T.Text -> Int
breakCaesarMode = head.mode.(map abs).breakCaesar

mode :: (Ord a) => [a] -> [a]
mode xs = map fst $ filter ((==best).snd) counts
    where counts = map (\l -> (head l, length l)) . group . sort $ xs
          best = maximum (map snd counts)
