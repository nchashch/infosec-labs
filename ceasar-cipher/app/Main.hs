module Main where

import System.Environment (getArgs)
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T (putStrLn, readFile)
import Caesar (caesar)
import Freq

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName, key] -> do
      cipherText <- encryptFile fileName (read key :: Int)
      T.putStrLn cipherText
      putStrLn ""
      print freqOrder
      print $ freqRank cipherText
      print $ breakCaesarMode cipherText
    otherwise -> putStrLn "Invalid arguments\nUsage:\ncaesar fileName key"

encryptFile :: FilePath -> Int -> IO T.Text
encryptFile fileName key = do
  text <- T.readFile fileName
  return $ caesar key text
