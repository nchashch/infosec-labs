module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Lib (caesar)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName, key] -> encryptFile fileName (read key :: Int) >>= T.putStrLn
    otherwise -> putStrLn "Invalid arguments\nUsage:\ncaesar fileName key"
encryptFile :: FilePath -> Int -> IO T.Text
encryptFile fileName key = do
  text <- T.readFile fileName
  return $ caesar key text
