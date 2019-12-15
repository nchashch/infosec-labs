module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Lib (caesar)

main :: IO ()
main = do
  fileName <- T.getLine
  text <- T.readFile (T.unpack fileName)
  let cipherText = caesar 10 text
  T.putStrLn cipherText
  T.putStrLn $ caesar (-10) cipherText
