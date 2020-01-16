module DiffieHellman where

import Primes (randomPrime, randomBits)
import System.Random (randomRIO)
import Math.NumberTheory.Powers.Modular (powMod)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getGenerator :: Integer -> IO Integer
getGenerator p = do
  g <- randomRIO (2, p-2)
  if powMod g ((p-1)`div`2) p /= 1
    then return g
    else getGenerator p

diffieHellman :: Integer -> Integer -> Integer -> Integer
diffieHellman p g a = powMod g a p
