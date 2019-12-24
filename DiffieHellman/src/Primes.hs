module Primes (randomPrime, isPrime) where

import Control.Monad (replicateM, unless)
import System.Random (randomRIO)
import Data.Bits (setBit)
import System.IO.Unsafe (unsafePerformIO)
import Math.NumberTheory.Powers.Modular (powMod)

randomPrime :: Int -> IO Integer
randomPrime numBits = do
  n <- randomOddBits numBits
  if isPrime n
    then return n
    else randomPrime numBits

randomOddBits :: Int -> IO Integer
randomOddBits numBits = do
  number <- randomBits numBits
  return $ number `setBit` 0  `setBit` (numBits-1)

randomBits :: Int -> IO Integer
randomBits numBits = randomRIO (2^(numBits-1), 2^(numBits) - 1)

numPrimalityTests = 10

isPrime :: Integer -> Bool
isPrime n = smallPrimesTest && rabinMiller
  where
    smallPrimesTest = all (\p -> n `mod` p /= 0)primesTo100
    rabinMiller = unsafePerformIO $
                  do
                    as <- replicateM numPrimalityTests $ randomRIO (1,n-1)
                    let arePrimes = map (primalityTest n) as
                    return $ all (==True) arePrimes

-- Primality test
primalityTest :: Integer -> Integer -> Bool
primalityTest n a = fermatTest n a && millerRabinTest n a

fermatTest :: Integer -> Integer -> Bool
fermatTest n a = powMod a (n-1) n == 1

millerRabinTest :: Integer -> Integer -> Bool
millerRabinTest n a
  | any (==1) checks = True
  | otherwise = False
  where
    checks = map (\i -> abs $ powMod a (2^i * q) n) [0..s]
    s = findS (n-1)
    q = n `div` 2^s

findS :: Integer -> Integer
findS n = step n 0
  where
    step :: Integer -> Integer -> Integer
    step n s
      | n `mod` 2^s == 0 = step n (s+1)
      | otherwise = s-1

primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
