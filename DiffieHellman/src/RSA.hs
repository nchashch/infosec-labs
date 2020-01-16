{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module RSA where

import Primes
import Data.Proxy
import Math.NumberTheory.Powers.Modular (powMod)

carmichaelTotient :: Integer -> Integer -> Integer
carmichaelTotient p q = lcm (p-1) (q-1)

gcd :: Integer -> Integer -> Integer
gcd = undefined

inverse :: Integer -> Integer -> Integer
inverse n modulus = undefined

encryptionExponent :: Integer
encryptionExponent = 2^16 + 1

decryptionExponent :: Integer -> Integer -> Maybe Integer
decryptionExponent p q = modInv encryptionExponent (carmichaelTotient p q)

encrypt :: Integer -> Integer -> Integer
encrypt modulus message = powMod message encryptionExponent modulus

decrypt :: Integer -> Integer -> Integer -> Integer
decrypt modulus d ciphertext = powMod ciphertext d modulus

-- Code from https://github.com/metaleap/rosetta-haskell-dump/blob/master/modular-inverse.hs
-- Extended Euclidean algorithm.  Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).  Note that x or y may be negative.
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)

-- Given a and m, return Just x such that ax = 1 mod m.  If there is no such x
-- return Nothing.
modInv a m = let (i, _, g) = gcdExt a m
             in if g == 1 then Just (mkPos i) else Nothing
  where mkPos x = if x < 0 then x + m else x
