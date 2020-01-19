module SRP where

import Math.NumberTheory.Powers.Modular (powMod)
import DiffieHellman (getGenerator)
import Primes
import Crypto.Saltine.Core.Hash
import Data.Serialize (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Random
import Data.Bits
import qualified Data.Text as T
import Data.Text.Encoding

securePrime :: Int -> IO Integer
securePrime numBits = randomPrime numBits >>= (\p -> return $ 2*p + 1)

hashInteger :: Integer -> Integer
hashInteger = fromBytes.hash.encode

getK :: Integer -> Integer -> Integer
getK n g = fromBytes . hash $ (hash.encode) n <> (hash.encode) g

data Params = Params {
  n :: Integer,
  g :: Integer,
  k :: Integer
  } deriving Show

getParams :: Int -> IO Params
getParams numBits =
  do
    n <- securePrime numBits
    g <- getGenerator n
    let k = getK n g
    return Params {
      n = n,
      g = g,
      k = k
      }

fromBytes :: B.ByteString -> Integer
fromBytes = B.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b

data SignUpData = SignUpData {
  username :: T.Text,
  password :: T.Text,
  salt :: B.ByteString,
  x :: Integer,
  v :: Integer
  } deriving Show

signUp :: T.Text -> T.Text -> Params -> IO SignUpData
signUp username password (Params n g k) =
  do
    salt <- random 64
    let passwordBytes = encodeUtf8 password
    let x = fromBytes $ hash salt <> passwordBytes
    let v = powMod g x n
    return SignUpData {
      username = username,
      password = password,
      salt = salt,
      x = x,
      v = v
      }

data ScramblerData = ScramblerData {
  r0 :: Integer,
  r1 :: Integer,
  a :: Integer,
  b :: Integer,
  u :: Integer
  }

computeScrambler :: Params -> SignUpData -> IO ScramblerData
computeScrambler (Params n g k) (SignUpData _ _ _ _ v) = do
    r0 <- randomBits 512
    r1 <- randomBits 512

    let a = powMod g r0 n
    let b = powMod g (k * v + powMod g r1 n) n
    let u = fromBytes $ (hash.encode) a <> (hash.encode) b

    return ScramblerData {
      r0 = r0,
      r1 = r1,
      a = a,
      b = b,
      u = u
      }

data SessionKey = SessionKey {
  secret :: Integer,
  key :: Integer
  } deriving Show

clientComputeSessionKey :: Params -> SignUpData -> ScramblerData -> SessionKey
clientComputeSessionKey (Params n g k) (SignUpData _ p salt x _) (ScramblerData r0 _ _ b u) =
  SessionKey {
    secret = s,
    key = hashInteger s
  }
  where
    s = powMod (b - k*(powMod g x n)) (r0 + u*x) n

serverComputeSessionKey :: Params -> SignUpData -> ScramblerData -> SessionKey
serverComputeSessionKey (Params n g k) (SignUpData _ p salt _ v) (ScramblerData _ r1 a _ u) =
  SessionKey {
    secret = s,
    key = hashInteger s
  }
  where
    s = powMod (a * powMod v u n) r1 n
