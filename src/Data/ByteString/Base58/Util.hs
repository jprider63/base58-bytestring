module Data.ByteString.Base58.Util where

import Data.Bits ((.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.List (unfoldr)
import Data.Word (Word8)

-- | Decode a big endian Integer from a bytestring
bsToInteger :: BS.ByteString -> Integer
bsToInteger = (foldr f 0) . reverse . BS.unpack
  where 
    f w n = (toInteger w) .|. shiftL n 8

-- | Encode an Integer to a bytestring as big endian
integerToBS :: Integer -> BS.ByteString
integerToBS 0 = BS.pack [0]
integerToBS i 
    | i > 0     = BS.pack $ reverse $ unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where 
    f 0 = Nothing
    f x = Just $ (fromInteger x :: Word8, x `shiftR` 8)

