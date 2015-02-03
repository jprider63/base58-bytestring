module Data.ByteString.Base58 (encode, decode) where

import Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord, chr)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.String (fromString)
import Data.Word (Word8)
import Numeric (showIntAtBase, readInt)

import Data.ByteString.Base58.Util

b58Data :: BS.ByteString
b58Data = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

b58 :: Word8 -> Word8
b58 i = BS.index b58Data (fromIntegral i)

b58' :: Word8 -> Maybe Word8
b58' w = fromIntegral <$> BS.elemIndex w b58Data

encodeBase58I :: Integer -> BS.ByteString
encodeBase58I i = 
    fromString $ showIntAtBase (58 :: Integer) f (fromIntegral i) ""
  where
    f = chr . fromIntegral . b58 . fromIntegral

decodeBase58I :: BS.ByteString -> Maybe Integer
decodeBase58I s = case go of 
    Just (r,[]) -> Just r
    _           -> Nothing
  where
    c = b58' . fromIntegral . ord
    p = isJust . c 
    f = fromIntegral . fromJust . c
    go = listToMaybe $ readInt 58 p f (B8.unpack s)

-- | Encode a bytestring to a base 58 representation.
encode :: BS.ByteString -> BS.ByteString
encode bs = BS.append l r
  where 
    (z,b) = BS.span (== 0) bs
    l = BS.map b58 z -- preserve leading 0's
    r | BS.null b = BS.empty
      | otherwise = encodeBase58I $ bsToInteger b

-- | Decode a base 58 encoded bytestring. This can fail if the input bytestring
-- contains invalid base 58 characters such as 0,O,l,I
decode :: BS.ByteString -> Maybe BS.ByteString
decode bs = r >>= return . (BS.append prefix)
  where 
    (z,b)  = BS.span (== (b58 0)) bs
    prefix = BS.map (fromJust . b58') z -- preserve leading 1's
    r | BS.null b = Just BS.empty
      | otherwise = integerToBS <$> decodeBase58I b

