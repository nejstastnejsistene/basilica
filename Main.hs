{-# LANGUAGE OverloadedStrings #-}

import Data.Bits
import Data.Text.Lazy.Encoding
import Data.Word8
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Types
import Web.Scotty

encode :: B.ByteString -> B.ByteString
encode = B.concatMap encodeWord
  where
    basilica = B.unpack "basilica"
    encodeWord x = B.pack (zipWith encodeBit basilica bits)
      where bits = map (testBit x) [0..]
    encodeBit letter True  = toUpper letter
    encodeBit letter False = toLower letter

decode :: B.ByteString -> Maybe B.ByteString
decode "" = Just ""
decode x  = uncurry decode' (B.splitAt 8 x)
  where
    decode' x xs 
      | B.length x /= 8 = Nothing
      | otherwise = fmap (B.cons $ decodeWord x) (decode xs)
    decodeWord :: B.ByteString -> Word8
    decodeWord x = foldl setBit 0 bits
      where bits = [i | (i, c) <- zip [0..] (B.unpack x), isUpper c]

main :: IO ()
main = scotty 5050 $ do
  post "/encode" (body >>= text . decodeUtf8 . encode)
  post "/decode" (body >>= maybe err (text . decodeUtf8) . decode)
    where err = status status400 >> text "invalid basilica"
