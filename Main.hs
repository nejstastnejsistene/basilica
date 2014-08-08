{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Control.Monad.IO.Class
import Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text as T
import Data.Text.Lazy.Encoding
import Data.Word8
import Text.RawString.QQ
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Text.StringTemplate
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

serveHtml :: String -> ActionM ()
serveHtml path = liftIO (readFile path) >>= serveHtml'
  where serveHtml' = html . decodeUtf8 . renderTemplate . encode . B8.pack

renderTemplate :: B.ByteString -> B.ByteString
renderTemplate x = render (setAttribute "content" x tmpl)
  where tmpl = newSTMP [r|<div id="content" style="display:none">$content$</div>
<script>
  x = new XMLHttpRequest();
  x.open('POST', '/decode', true);
  x.onreadystatechange = function() {
    if (x.readyState == 4) {
      document.write(x.responseText)
    }
  }
  x.send(document.getElementById('content').textContent);
</script>
|]

main :: IO ()
main = scotty 5050 $ do
  middleware logStdoutDev
  get "/" (serveHtml "index.html")
  --get "/:path" (param "path" >>= serveHtml)
  post "/encode" (body >>= text . decodeUtf8 . encode)
  post "/decode" (body >>= maybe err (text . decodeUtf8) . decode)
    where err = status status400 >> text "invalid basilica"
