module Utils (shortHash) where

import Data.ByteString.Base64 (encodeBase64)

shortHash :: ByteString -> [Char]
shortHash = take 8 . toString . encodeBase64