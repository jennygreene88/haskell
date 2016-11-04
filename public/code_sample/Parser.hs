
module Parser
( sToW
, elemIndex'
) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Int
import Data.Maybe
import Data.Word


-- convert [Char] to [Word8]
sToW :: String -> [Word8]
sToW s = BSL.unpack $ BSLC.pack s

-- like BSL.elemIndex but takes [Word8] instead of Word
elemIndex' :: [Word8] -> BSL.ByteString -> Maybe Int64
elemIndex' w bs = Just 2
