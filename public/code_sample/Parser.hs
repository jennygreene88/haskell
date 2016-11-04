
module Parser
( sToW
, elemIndex'
) where

import qualified Data.Bool as Bool
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Int
import Data.Maybe
import Data.List
import Data.Word


-- convert [Char] to [Word8]
sToW :: String -> [Word8]
sToW s = BSL.unpack $ BSLC.pack s

-- like BSL.elemIndex but takes [Word8] instead of Word
elemIndex' :: [Word8] -> BSL.ByteString -> Maybe Int64
elemIndex' w bs = do
    let len = length w
    let maybeiFirst = BSL.elemIndex (w !! 0) bs
    case maybeiFirst of Nothing -> Nothing
                        Just _ -> do
                            let here = wordIsHere w 0 len bs (fromJust maybeiFirst)
                            case here of True -> maybeiFirst
                                         False -> elemIndex' w (BSL.tail bs)

-- See if a [Word8] matches a particular section of a ByteString
-- Input parameters:
--  w       string to search for
--  i       current index of string
--  len     length of string
--  bs      byte string to search in
--  k       current index of bytestring
wordIsHere :: [Word8] -> Int64 -> Int -> BSL.ByteString -> Int64 -> Bool
wordIsHere w i len bs k
    | i == ((fromIntegral len) :: Int64) = True                   -- Made to the end of the key without failing
    | w `genericIndex` i /= BSL.index bs k = False  -- this element does not match
    | otherwise = wordIsHere w (i+1) len bs (k+1)


