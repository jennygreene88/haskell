
module Parser
( sToW
, wToS
, elemIndex'
--, wordIsHere
, walk
) where

import qualified Data.Bool as Bool
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Int
import Data.Maybe
import Data.List
import Data.Word
-----------------
--import Packet
--import Pcap
-----------------

-- convert [Char] to [Word8]
sToW :: String -> [Word8]
sToW s = BSL.unpack $ BSLC.pack s

-- convert [Word8] to [Char]
wToS :: [Word8] -> String
wToS w = BSLC.unpack $ BSL.pack w

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
--
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

-- Walk through a ByteStream and look for packets.
-- When a packet is found, print out information about it.
walk w bs = walk' w bs 1

walk' :: [Word8] -> BSL.ByteString -> Int64 -> IO ()
walk' w bs counter = do
    let i = elemIndex' w bs
    --putStrLn $ "i = " ++ (show (fromJust i))
    case i of Nothing -> putStrLn "End of ByteString."
              Just _ -> do
                       putStrLn $ "Packet # " ++ (show counter)
                       --putStrLn "packet info goes here"
                       -- Using tail recursion can read the ByteString forever without filling up the stack
                       walk' w (BSL.drop ((fromJust i)+1) bs) (counter + 1)


