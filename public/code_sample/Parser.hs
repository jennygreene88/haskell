
module Parser
( parseQuotes
) where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import qualified Data.Bool as Bool
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Int
import Data.Maybe
import Data.List
import Data.Text.Encoding
import Data.Word
import Debug.Trace
import Text.Read
-----------------
import TsuruQuote
--https://hackage.haskell.org/package/bytestring-conversion
-----------------

-- convert [Char] to [Word8]
sToW :: String -> [Word8]
sToW s = BSL.unpack $ BSLC.pack s

-- convert [Word8] to [Char]
wToS :: [Word8] -> String
wToS w = BSLC.unpack $ BSL.pack w

test :: [Word8] -> BSL.ByteString -> IO ()
test w bs = do
    let len = length w
    --putStrLn ("length of '" ++ (wToS w) ++ "' = " ++ (show len))
    let firstI = BSL.elemIndex (w !! 0) bs
    BSLC.putStr (BSL.take 1 bs)
    case firstI of
        Nothing -> putStrLn "first element not found"
        Just _  -> do
            -- Found the first byte, so see if subsequent bytes are also there
            let here = wordIsHere w len bs (fromJust firstI)
            case here of
                True  -> putStrLn ( "\nWord found at index: " ++ (show firstI) )
                False -> do
                    putStrLn ( "NOT found at index: " ++ (show (fromJust firstI)) )
                    x <- (test w (BSL.drop ((fromJust firstI)+1) bs))
                    return ()

-- like BSL.elemIndex but takes [Word8] instead of Word
-- TODO: don't calculate len every time 
elemIndex' :: [Word8] -> BSL.ByteString -> Maybe Int64
elemIndex' w bs = do
    let len = length w
    let firstI = BSL.elemIndex (w !! 0) bs
    case firstI of Nothing -> Nothing
                   Just _  -> do
                        -- Found the first byte, so see if subsequent bytes are also there
                        let here = wordIsHere w len bs (fromJust firstI)
                        case here of True -> firstI
                                     False -> ((+) <$> firstI) <*> elemIndex' w (BSL.drop ((fromJust firstI)+1) bs)

-- See if a [Word8] matches a particular section of a ByteString
--
-- Input parameters:
--  w       string to search for
--  i       current index of string
--  len     length of string
--  bs      byte string to search in
--  k       current index of bytestring; this is the "here"
wordIsHere :: [Word8] -> Int -> BSL.ByteString -> Int64 -> Bool
wordIsHere w len bs k = wordIsHere' w 0 len bs k

wordIsHere' :: [Word8] -> Int64 -> Int -> BSL.ByteString -> Int64 -> Bool
wordIsHere' w i len bs k
    | i == ((fromIntegral len) :: Int64) = True                   -- Made to the end of the key without failing
    | w `genericIndex` i /= BSL.index bs k = False  -- this element does not match
    | otherwise = wordIsHere' w (i+1) len bs (k+1)

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

-- main parsing entry point
-- Returns: nothing
parseQuotes :: Bool -> BSL.ByteString -> IO ()
parseQuotes reorder bs = do
    --        let time = runGet getPacketTime $ BSL.take 4 (fromJust (getPacketHeader bs)) 
    --------------------------------------
    parseQuotes' reorder [] bs

-- recursive version
-- Walks through the ByteString, looks for quotes, and does stuff with them.
parseQuotes' :: Bool -> [Quote] -> BSL.ByteString -> IO ()
parseQuotes' reorder quotes bs = do
    case reorder of
        -- unordered
        False -> do
            let q = getNextQuote bs
            case q of
                Nothing -> putStrLn "\nNo quotes found.\n"
                Just _  -> do
                    printQuote (fromJust q)
        -- reorder
        True -> do
            let q = getNextQuote bs
            case q of
                Nothing -> do
                    -- flush remaining packets
                    putStrLn "\n<EOF>\n"
                Just _  -> do
                    -- print quotes
                    trace "printing quotes..." (return ())
                    printQuotesReorder quotes (fromJust q)
                    let i = getNextPacketIndex bs
                    case i of 
                        Nothing -> putStrLn "<EOF>"
                        Just _  -> parseQuotes' reorder quotes (BSL.drop (fromJust i) bs) -- TODO add the length of the packet

-- non reorder
printQuote :: Quote -> IO ()
printQuote q = do
    putStrLn $ show (sec q)
    putStrLn $ show (subSec q)
    putStrLn $ show (capLen q)
    putStrLn $ show (untruncLen q)
    BSLC.putStrLn (dataType q)
    BSLC.putStrLn (infoType q)
    BSLC.putStrLn (marketType q)
    BSLC.putStrLn (issueCode q)
    return ()

printQuotesReorder :: [Quote] -> Quote -> IO ()
printQuotesReorder quotes q = do
    -- grab "current" timestamp from newPacket
    -- add newPacket to quotes
    -- print and flush packets 3 seconds older than "current" timestamp, starting with oldest
    return ()

-- Returns: ByteString containing (the next) entire packet header
getPacketHeader :: BSL.ByteString -> Maybe BSL.ByteString
getPacketHeader bs = do
    let dataStart = elemIndex' (sToW "B6034") bs
    case dataStart of
        Nothing -> Nothing
        -- take the bytes up to this index, then drop all but the bytes we need
        Just _  -> do
            --trace ("dataStart = " ++ (show dataStart)) Nothing
            let all = BSL.take ((fromJust dataStart)) bs
            --trace ("len all = " ++ (show (BSL.length all))) Nothing
            --trace ("header: " ++ (show (BSLC.unpack all))) Nothing
            Just (BSL.drop ((fromJust dataStart) - 16) all)

-- Given a 16-bit packet header ByteString, extract the timestamp value.
--getPacketTime :: BSL.ByteString -> Maybe Int64
getPacketTime :: Get Int32
getPacketTime = do
    --trace ("header passed to getPacketTime: " ++ (BSLC.unpack bs) ++ "\n") Nothing
    --let t = BSL.take 8 bs
    --let ts = BSLC.unpack t
    --trace ts (Just 0) 
    --let tsq = "\"" ++ ts ++ "\""
    --trace ("time bytes: " ++ tsq) (Just 0) 
    --let tsr = (readMaybe tsq :: Maybe Int64)
    --case tsr of
    --    Nothing -> Nothing
    --    Just _  -> tsr
    -------------------------------------------
    --let td = ((decode t) :: Int32)
    --Just ((fromIntegral td) :: Int64)
    -------------------------------------------
    a <- getWord32le
    return ((fromIntegral a) :: Int32)

-- Returns: ByteString containing (the next) packet data
getPacketData :: BSL.ByteString -> BSL.ByteString
getPacketData bs = bs

-- returns the index of the first byte in a ByteStream of a quote packet's header.
getNextPacketIndex :: BSL.ByteString -> Maybe Int64
getNextPacketIndex bs = do
    let i = elemIndex' (sToW "B6034") bs
    case i of
        Nothing -> Nothing
        -- include the header
        Just _  -> Just ((fromJust i) - 16)  --Just (i - 16) -- TODO error handling for invalid index

getNextQuote :: BSL.ByteString -> Maybe Quote
getNextQuote bs = do
    -- isolate the ByteStream we need and feed it into a Quote.
    let i = getNextPacketIndex bs
    --trace (show i) (Nothing) -- OK 335

    --let bsQuote = BSL.take (16 + 215) bs
    case i of
        Nothing -> Nothing
        Just _  -> do
            let bsQuoteStart = BSL.drop (fromJust i) bs
            let bsQuote = BSL.take (16 + 215) bsQuoteStart
            --trace (show bsQuote) (Nothing)
            Just ( runGet makeQuote bsQuote )

-- fill up Quote
-- i is start of packet header 
makeQuote :: Get Quote
makeQuote = do
    s   <- fmap fromIntegral getWord32le 
    ss  <- getWord32le
    cl  <- getWord32le
    ul  <- getWord32le
    dt  <- getLazyByteString 2
    it  <- getLazyByteString 2 -- getWord16le
    mt  <- getLazyByteString 1 -- getWord8
    ic  <- getLazyByteString 12

    return Quote{
         sec            = s
        ,subSec         = ss
        ,capLen         = cl
        ,untruncLen     = ul
        ,dataType       = dt
        ,infoType       = it
        ,marketType     = mt
        ,issueCode      = ic
        }



