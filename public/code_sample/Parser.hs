
module Parser
( parseQuotes
, printQuotes
, getNextPacketIndex
, getNextPacket
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
    --let header = getPacketHeader bs
    --case header of
    --    Nothing -> putStrLn "header not found"
    --    Just _  -> do
    --        let time = runGet getPacketTime $ BSL.take 4 (fromJust (getPacketHeader bs)) 
    --        putStrLn $ show time
    --------------------------------------
    parseQuotes' reorder [] bs

parseQuotes' :: Bool -> [a] -> BSL.ByteString -> IO ()
parseQuotes' reorder quotes bs = do
    let i = getNextPacketIndex bs
    case i of
        Nothing -> putStrLn "\nNo packets found.\n"
        Just _  -> do
            let newPacket = getNextPacket (fromJust i) bs -- returns Maybe Quote
            case reorder of
                -- unordered
                False -> do
                    return ()
                -- reorder
                True -> do
                    case newPacket of
                        Nothing -> do
                            -- flush remaining packets
                            putStrLn "\n<EOF>\n"
                        Just _  -> do
                            -- print quotes
                            trace "printing quotes..." (return ())
                            printQuotes quotes newPacket
                            parseQuotes' reorder quotes (BSL.drop (fromJust i) bs)

printQuotes :: [a] -> a -> IO ()
printQuotes quotes newPacket = do
    return ()

printQuotesReorder :: [a] -> a -> IO ()
printQuotesReorder quotes newPacket = do
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

-- *
getNextPacketIndex :: BSL.ByteString -> Maybe Int64
getNextPacketIndex bs = do
    let i = elemIndex' "B6034" bs
    case i of
        False -> Nothing
        True  -> Just (i - 16) -- TODO error handling for invalid index

-- *
getNextPacket :: Int64 -> BSL.ByteString -> Maybe TsuruQuote.Quote
getNextPacket i bs = do
    let q = TsuruQuote.Quote {test=sToW("1111")}
    q
