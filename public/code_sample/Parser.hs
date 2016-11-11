
module Parser
( parseQuotes
--,countPackets
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
--import Data.DateTime
import Data.Time.Clock
import Data.Time.LocalTime
--import Data.Time.Format
import Data.Word
import Debug.Trace
import Numeric
import Text.Printf
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
                        case here of
                            -- Found the key here, so return the index
                            True -> firstI        
                            -- Skip to the "false positive" byte and continue searching after it.
                            -- The "plus one"s were added to stop endless loops and keep the numbers accurate.
                            --False -> ((+) <$> (firstI)) <*> elemIndex' w (BSL.drop ((fromJust firstI)+1) bs)
                            False -> ((+) <$> (Just ((fromJust firstI)+1))) <*> elemIndex' w (BSL.drop ((fromJust firstI)+1) bs)

-- See if a [Word8] matches a particular section of a ByteString
{-|
-- Input parameters:
--  w       string to search for
--  i       current index of string
--  len     length of string
--  bs      byte string to search in
--  k       current index of bytestring; this is the "here"
-}
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
    parseQuotes' reorder bs

-- recursive version
-- Walks through the ByteString, looks for quotes, and does stuff with them.
parseQuotes' :: Bool -> BSL.ByteString -> IO ()
parseQuotes' reorder bs = do
    case reorder of
        -- unordered
        False -> do
            printQuotes bs
        -- reorder
        True -> do
            putStrLn "reorder :)"
            return ()
            {-|
            let q = getNextQuote bs
            case q of
                Nothing -> do
                    -- flush remaining packets
                    putStrLn "\n<EOF>\n"
                Just _  -> do
                    -- print quotes
                    trace "printing quotes..." (return ())
                    printQuotesReorder quotes (fromJust q)
                    parseQuotes' reorder quotes (BSL.drop (fromJust i) bs) -- TODO add the length of the packet
            -}

-- non reorder
printQuotes :: BSL.ByteString -> IO ()
printQuotes bs = do

    let iNextPacket = getNextPacketIndex bs
    case iNextPacket of
        Nothing -> putStrLn "\n<EOF>\n"
        Just _  -> do
            let bsNextPacket = BSL.drop (fromJust iNextPacket) bs
            let q = runGet makeQuote bsNextPacket

            -- Simple error checking
            --putStrLn $ show $ dataType q
            if (show $ dataType q) == show "B6" 
            then do
                --putStrLn (show q)
                putStr (packetTime q)
                putStr " "
                putStr (qAccTime q)
                putStr " "
                BSLC.putStr (issueCode q) 
                putStr " "
                BSLC.putStr (bestBidQ5 q)
                putStr "@"
                BSLC.putStr (bestBidP5 q)
                putStr " "
                BSLC.putStr (bestBidQ4 q)
                putStr "@"
                BSLC.putStr (bestBidP4 q)
                putStr " "
                BSLC.putStr (bestBidQ3 q)
                putStr "@"
                BSLC.putStr (bestBidP3 q)
                putStr " "
                BSLC.putStr (bestBidQ2 q)
                putStr "@"
                BSLC.putStr (bestBidP2 q)
                putStr " "
                BSLC.putStr (bestBidQ1 q)
                putStr "@"
                BSLC.putStr (bestBidP1 q)
                putStr " "
                BSLC.putStr (bestAskQ1 q)
                putStr "@"
                BSLC.putStr (bestAskP1 q)
                putStr " "
                BSLC.putStr (bestAskQ2 q)
                putStr "@"
                BSLC.putStr (bestAskP2 q)
                putStr " "
                BSLC.putStr (bestAskQ3 q)
                putStr "@"
                BSLC.putStr (bestAskP3 q)
                putStr " "
                BSLC.putStr (bestAskQ4 q)
                putStr "@"
                BSLC.putStr (bestAskP4 q)
                putStr " "
                BSLC.putStr (bestAskQ5 q)
                putStr "@"
                BSLC.putStr (bestAskP5 q)
                putStr "\n\n"
                
                -- next quote
                -- I can safely drop 215 bytes (data length); more if I wanted.
                printQuotes (BSL.drop (215) bsNextPacket)

            else do
                putStrLn (show q) 
                putStrLn "[invalid packet ????????????????????]\n"

printQuotesReorder :: [Quote] -> Quote -> IO ()
printQuotesReorder quotes q = do

    -- grab "current" timestamp from newPacket
    -- add newPacket to quotes
    -- print and flush packets 3 seconds older than "current" timestamp, starting with oldest
    return ()

-- count packets for debugging
countPackets :: BSL.ByteString -> Int64 -> IO Int64
countPackets bs counter = do
    putStrLn $ show counter
    let nextIndex = elemIndex' (sToW "B6034") bs 
    case nextIndex of 
        Nothing -> do
            return counter
        Just _  -> do
            --putStrLn $ ("       " ++ (show nextIndex))
            countPackets ((BSL.drop $ ((fromJust nextIndex) + 1)) bs) (counter+1)

-- returns the index of the first byte of a packet in a ByteStream 
getNextPacketIndex :: BSL.ByteString -> Maybe Int64
getNextPacketIndex bs = do
    let i = elemIndex' (sToW "B6034") bs
    case i of
        Nothing -> Nothing
        -- include the header
        Just _  -> (Just ((fromJust i) - 16 - 42))  --Just (i - 16) -- TODO error handling for invalid index

-- Make the Quote Accept Time into a neat string to be stored in a Quote.
prettyQAT :: BSL.ByteString -> [Char]
prettyQAT bs = do
    let qHH = (wToS $ [BSL.index bs 0]) ++ (wToS $ [BSL.index bs 1]) ++ ":"
    let qMM = qHH ++ (wToS $ [BSL.index bs 2]) ++ (wToS $ [BSL.index bs 3]) ++ ":"
    let qSS = qMM ++ (wToS $ [BSL.index bs 4]) ++ (wToS $ [BSL.index bs 5]) ++ ":"
    let q = qSS ++(wToS $ [BSL.index bs 6]) ++ (wToS $ [BSL.index bs 7])
    q

-- pretty packet timestamp
-- Take seconds & milliseconds and convert to a pretty timestamp string
prettyPacketTime :: Word32 -> Word32 -> [Char]
prettyPacketTime s ss = do
    (show s)++"."++(show ss)++"s" 

{-| 
    fill up Quote
    TODO: There is probably a prettier way to do this, but this works.
-}
makeQuote :: Get Quote
makeQuote = do
    s       <- getWord32le -- Using getInt32le reverses bytes 
    ss      <- getWord32le
    cl      <- getLazyByteString 4 -- getWord32le
    ul      <- getLazyByteString 4 --getWord32le
    o       <- getLazyByteString 42 -- other packet info
    dt      <- getLazyByteString 2
    it      <- getLazyByteString 2 -- getWord16le
    mt      <- getLazyByteString 1 -- getWord8
    ic      <- getLazyByteString 12
    is      <- getLazyByteString 3
    mst     <- getLazyByteString 2
    tbqv    <- getLazyByteString 7
    bbp1    <- getLazyByteString 5
    bbq1    <- getLazyByteString 7
    bbp2    <- getLazyByteString 5
    bbq2    <- getLazyByteString 7
    bbp3    <- getLazyByteString 5
    bbq3    <- getLazyByteString 7
    bbp4    <- getLazyByteString 5
    bbq4    <- getLazyByteString 7
    bbp5    <- getLazyByteString 5
    bbq5    <- getLazyByteString 7
    taqv    <- getLazyByteString 7
    bap1    <- getLazyByteString 5
    baq1    <- getLazyByteString 7
    bap2    <- getLazyByteString 5
    baq2    <- getLazyByteString 7
    bap3    <- getLazyByteString 5
    baq3    <- getLazyByteString 7
    bap4    <- getLazyByteString 5
    baq4    <- getLazyByteString 7
    bap5    <- getLazyByteString 5
    baq5    <- getLazyByteString 7
    nbbvqt  <- getLazyByteString 5
    nbbq1   <- getLazyByteString 4
    nbbq2   <- getLazyByteString 4
    nbbq3   <- getLazyByteString 4
    nbbq4   <- getLazyByteString 4
    nbbq5   <- getLazyByteString 4
    nbavqt  <- getLazyByteString 5
    nbaq1   <- getLazyByteString 4
    nbaq2   <- getLazyByteString 4
    nbaq3   <- getLazyByteString 4
    nbaq4   <- getLazyByteString 4
    nbaq5   <- getLazyByteString 4
    -- quote accept time
    qat     <- getLazyByteString 8
    eom     <- getLazyByteString 1

    return Quote{
        --sec             = s
        --,subSec         = ss
        packetTime      = prettyPacketTime s ss
        ,capLen         = cl
        ,untruncLen     = ul
        ,other          = o
        ,dataType       = dt
        ,infoType       = it
        ,marketType     = mt
        ,issueCode      = ic
        ,issueSeq       = is
        ,markStatType   = mst
        ,totBidQVol     = tbqv
        ,bestBidP1      = bbp1
        ,bestBidQ1      = bbq1
        ,bestBidP2      = bbp2
        ,bestBidQ2      = bbq2
        ,bestBidP3      = bbp3
        ,bestBidQ3      = bbq3
        ,bestBidP4      = bbp4
        ,bestBidQ4      = bbq4
        ,bestBidP5          = bbp5
        ,bestBidQ5          = bbq5
        ,totAskQVol         = taqv
        ,bestAskP1          = bap1
        ,bestAskQ1          = baq1
        ,bestAskP2          = bap2
        ,bestAskQ2          = baq2
        ,bestAskP3          = bap3
        ,bestAskQ3          = baq3
        ,bestAskP4          = bap4
        ,bestAskQ4          = baq4
        ,bestAskP5          = bap5
        ,bestAskQ5          = baq5
        ,noBestBidValidQTot = nbbvqt
        ,noBestBidQ1        = nbbq1
        ,noBestBidQ2        = nbbq2
        ,noBestBidQ3        = nbbq3
        ,noBestBidQ4        = nbbq4
        ,noBestBidQ5        = nbbq5
        ,noBestAskValidQTot = nbavqt
        ,noBestAskQ1        = nbaq1
        ,noBestAskQ2        = nbaq2
        ,noBestAskQ3        = nbaq3
        ,noBestAskQ4        = nbaq4
        ,noBestAskQ5        = nbaq5
        ,qAccTime           = (prettyQAT qat)
        ,endOfMessage       = eom
       
        }







