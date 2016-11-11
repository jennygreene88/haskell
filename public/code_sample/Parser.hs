
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
import Data.Char
import Data.Int
import Data.Maybe
import Data.List
import Data.Text.Encoding
--import Data.DateTime
import Data.Time.Clock
import Data.Time.Clock.POSIX
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

-- main parsing entry point
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
            printQuotesReorder bs
            {-
            printQuotesReorder

                Just _  -> do
                    -- print quotes
                    trace "printing quotes..." (return ())
                    printQuotesReorder quotes (fromJust q)
                    parseQuotes' reorder quotes (BSL.drop (fromJust i) bs) -- TODO add the length of the packet
            -}

-- Print one Quote
printQuote :: Quote -> IO ()
printQuote q = do
    --putStrLn (show q)
    
    putStrLn ("qat ms: "++ (show(qatToMS (qAccTime q))))

    putStr $ prettyPacketTime (packetTimeS q) (packetTimeSS q)
    putStr " "
    putStr $ prettyQAT (qAccTime q)
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
                printQuote q
                
                -- next quote
                -- I can safely drop 215 bytes (data length); more if I wanted b/c of other headers.
                printQuotes (BSL.drop 215 bsNextPacket)

            else do
                putStrLn ("[invalid packet:]\n"++(show q)++"\n") 

-- reorder
-- walk through ByteString and maintain a buffer list of Quotes
printQuotesReorder :: BSL.ByteString -> IO ()
printQuotesReorder bs = printQuotesReorder' [] bs 0 0
-- recursive version
-- sweep the list THEN add a new quote
printQuotesReorder' :: [Quote] -> BSL.ByteString -> Int -> Int64 -> IO ()
printQuotesReorder' quotes bs index timeMS = do
    if (index < (length quotes))
    -- sweep (check the time of the quote that index points to)
    then do
        --putStrLn ("index = " ++ (show index))
        if (( qAccTimeMS (quotes !! index)) + 3000) < timeMS -- see if there's a quote w/QAT 3sec older than packetTime.
        then do -- Print the Quote then pop it from the list
            --putStrLn "printing..."
            printQuote (quotes !! index)
            --putStrLn "popping..."
            printQuotesReorder' ((take index quotes)++(drop (index+1) quotes)) bs (index) timeMS 
        else do -- Quote isn't old enough to print, so just continue walking the quote list
            --putStrLn "quote is too young"
            printQuotesReorder' quotes bs (index+1) timeMS 
    else do -- Add a new Quote and start a new sweep
        let iNextPacket = getNextPacketIndex bs
        --putStrLn ("iNextPacket = " ++ (show iNextPacket))
        case iNextPacket of
            Nothing -> do  -- no more packets, so just start a new sweep (eventually the list will run out)
                putStrLn "\n NO MORE PACKETS \n"
                -- printQuotesReorder' quotes bs 0 timeMS
            Just _  -> do -- Add the next Quote to the list and start a new sweep 
                putStrLn "pushing new quote"
                let bsNextPacket = BSL.drop (fromJust iNextPacket) bs
                let q = runGet makeQuote bsNextPacket
                printQuotesReorder' (q:quotes) (BSL.drop 215 bsNextPacket) 0 (packetMSToday q)-- can do more than 215
                
                
    {-|
    -- add new quote to list
    let iNextPacket = getNextPacketIndex bs
    case iNextPacket of
        Nothing -> do
            -- no more packets
            putStrLn "\n NO MORE QUOTES \n"
            flushQuotes quotes
        Just _  -> do
            let bsNextPacket = BSL.drop (fromJust iNextPacket) bs
            let q = runGet makeQuote bsNextPacket
            --putStrLn "APPENDING NEW QUOTE"
            let newQs = processQuotes (q:quotes) 0 (packetTimeMS q)
            case newQs of
                [] -> putStrLn "EMPTY QUOTE LIST"
                _  ->  printQuotesReorder' ( newQs ) (BSL.drop 215 bsNextPacket)
    -}

{-|
-- sweep the list and print any that are older than 3 seconds
processQuotes :: [Quote] -> Int -> Int64 -> [Quote]
processQuotes quotes index timeMS = do
    trace "\nPROCESSING A QUOTE!\n" []
    if index > (length quotes)
    then quotes
    else
        case quotes of
            [] -> []
            _  -> do
                if ( (packetTimeMS (quotes !! index)) + 3000) < timeMS
                then do
                    return (printQuote (quotes !! index))
                    processQuotes  ((take index quotes)++(drop (index+1) quotes)) (index+1) timeMS
                else quotes -- no quotes to print
-}

-- Print all the quotes in the list, from oldest to newest
flushQuotes :: [Quote] -> IO ()
flushQuotes quotes = do
    -- TODO
    putStrLn "\nFLUSHING\n"
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

-- Format the Quote Accept Time.
prettyQAT :: BSL.ByteString -> [Char]
prettyQAT bs = do
    let qHH = ([BSLC.index bs 0]) ++ ([BSLC.index bs 1]) ++ ":"
    let qMM = qHH ++ ([BSLC.index bs 2]) ++ ([BSLC.index bs 3]) ++ ":"
    let qSS = qMM ++ ([BSLC.index bs 4]) ++ ([BSLC.index bs 5]) ++ ":"
    let q = qSS ++([BSLC.index bs 6]) ++ ([BSLC.index bs 7])
    q
    --[(qat!!0),(qat!!1),':',(qat!!2),(qat!!3),':',(qat!!4),(qat!!5)]

-- pretty packet timestamp
-- Take seconds & milliseconds and convert to a pretty timestamp string
prettyPacketTime :: Word32 -> Word32 -> [Char]
prettyPacketTime s ss =
    ((show s)++"."++(show ss)++"s" )

-- convert to milliseconds since midnight
qatToMS :: BSL.ByteString -> Int64
qatToMS bs = do
    let qH1 = ( (fromIntegral ( digitToInt ( BSLC.index bs 0) ))::Int64) * 10 * 60 * 60 * 1000  
    let qH2 = ( (fromIntegral ( digitToInt ( BSLC.index bs 1) ))::Int64)      * 60 * 60 * 1000
    let qM1 = ( (fromIntegral ( digitToInt ( BSLC.index bs 2) ))::Int64) * 10      * 60 * 1000
    let qM2 = ( (fromIntegral ( digitToInt ( BSLC.index bs 3) ))::Int64)           * 60 * 1000
    let qS1 = ( (fromIntegral ( digitToInt ( BSLC.index bs 4) ))::Int64) * 10           * 1000
    let qS2 = ( (fromIntegral ( digitToInt ( BSLC.index bs 5) ))::Int64)                * 1000
    let qu1 = ( (fromIntegral ( digitToInt ( BSLC.index bs 6) ))::Int64) * 10 
    let qu2 = ( (fromIntegral ( digitToInt ( BSLC.index bs 7) ))::Int64)      
    (qH1 + qH2 + qM1 + qM2 + qS1 + qS2 + qu1 + qu2)


-- get milliseconds since midnight
packetTimeToMS :: Word32 -> Word32 -> Int64
packetTimeToMS s ss =
    1 -- TODO


{-
    fill up Quote
    TODO: There is probably a prettier way to do this, but this works.
-}
makeQuote :: Get Quote
makeQuote = do
    s       <- getWord32le -- Using getInt32le reverses bytes 
    ss      <- getWord32le -- subseconds packet timestamp
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
    qat     <- getLazyByteString 8 -- get as ByteString???
    eom     <- getLazyByteString 1

    return Quote{
        packetTimeS     = s
        ,packetTimeSS   = ss    -- sub-seconds 
        --,packetTimeMS   = ((fromIntegral s)::Int64) * 1000 + ((fromIntegral ss)::Int64) -- total time in milliseconds
        ,packetMSToday  = packetTimeToMS s ss
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
        ,qAccTime           = qat
        ,qAccTimeMS         = qatToMS qat -- convert to milliseconds since midnight
        ,endOfMessage       = eom
       
        }







