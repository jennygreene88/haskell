-- File:            parse-quote.hs
-- Description:     Main program for Tsuru Capital code sample.

import Control.Applicative
import Data.Bool
import Data.Word
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import System.Environment
import System.IO
-----------------
import Parser
-----------------
-- Function outline: 
--  Scan data stream for a packet
--      If found, print it and call scan function with remaining data stream (tail recursion to save stack.
--      If end of file reached, return.
--      If not end of file, call scan function (tail recursion to save stack).
main = do
    let pcap_file = "packets.pcap" -- 99106 packets?

    -- arguments
    args <- getArgs
    putStrLn "args:"
    mapM_ putStrLn args
    if elem "-r" args
    then putStrLn "REORDER"
    else putStrLn ""

    -- Open pcap file into bytestream
    bs <- BSL.readFile pcap_file
    -- trim global header

    -- BSL.drop results in an endless loop; need to use recursion to trim a ByteString.
    parseQuotes False bs
    --putStrLn  "B6034"
    --putStrLn $ (show $ elemIndex' (sToW "B6034") bs)

    --let i = elemIndex' (sToW "B6034" ) bs
    --case i of Nothing -> return ()
    --          Just _  -> putStrLn (show (fromJust i ))

    --test (sToW "B6034") bs
    return ()


