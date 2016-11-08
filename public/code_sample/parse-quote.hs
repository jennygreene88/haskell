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

    bs <- BSL.readFile pcap_file

    let reorder = False
    if elem "-r" args
    then do
        let reorder = True
        return ()
    else do
        let reorder = False
        return ()

    parseQuotes reorder bs

    -- BSL.drop results in an endless loop; need to use recursion to trim a ByteString.
    --parseQuotes False bs

    --putStrLn $ (show $ elemIndex' (sToW "B6034") bs)

    return ()


