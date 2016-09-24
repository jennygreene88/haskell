import System.Environment
import System.IO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

import Funs

main = do
    let pcap = "mdf-kospi200.20110216-0.pcap"

    -- arguments
    args <- getArgs
    putStrLn "args:"
    mapM_ putStrLn args
    if elem "-r" args
    then putStrLn "REORDER"
    else putStrLn ""

    -- read pcap file
    bs <- BSL.readFile pcap
    let packet = get_next_packet bs
    putStrLn packet

    return ()

-- read a ByteStream until a packet is found, then return that packet
get_next_packet :: BSL.ByteString -> String
get_next_packet bs = do
    let c = BSLC.head bs    -- grab 1st byte, convert to char
    [c]

-- test loop
loopn :: IO () -> Int -> IO ()
loopn action n =
    if n == 0
    then return ()
    else do
         action
         loopn action (n - 1)

