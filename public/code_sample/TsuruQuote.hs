
module TsuruQuote
( Quote(..)
) where

import qualified Data.ByteString.Lazy as BSL
import Data.Int
import Data.List
import Data.Word

-- Value constructor names have to start with a capital letter
-- Do I need to add strict?
data Quote = Quote { 
    packetTimeS     :: Word32
    ,packetTimeSS   :: Word32
    --,packetTimeMS   :: Int64
    ,packetMSToday  :: Int64
    ,capLen         :: BSL.ByteString -- Word32
    ,untruncLen     :: BSL.ByteString -- Word32
    ,other          :: BSL.ByteString -- 
    ,dataType       :: BSL.ByteString
    ,infoType       :: BSL.ByteString   --Word16
    ,marketType     :: BSL.ByteString   --Word8
    ,issueCode      :: BSL.ByteString
    ,issueSeq       :: BSL.ByteString   -- not sure what "Issue seq.-no." is.
    ,markStatType   :: BSL.ByteString
    ,totBidQVol     :: BSL.ByteString
    ,bestBidP1      :: BSL.ByteString
    ,bestBidQ1      :: BSL.ByteString
    ,bestBidP2      :: BSL.ByteString
    ,bestBidQ2      :: BSL.ByteString
    ,bestBidP3      :: BSL.ByteString
    ,bestBidQ3      :: BSL.ByteString
    ,bestBidP4      :: BSL.ByteString
    ,bestBidQ4      :: BSL.ByteString
    ,bestBidP5      :: BSL.ByteString
    ,bestBidQ5      :: BSL.ByteString
    ,totAskQVol     :: BSL.ByteString
    ,bestAskP1      :: BSL.ByteString
    ,bestAskQ1      :: BSL.ByteString
    ,bestAskP2      :: BSL.ByteString
    ,bestAskQ2      :: BSL.ByteString
    ,bestAskP3      :: BSL.ByteString
    ,bestAskQ3      :: BSL.ByteString
    ,bestAskP4      :: BSL.ByteString
    ,bestAskQ4          :: BSL.ByteString
    ,bestAskP5          :: BSL.ByteString
    ,bestAskQ5          :: BSL.ByteString
    ,noBestBidValidQTot :: BSL.ByteString
    ,noBestBidQ1        :: BSL.ByteString
    ,noBestBidQ2        :: BSL.ByteString
    ,noBestBidQ3        :: BSL.ByteString
    ,noBestBidQ4        :: BSL.ByteString
    ,noBestBidQ5        :: BSL.ByteString
    ,noBestAskValidQTot :: BSL.ByteString
    ,noBestAskQ1        :: BSL.ByteString
    ,noBestAskQ2        :: BSL.ByteString
    ,noBestAskQ3        :: BSL.ByteString
    ,noBestAskQ4        :: BSL.ByteString
    ,noBestAskQ5        :: BSL.ByteString
    ,qAccTime           :: BSL.ByteString
    ,qAccTimeMS         :: Int64
    ,endOfMessage       :: BSL.ByteString
    } deriving (Show)






