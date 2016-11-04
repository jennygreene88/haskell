
module Pcap
( Pcap
, GlobalHeader
, MagicNumber
--, VersionMajor
--, VersionMinor
--, Zone
--, SigFigs
--, SnapLen
--, Network
, Header
--, TimeSec
--, TimeMil
--, SavedSize
--, DataSize
, PacketData
--, IssueCode
) where

import Data.Int
import Data.List
import Data.Word
-----------------
--import Packet

-- Value constructor names have to start with a capital letter
data Pcap = GlobalHeader | Header | PacketData
data GlobalHeader = MagicNumber  [Word8]    -- 4
                  | VersionMajor [Word8]    -- 2
                  | VersionMinor [Word8]    -- 2
                  | Zone [Word8]            -- 4
                  | SigFigs [Word8]         -- 4
                  | SnapLen [Word8]         -- 4
                  | Network [Word8]         -- 4
data Header = TimeSec [Word8]   -- (4 bytes)
            | TimeMil [Word8]   -- 4
            | SavedSize [Word8] -- 4
            | DataSize [Word8]  -- 4
data PacketData = IssueCode [Word8]    -- [savedSize]


