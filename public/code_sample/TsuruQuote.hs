
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
    sec             :: Word32
    ,subSec         :: Word32
    ,capLen         :: Word32
    ,untruncLen     :: Word32
    ,dataType       :: BSL.ByteString
    ,infoType       :: BSL.ByteString   --Word16
    ,marketType     :: BSL.ByteString   --Word8
    ,issueCode      :: BSL.ByteString
    } deriving (Show)


