
module TsuruQuote
( Quote
) where

import Data.Int
import Data.List
import Data.Word

-- Value constructor names have to start with a capital letter

data Quote = Quote { 
test :: Word32
} deriving (Show)


