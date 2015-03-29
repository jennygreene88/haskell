--
-- data <Type Name> =
-- <Value Constructor> [<Field1> ... ]
-- [ | <Value Constructor> [<Field1> ... ] ] [ | ... ]
-- [ deriving (<Typeclass>) ]
--
-- <Type Name> must start with a capital letter
-- <Value Constructor> can match <Type Name> if there is only one <Value Constructur>.
--

--module Stock
--(
--) where


-- Type Stock
-- Price <datetime> <price for that timestamp>
-- <stock symbol>
-- Max <max price ever>
-- Min <min price ever>
data Stock = Price String Float | Symbol String | Max Float | Min Float deriving (Show)
