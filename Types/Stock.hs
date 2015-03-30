--
-- data <Data Type Name> =
--     <Value Constructor> [<Field1> ... ]
--     [ | <Value Constructor> [<Field1> ... ] [ | ... ] ... ] 
--     [ deriving (<Typeclass>) ]
--
-- <Data Type Name> must start with a capital letter
-- <Value Constructor> must start with a capital letter
-- <Value Constructor> can match <Type Name> if there is only one <Value Constructur>.
--

-- If I wanted, I could choose to NOT export the value constructurs.
-- This would prevent use of those functions, but those types could 
-- be returned by an exposed function. Also, whoever uses the module can't
-- pattern match against the value constructors.
--
--module Stock
--( Stock(..)
--) where

data Stock = Price String Float | Symbol String | Max Float | Min Float deriving (Show)

-- get specific info
get_symbol :: Stock -> String
get_symbol (Symbol s) = s


