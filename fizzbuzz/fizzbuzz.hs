
---------------------------------
-- FizzBuzz
--
-- AUTHOR
--    Alan Kroner
--
-- CREATED
--    2015-03-01
--
-- PURPOSE
--    The FizzBuzz problem, implemented various ways.
--    This was done as a learning exercise.
--
-- THE FIZZBUZZ PROBLEM STATEMENT
--    Write a program that prints the numbers from 1 to 100.
--    But for multiples of three print “Fizz” instead of the number
--    and for the multiples of five print “Buzz”.
--    For numbers which are multiples of both three and five print “FizzBuzz”.
--
-- REMARKS
--    Instead of returning a list of Strings, perhaps one big tuple
--    would be useful for retaining data types.
---------------------------------

-- fizzbuzz functions to be called by user
fizzbuzz = fb1 [1..100]
fizzbuzz_r = fb2 1 100 []
fizzbuzz_map = fb3 [1..100]
fizzbuzz_high = fb4 [1..100]

-- method 1: list comprehension
fb1 :: (Integral a, Show a) => [a] -> [String]
fb1 x_list = [ fb_show x | x <- x_list ]

-- method 2: recursive
-- error on: start number > end number
-- edge condition: when processing the start number
fb2 :: (Integral a, Show a) => a -> a -> [String] -> [String]
fb2 x_start x_end x_list
    | (x_start > x_end)  = error "Start number must be less than or equal to end number."
    | (x_end == x_start) = ((fb_show x_end) : x_list) 
    | otherwise          = (fb2 x_start (x_end - 1) ((fb_show x_end) : x_list))

-- method 3: map
fb3 :: (Integral a, Show a) => [a] -> [String]
fb3 x_list = map fb_show x_list

-- method 4: high-order function
-- leaving out the second parameter to `map`.
fb4 :: [Integer] -> [String]
fb4 = (map fb_show)
 
-- check if argument is divisible by 3 or 5
-- using if statements
fb_show :: ( Integral a, Show a) => a -> String
fb_show x = if x `mod` 3 == 0
            then if x `mod` 5 == 0
                 then "FizzBuzz"
                 else "Fizz"
            else if x `mod` 5 == 0
                 then "Buzz"
                 else  show x

-- check if argument is divisible by 3 or 5
-- guards can't be nested, so can't use only guards.
-- using guards + case statements
fb_show' :: ( Integral a, Show a) => a -> String
fb_show' x
    | (x `mod` 3 == 0) = case (x `mod` 5) of 0 -> "FizzBuzz"
                                             _ -> "Fizz"
    | otherwise        =  case (x `mod` 5) of 0 -> "Buzz"
                                              _ -> show x

-- check if argument is divisible by 3 or 5
-- using only case statements
fb_show'' :: ( Integral a, Show a) => a -> String
fb_show'' x = case (x `mod` 3) of 0 -> case (x `mod` 5) of 0 -> "FizzBuzz"
                                                           _ -> "Fizz"
                                  _ -> case (x `mod` 5) of 0 -> "Buzz"
                                                           _ -> show x