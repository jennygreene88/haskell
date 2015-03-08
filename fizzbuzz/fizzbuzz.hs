-- main fizzbuzz function
fizzbuzz = fb1 [1..100]
fizzbuzz_r = fb2 1 100 []

-- method 1: list comprehension
fb1 :: (Integral a, Show a) => [a] -> [String]
fb1 x_list = [ fb_show x | x <- x_list ]

-- method 2: recursive
-- error: when start number > end number
-- edge condition: when processing the start number
fb2 :: (Integral a, Show a) => a -> a -> [String] -> [String]
fb2 x_start x_end x_list
    | (x_start > x_end)  = error "Start number must be less than or equal to end number."
    | (x_end == x_start) = ((fb_show x_end) : x_list) 
    | otherwise          = (fb2 x_start (x_end - 1) ((fb_show x_end) : x_list))

-- check if argument is divisible by 3 or 5
-- using if statements
fb_show :: ( Integral a, Show a) => a -> String
fb_show x = if x `mod` 3 == 0
            then if x `mod` 5 == 0
                 then "FizzBuzz"
                 else "Fizz"
            else if x `mod` 5 == 0
                 then "Buzz"
                 else show x

-- guards can't be nested, so can't use only guards.
-- using guards + case statements
fb_show' :: ( Integral a, Show a) => a -> String
fb_show' x
    | (x `mod` 3 == 0) = case (x `mod` 5) of 0 -> "FizzBuzz"
                                             _ -> "Fizz"
    | otherwise        =  case (x `mod` 5) of 0 -> "Buzz"
                                              _ -> show x

-- using only case statements
fb_show'' :: ( Integral a, Show a) => a -> String
fb_show'' x = case (x `mod` 3) of 0 -> case (x `mod` 5) of 0 -> "FizzBuzz"
                                                           _ -> "Fizz"
                                  _ -> case (x `mod` 5) of 0 -> "Buzz"
                                                           _ -> show x