-- main fizzbuzz function
fizzbuzz = fb1 [1..100]
fizzbuzz_r = fb2 1 100 []

-- method 1: list comprehension
fb1 :: [Int] -> [String]
fb1 x_list = [ fb_show x | x <- x_list ]

-- method 2: recursive
fb2 :: Int -> Int -> [String] -> [String]
fb2 x_min x_max x_list = if (x_max == x_min) then ((fb_show x_max) : x_list) else (fb2 x_min (x_max - 1) ((fb_show x_max) : x_list))

-- check divisible by 3 or 5
--fb_show :: (Integral a, Show a) => a -> String
fb_show x = if x `mod` 3 == 0 then 
	if x `mod` 5 == 0 then
		"FizzBuzz"
	else
		"Fizz"
else 
	if x `mod` 5 == 0 then 
		"Buzz" 
	else
		show x



