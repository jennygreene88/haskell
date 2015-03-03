-- define fizzbuzz
fizzbuzz = fb1 [1..100]

-- fizzbuzz method 1: if-else
fb1 :: (Integral a, Show a) => [a] -> [[Char]]
fb1 xs = [ 
	if x `mod` 3 == 0 then 
		if x `mod` 5 == 0 then
			"FB"
		else
			"F"
	else 
		if x `mod` 5 == 0 then 
			"B" 
		else
			show x
		| x <- xs ]

-- fizzbuzz method 2
-- To be determined...