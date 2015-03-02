-- define fizzbuzz
fizzbuzz = fb1 [1..100]

-- fizzbuzz method 1
fb1 xs = [ 
	if x `mod` 3 == 0 then 
		if x `mod` 5 == 0 then
			do
				putStr "FB"
		else
			do
				putStr "F"
	else 
		if x `mod` 5 == 0 then 
			do
				putStr "B" 
		else
			do
				putStr "x"
		| x <- xs ]

-- fizzbuzz method 2
fb2_init xs = [fb2_process x | x <- xs]
fb2_process x = if x `mod` 3 == 0 then "F" else "xxx"

-- finished
fb_done = "END"