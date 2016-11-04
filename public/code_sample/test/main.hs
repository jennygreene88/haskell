double x = x + x

isLessThanTen x = if x < 10
	then True
	else False

test xs = sum [1 | x <- xs]