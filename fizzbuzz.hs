
fizzbuzz = fb1 [1..100]


fb1 xs = [ if x `mod` 3 == 0 then ("FB") else ("F") | x <- xs ]


fb2_init xs = [fb2_process x | x <- xs]
fb2_process x = if x `mod` 3 == 0 then "F"


fb_done = "  DONE  "