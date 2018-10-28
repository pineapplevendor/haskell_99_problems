module Solutions5
(grayCode,
) where

import Solutions1
import Solutions2
import Solutions3
import Solutions4

--problem 46, 47, and 48 are uninteresting 

--problem 49
grayCode :: Int -> [[Char]]
grayCode 0 = [""]
grayCode 1 = ["0","1"]
grayCode n = 
    let prev = grayCode (n-1)
        reflected = reverse prev
        prefix0 = map ('0':) prev
        prefix1 = map ('1':) reflected
    in prefix0 ++ prefix1

--problem 50 (unclear instructions)
