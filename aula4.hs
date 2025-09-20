enesimo::[Int] -> Int -> Int
enesimo [] n = -1 
enesimo (h:t) 1 = h 
enesimo (h:t) n = enesimo t (n-1) 


positivos:: [Int] -> [Int]
positivos [] = []
positivos(h:t)
    | h > 0 = h : positivos t 
    | otherwise = positivos t 


unicos::[Int] -> [Int]
unicos [] = []
unicos [h] = [h]
unicos (h:(ht:tt))
    | h == ht = unicos (h:tt)
    | otherwise = h : unicos (ht:tt)


multiplos :: Int -> [Int]
multiplos n = [x*n| x <- [1..10]]


primo :: Int -> Bool 
primo n = length divisores == 2 
where
    divisores = [ x | x <- [1..n], mod n x == 0]
