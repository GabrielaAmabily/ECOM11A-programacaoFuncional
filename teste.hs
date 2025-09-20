absoluto :: Int -> Int
absoluto x = if x > 0 then x else x*(-1)

--fatorial 
fatorial :: Int -> Int 
fatorial n 
    | n == 0 = 1 
    | n > 0 = n * (fatorial (n -1))

    
moduloResto :: Int -> Int -> Int 
moduloResto a b 
    | b > a = a 
    | b == a = 0
    | a > b = moduloResto (a-b) b 

multSoma :: Int -> Int -> Int
multSoma x n 
    | n == 0 = 0
    | otherwise = x + (multSoma x (n-1))

--14 
ehPrimo :: Int -> Bool
ehPrimo x
  | x <= 1 = False
  | otherwise = temDivisor x 2
  where
    temDivisor n i
      | i * i > n = True
      | mod n i == 0 = False
      | otherwise = temDivisor n (i + 1)



