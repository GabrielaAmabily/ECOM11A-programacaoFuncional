
maior1:: Int -> Int -> Int
maior1 a b = if a >= b
    then a
    else b

maior2:: Int -> Int -> Int
maior2 a b
    | a >= b = a
    | otherwise = b

fat1:: Int -> Int
fat1 0 = 1
fat1 n = n * fat1(n-1)

fat2:: Int -> Int
fat2 n = if n == 0 then 1 else n * fat2(n-1)

fat3:: Int -> Int
fat3 n 
    | n==0 = 1
    | otherwise = n*fat3(n-1)

fib:: Int -> Int
fib n
    | n == 1 = 1 
    | n == 2 = 1
    | otherwise = fib(n-1) + fib(n-2)

par:: Int -> String
par n
    | (mod n 2) == 0 = "Par"
    | otherwise = "Impar"

letra:: Char -> String
letra c
    | c >= 'a' && c <= 'z' = "Minuscula"
    | c >= 'A' && c <= 'z' = "Maiuscula"
    | otherwise = "Nao e letra"

funcao:: Int -> Int -> Int -> Int
funcao a b c 
    | a == 0 = b^2 + 3*c
    | a == 1 = 2*c^2 - 3*c
    | a == 2 = 3*c - b^2
    | otherwise = 0

resto:: Int -> Int -> Int 
resto a b
    | a < b = a
    | a >= b = resto (a-b) b

soma:: Int -> Int -> Int
soma a b
    | b == 0 || a == 0 = 0
    | otherwise = a + soma a (b-1)

mdc:: Int -> Int -> Int
mdc x y
    | x > y = mdc (x-y) y
    | x < y = mdc y x
    | x == y = x