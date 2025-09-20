--1
perimetro :: Float -> Float -> Float
perimetro w h = 2*w + 2*h

areaTri :: Float -> Float -> Float
areaTri b h = b*h/2

--2
veloMedia :: Float -> Float -> Float 
veloMedia s t = s/t 

difVeloMedia :: Float -> Float -> Float -> Float
difVeloMedia s t1 t2 = abs((s/t1) - (s/t2))

--3
converte :: Float -> Float -> Float
converte r d = r / d 

--4
temperatura :: Float -> Float 
temperatura c = c * 1.8 + 32

--5 
ehPar :: Int -> Bool
ehPar n = mod n 2 == 0 

ehMultiplo :: Int -> Int -> Bool
ehMultiplo i j = mod i j == 0 

--6 
absoluto :: Int -> Int 
absoluto n = if n > 0 
    then n 
    else n*(-1)

--7 
menor :: Char -> Char -> Char  
menor c1 c2 = if c1 > c2 -- menor = min
    then c2 
    else c1 

--8 
triMenores :: Float -> Float -> Float -> Float
triMenores a b c
    | a <= b && a <= c = a 
    | b <= a && b <= c = b
    |otherwise = c 

--9 
seila :: Int -> Int -> Int 
seila n y 
    |y == 0 = 1
    |y>=1 &&  y<= 5 = n 
    |y > 5 = n^5 

--10
ackerman :: Int -> Int -> Int 
ackerman m n 
    |m==0 = n +1
    |m>0 && n==0 = ackerman (m-1) 1 
    |m>0 && n>0 = ackerman(m-1) (ackerman m (n-1))

--11
triangulo :: Float -> Float -> Float -> Int 
triangulo a b c  -- 1 para escaleno, 2 para isósceles e 3 para equilátero.
    | a==b && b==c = 3
    | a==b || b==c || c==a = 2 
    |otherwise = 1 

--12 
funcao12 :: Float -> Float -> Float
funcao12 a b 
    | a < b = -(a*b)
    | a == b = 0 
    |otherwise = a*b 

--13 soma todos multiplos de y de 0 a x 
imitaC :: Int -> Int -> Int 
imitaC x y = sum [i | i <- [0..x], i `mod` y == 0]

--14
ehPrimo :: Int -> Bool 
ehPrimo x 
    | x == sum [i | i <- [2..x], x `mod` i == 0] = True 
    | otherwise = False 

--15 
enesimoFibo :: Int -> Int
enesimoFibo n 
    | n <= 0 = -1
    | n <= 2 = 1
    | otherwise = enesimoFibo(n-1) + enesimoFibo(n-2)

--16
movUni1 :: Float -> Float -> Float -> Float 
movUni1 vi vf t = vi*t + a/2 * (t^2)
    where
        a = (vf - vi)/t

movUni2 :: Float -> Float -> Float -> Float 
movUni2 vi vf t = let a = (vf - vi)/t
                  in vi*t + a/2 * (t^2)

--17 
soma :: Int -> Int -> Int 
soma a b 
    | a < 0 || b < 0 = -1
    | a == 0 = b
    | b == 0 = a 
    | otherwise = if a > b
        then soma (a+1) (b-1)
        else soma (b+1) (a-1)

--18 
-- recebe duas listas ordenadas e retorna uma nova lista com os elementos únicos.
listaUnicos :: [Int] -> [Int] -> [Int]
listaUnicos lista1 lista2 = percorre lista1 lista2 []

-- junta tudo em uma unica lista ordenada, e ve qual n ta repetido
percorre :: [Int] -> [Int] -> [Int] -> [Int]
percorre [] [] acc = reverse acc
percorre [] (y:ys) acc = percorre [] ys (y:acc)
percorre (x:xs) [] acc = percorre xs [] (x:acc)
percorre (x:xs) (y:ys) acc
  | x < y     = percorre xs (y:ys) (x:acc)
  | x > y     = percorre (x:xs) ys (y:acc)
  | otherwise = percorre xs ys acc

--19 
enesimo :: Int -> [Int] -> Int 
enesimo n [] = -1
enesimo 1 (h:t)  = h 
enesimo n (h:t) = enesimo (n-1) t 

--20 
inserirLista :: Int -> Int -> [Int] -> [Int]
inserirLista n novo lista
  | n <= 1 = novo : lista
inserirLista _ novo [] = [novo]
inserirLista n novo (h:t) = h : inserirLista (n-1) novo t





