-- quadrado de um numero
quadrado :: Float -> Float --assinatura da função
quadrado num = num * num --função


-- raiz de uma equação de primiero grau 
raiz :: Float -> Float -> Float 
raiz a b = (-b)/a 
--ax + b = 0
--x = -b/a

funcao :: Float -> Float -> Float
funcao x y = raiz (quadrado x) y

polinomio :: Int -> Int 
polinomio x = x*x + 10*x + 2

-- exercicios 
area :: Float -> Float 
area r = r * r * pi

perimetro :: Float -> Float 
perimetro r = pi * r * 2

hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt ((a * a)+(b * b))

diferencaArea :: Float -> Float -> Float
diferencaArea r1 r2 = abs (area r2 - area r1)

-- div e mod
-- ceiling arredonda pra cima
-- floor aredonda pra baixo 
-- round arredonda 

compressao :: Float -> Float -> Float
compressao f k = f/k

energiaPotencial :: Float -> Float -> Float
energiaPotencial f k = 1/2 * k * (compressao f k)^2



