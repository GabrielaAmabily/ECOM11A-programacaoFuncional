heron :: Float -> Float -> Float -> Float
heron a b c = sqrt(s*(s-a)*(s-b)*(s-c))
    where
        s = (a+b+c)/2



bhaskara :: Float -> Float -> Float -> Int
bhaskara a b c 
    | delta > 0 = 2
    | delta == 0 = 1
    | delta < 0 = 0
    where 
        delta = (b*b)-(4*a*c)



areacilindro :: Float -> Float -> Float 
areacilindro r h = let areabase = pi*r*r
                       arealado = 2*pi*r*h
                   in 2*areabase + arealado 
   


areaheron :: Float -> Float -> Float -> Float
areaheron a b c =  let s = (a+b+c)/2
                   in sqrt((s*(s-a)*(s-b)*(s-c)))



imcMsg :: Float -> Float -> String 
imcMsg w h 
    |imc <= 18.5 = "abaixo do peso" 
    |imc <= 25 = "peso ideal" 
    |imc <= 30 = "levemente acima do peso" 
    |otherwise = "abaixo do peso" 
    where
        imc = w/(h^2)



employeeSeg :: Float -> Float -> Char
employeeSeg hrs vhrs = let salary = hrs*vhrs
    in if salary < 1000 then 'A'
       else if salary < 5000 then 'B'
            else 'C' 



quantidade :: [Int] -> Int 
quantidade[] = 0 
quantidade (h:t) = 1 + quantidade t 


soma :: [Int] -> Int 
soma[] = 0 
soma (h:t) = h + soma t 


quadradolista :: [Int] -> [Int]
quadradolista [] = []
quadradolista (h:t) = h*h : quadradolista t 
    

verificaCaracter :: String -> Char -> Bool
verificaCaracter [] letra = False
verificaCaracter(h:t) letra 
    |h == letra = True
    |otherwise = verificaCaracter t letra


maior :: [Int] ->Int 
maior [] = -1 
maior(h:t) = if h >= maiorcauda then h else maiorcauda
    where
        maiorcauda = maior t

