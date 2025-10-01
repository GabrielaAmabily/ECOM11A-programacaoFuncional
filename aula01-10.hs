--tuplas

type NomeAluno = String
type MediaNota= Float
type Aluno = (NomeAluno, MediaNota)
type Turma = [Aluno]

approved :: Turma -> Float -> [NomeAluno]
approved tm1 ncorte = [nome | (nome, nota) <- tm1, nota >= ncorte]

--ou 
approved2 :: [(NomeAluno, MediaNota)] -> Float -> [NomeAluno]
approved2 tm1 ncorte = [nome | (nome, nota) <- tm1, nota >= ncorte]

type Ponto3D = (Float, Float, Float)

distancia :: Ponto3D -> Ponto3D -> Float
distancia (x1, y1, z1) (x2, y2, z2) = sqrt((x1-x2)^2 +(y1-y2)^2 + (z1-z2)^2)


--casamento de padroes
compr :: [Int] -> Int
compr [] = 0
compr (_:t) = 1 + compr t 

func :: (Int, Int, Int, Int) -> String 
func(_,_,_,fourth)
    |fourth > 10 = "maior que dez"
    | otherwise = "nao maior que dez"

opp :: (Int, (Int, Int)) -> Int
opp (1, (x,y)) = x + y
opp (2, (x,y)) = x - y
opp (_, (x,y)) = 0
--opp _ = 0


--funçoes de alta ordem 
dobra :: Int-> Int
dobra x = 2*x

quad :: Int-> Int
quad x = x*x

mapLista :: [Int] -> (Int -> Int) -> [Int]
mapLista [] _ = []
mapLista (h:t) func = func h : mapLista t func

--2 funcao
filterList :: [Int] -> (Int -> Bool) -> [Int]
filter [] _ = []
filterList (h:t) filtro
    | filtro h = h : filterList t filtro
    | otherwise = filterList t filtro

ePar :: Int -> Bool
ePar x = mod x 2 == 0 --then True else False

eMult10 :: Int -> Bool
eMult10 x = mod x 10 == 0 --then True else False

menor50 :: Int -> Bool
menor50 x = x < 50 --then True else False

--3 funcao 
buscaLista :: [Int] -> (Int -> Int -> Bool )-> Int
buscaLista [] _ = -1
buscaLista [h] _ = h
buscaLista (h:t) func = if func h x then h else x
    where x = buscaLista t func

maior :: Int -> Int -> Bool
maior a b = a > b

menor :: Int -> Int -> Bool
menor a b = a < b








