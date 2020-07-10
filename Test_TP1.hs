--1

signo :: Int -> Int
signo n = if n < 0 then -1 else (if n > 0 then 1 else 0)

--NO VA signo(n) sino signo n cuando lo declarás. y no olvidarse los parentesis para señalar la precedencia.

negativo :: Int -> Bool
negativo n = if signo n /= -1 then False else True

-- El distinto se escribe /= y NO <> ó !=
-- Otra variante del ejercicio negativo: negativo n = signo n < 0

--2

maximo :: Int -> Int -> Int
maximo n1 n2 = if n1 > n2 then n1 else n2

max3 :: Int -> Int -> Int -> Int
max3 x y z = max x (max y z)

minimo :: Int -> Int -> Int
minimo x y = if x < y then x else y

--3

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * (factorial (x - 1))

-- Deberian ser naturales para los factoriales.

-- x >= 1 y 0<= y <= x
combinatorio :: Int -> Int -> Int
combinatorio x y = div (factorial x) (factorial y * factorial (x - y))

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

divisiblepor :: Int -> Int -> Bool
divisiblepor x y = mod x y == 0

--4

esvacia :: [a] -> Bool
esvacia [] = True
esvacia (x:xs) = False

cabeza :: [a] -> a
cabeza (x:xs) = x

resto :: [a] -> [a]
resto (x:xs) = xs

--5

long :: [a] -> Int
long [] = 0
long (x:xs) = long (xs) + 1

sumalista :: [Int] -> Int
sumalista [] = 0
sumalista (x:xs) = x + sumalista xs

member :: [Int] -> Int -> Bool
member [] n = False
member (x:xs) n = if x == n then True else member xs n

append :: [a] -> [a] -> [a]
append [] xs = xs
append (x:xs) ys = (x: append xs ys)

tomar :: [a] -> Int -> [a]
tomar [] n = []
tomar xs 0 = []
tomar (x:xs) n = (x : tomar xs (n-1))

term :: [a] -> Int -> a
term (x:xs) n = if n == 1 then x else term xs (n-1)

reversa :: [a] -> [a]
agregfinal :: a -> [a] -> [a]

agregfinal x [] = (x: [])
agregfinal x (y:ys) = (y: agregfinal x ys)

reversa [] = []
reversa (x:xs) = agregfinal x (reversa xs)

maxl :: [Int] -> Int
maxl (x:[]) = x
maxl (x:xs) = if x > cabeza xs then maxl (x: resto(xs)) else maxl xs

cuenta :: Eq a => [a] -> a -> Int
cuenta [] a = 0
cuenta (x:xs) a = if  x == a then 1+(cuenta xs a) else cuenta xs a

repite :: a -> Int -> [a]
repite a 0 = []
repite a n = (a: repite a (n-1))

ultimo :: [a] -> a
ultimo (x:[]) = x
ultimo (x:xs) = ultimo xs

sacarultimo :: [a] -> [a]
sacarultimo (x:[]) = []
sacarultimo (x:xs) = (x: (sacarultimo xs))

--8
capicua :: Eq a => [a] -> Bool
capicua [] = True
capicua (x:[]) = True
capicua (x:xs) = if x == ultimo xs then capicua (sacarultimo xs) else False

--9
flat :: [[a]] -> [a]
flat [] = []
flat (x:xs) = append x (flat xs)

longLl :: [[a]] -> Int
longLl [] = 0
longLl xs = long (flat xs)

--10
intercalar :: [[a]] -> [[a]] -> [[a]]
intercalar [] ys = ys
intercalar xs [] = xs
intercalar (x:xs) (y:ys) = (x: (y: (intercalar xs ys) ))

aparear :: [Int] -> [Int] -> [Int]
aparear [] ys = ys
aparear xs [] = xs
aparear (x:xs) (y:ys) = if x > y then (y: (aparear (x:xs) ys)) else (x:(aparear xs (y:ys)))

--11 decAHex REVER
decAHex :: Int -> [Int]
decAHex n = if (n<16) then ((mod n 16):[]) else ((mod n 16):decAHex(div n 16))

--12
perfecto :: Int -> Bool
sumaDivisores :: Int -> Int -> Int
sumaDivisores n 1 = 1
sumaDivisores n m = if (divisiblepor n m) then m + (sumaDivisores n (m-1)) else sumaDivisores n (m-1)
perfecto n = if (sumaDivisores n (n-1)) == n then True else False 

--13
esPrefija :: Eq a => [a] -> [a] -> Bool
esPrefija [] xs = True
esPrefija xs [] = False
esPrefija (x:xs) (y:ys) = if x == y then esPrefija xs ys else False

posicion :: Eq a => [a] -> [[a]] -> Int
contiene :: Eq a => [a] -> [a] -> Bool
contiene [] [] = True
contiene [] ys = True
contiene xs [] = False
contiene (x:xs) (y:ys) = contiene xs ys

posicion xs [] = -10000
posicion xs (y:ys) = if contiene xs y then 1 else 1 + (posicion xs ys)

subcadena :: [Char] -> Int -> Int -> [Char]
subcadena [] n m = []
subcadena (x:xs) n m = if n == 1 then if m == 1 then (x:[]) else (x:(subcadena xs n (m-1))) else subcadena xs (n-1) m

--14
--quicksort :: [Int] -> [Int]
--quicksort [] = []
--quicksort xs = quicksort2 ([], (maxl xs: []), xs)
--
--quicksort2 :: [Int] -> [Int]
--quicksort2 xs (y:ys) (z:zs) = if y == z then (quicksort2 (z:(y:ys)) zs) else if z<y then (quicksort2 (z:(y:ys)) zs) else (y:(z:ys))

quicksort1 :: (Ord a) => [a] -> [a]
quicksort1 [] = []
quicksort1 (x:xs) =
  let smallerSorted = quicksort1 [a | a <- xs, a <= x]
      biggerSorted = quicksort1 [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

--Clase amitrano:
 
--enumFromTo 4 9 --> [4,5,6,7,8,9] TIENE QUE SER EL PRIMERO MENOR QUE EL SEGUNDO

(<->) :: Int -> Int -> [Int]
--x <-> y
--      | x > y = []
--      |otherwise = (x: ((x+1) <-> y))
x <-> y = if (x > y) then [] else (x: ((x+1) <-> y))

factorial2 :: Int -> Int
factorial2 n = product (1 <-> n)

--partes

--agregElemAListas :: a -> [[a]] -> [[a]]

--agregElemAListas (x:[]) = []
--agregElemAListas (x: (ys:[])) = ((x:ys):[])
--agregElemAListas (x:(ys:yss)) = ((x:ys),agregElemAListas x yss)

partes :: [a] -> [[a]]
partes [] = [[]]
partes(x:xs) = (partes xs) ++ (addAll x (partes xs))
--Otras dos formas de hacer esta ultima regla:
--partes(x:xs) = (partes xs) ++ (addAll x yss) where yss = (partes xs)
--partes(x:xs) = let yss = (partes xs) in yss ++ (addAll x yss)

addAll :: a -> [[a]] -> [[a]]
addAll x [] = []
--addAll x (ys:[]) = ((x:ys):[]) NO NECESARIO
addAll x (ys:yss) = ((x:ys) : addAll x yss)