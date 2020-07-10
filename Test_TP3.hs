pertenece:: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = if a == x then True else pertenece a xs

--esCerrada :: Eq a => (a -> a -> a) -> [a] -> Bool
--esCerrada f xs = if (pertenece f xs) then True else False


--Te va a devolver el elemento de la lista que despues de aplicarle la funcion, de el resultado mas grande (el numero maximo, o mas grande)
maxf :: Num a => Ord a => (a -> a) -> [a] -> a
maxf f (x:[]) = f x
maxf f (x:xs) = max (f x) (maxf f xs)

--Definir minf (ídem anterior, pero devolviendo el mínimo) usando maxf y la negación unaria (en Haskell se llama negate).
minf :: Num a => Ord a => (a -> a) -> [a] -> a 
minf f (x:[]) = f x
minf f (x:xs) = (-1)*negate( max (f x) (maxf f xs))

--Definir una función supf, que dadas dos funciones del mismo dominio e imagen, devuelva la función máximo puntual 
--(para todo elemento del dominio devuelve el máximo de las aplicaciones de las dos funciones).
supF :: Ord a => (a -> a) -> (a -> a) -> a -> a 
supF f g x = max (f x) (g x)

--Definir inff (ídem anterior, pero devolviendo el mínimo) usando supf.
infF :: Num a => Ord a => (a -> a) -> (a -> a) -> a -> a
infF f g x = (-1)*negate( max (f x) (g x) )

--GenLista
genLista :: Ord a => Num a => Eq a => a -> (a -> a) -> a -> [a]
genLista n f long = if long <= 0 then [] else (n: (genLista (f n) f (long-1)))

-- <-> a partir de GenLista
(<->) :: Ord a => Num a => Eq a => a -> a -> [a]
n <-> m = genLista n (\x -> x+1) ((m-n)+1)

--Ej: filter even [1,2,3,4,5] -> [2,4].
filterL :: (Int -> Bool) -> [Int] -> [Int]
filterL f [] = []
filterL f (x:xs) = if f x then (x:filterL f xs) else filterL f xs

-- o composicion de funciones fog -> f(gx)
(.) :: (a -> a) -> (a -> a) -> a -> a
(f).(g) = \x -> f(g x)

-- Ultimo y segundo de la lista a partir de composicion
--segundo :: ([a] -> a) -> ([a] -> [a]) -> a 
--segundo cab col = (cab)Prelude..(col)

--función curry, que dada una función de dos argumentos, devuelve su equivalente currificada
curryN :: ((a,b) -> c) -> a -> b -> c
curryN f x y = f(x,y)

--curryN (\(x,y)->x+y) 5 7

--Definir la función uncurry, que dada una función currificada de dos argumentos, devuelva su versión no currificada equivalente.
uncurryN :: (a-> b -> c) -> (a,b) -> c 
uncurryN f (x,y) = f x y
--uncurryN (\x-> \y -> x+y)(6,6)

--Definir las versiones currificadas y no currificadas suma de dos enteros y division entera de dos enteros.
sumaCur :: (Int, Int) -> Int                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
sumaCur (x,0) = x
sumaCur (x,y) = sumaCur(x+1,y-1)

sumaUnc :: Int -> Int -> Int
sumaUnc x 0 = x
sumaUnc x y = sumaUnc (x+1) (y-1)

divEntCur :: (Int, Int) -> Int
divEntCur (x, y) = if x < y then 0 else 1 + divEntCur(x-y, y)

divEntUncur :: Int -> Int -> Int
divEntUncur x y = if x < y then 0 else 1 + (divEntUncur (x-y) y)

--Definir (usando funciones currificadas anteriores) a las funciones:
--sucesor de un entero,
sucesor :: Int -> Int
sucesor x = sumaCur (x,1)

--predecesor de un entero,
predecesor :: Int -> Int
predecesor x = sumaCur(0, (x-1))

--mitad de un entero
mitadEnt :: Int -> Int
mitadEnt n = divEntCur (n,2)

--dosVeces la aplicación de una función.
dosVeces :: (a -> a) -> a -> a
dosVeces f n = f (f n)

--cuatroVeces
cuatroVeces :: (a -> a) -> a -> a
cuatroVeces f n = dosVeces f (dosVeces f n)

--función no currificada separar, que dada una condición y una lista devuelva un par de listas donde:
--la primera esté conformada por aquellos elementos de la lista original que cumplan con la condición,
--y la segunda, por aquellos que no la cumplen.

separar :: (a -> Bool) -> [a] -> ([a],[a])
separar f xs = (listaCond f xs, listaNoCond f xs)

listaCond :: (a->Bool) -> [a] -> [a]
listaCond f [] = []
listaCond f (x:xs) = if f x then (x:(listaCond f xs)) else listaCond f xs

listaNoCond :: (a->Bool) -> [a] -> [a]
listaNoCond f [] = []
listaNoCond f (x:xs) = if f x then listaNoCond f xs else (x: listaNoCond f xs)
--separar even [2,3,4,5,6]

--versión currificada de separar usando filter. ??

--funcion en la cual dado un numero n y una listas de listas, se devuelve una listas de listas, con aquellas listas donde
--la mayoria de los elementos son mas grandes que el numero n
mayoria :: Int -> [[Int]] -> [[Int]]
mayoria a [] = []
mayoria a (x:xs) = if ((mayores x a)>(menores x a)) then (x:(mayoria a xs)) else (mayoria a xs) 

mayores :: [Int] -> Int -> Int
mayores [] n = 0
mayores (x:xs) n = if (x > n) then 1 + (mayores xs n) else (mayores xs n)

menores :: [Int] -> Int -> Int
menores [] n = 0
menores (x:xs) n = if (x<n) then 1+(menores xs n) else (menores xs n)

--paraCada hecho en clase
paraCada ::  Int -> Int -> a -> (a -> Int -> a) -> a
paraCada ini fin x f = if (ini > fin) then x else paraCada (ini+1) fin (f x ini) f 

--paraCada 1 3 0 (\x y -> y+x)
--función todos que dada una lista de elementos y una condición sobre los elementos devuelva si todos los elementos de la lista cumplen con la condición.
--Usar paraCada, long y term. Hecho en clase

todos :: (a -> Bool) -> [a] -> Bool
todos f xs = paraCada 0 ((length xs) -1) True (\r y -> r && (f(xs !! y)))
--todos (>3) [3,5,6]

-- armar ninguno a partir de todos
ninguno :: (a -> Bool) -> [a] -> Bool
ninguno f xs = todos (\x -> not(f x)) xs
--ninguno f xs = todos (not.f) xs
--ninguno (<3) [5,6,7,8]

--Programar la función igLong, que dada una lista de listas, diga si todas las sublistas tienen igual longitud.
--Asumir que la lista de listas no debe ser vacía. Usar todos, long y ultimo.

igLong :: [[a]] -> Bool
igLong xss = (todos (\xs -> (length xs) == (length (last xss))) xss)
-- Para cada sublista, se compara la long de la sublista con la ultima sublista de la superlista

--Definir la función while, que dado un valor, una función que represente una condición y una función de transformación, devuelva la aplicación sucesiva de
--la función de transformación sobre el valor dado MIENTRAS se cumpla la función-condición dada. Si el valor inicial cumple la función-condición, ese valor
--deberá ser el devuelto.

whileN :: a -> (a -> Bool) -> (a -> a) -> a 
whileN n c t = if (c n) then (whileN (t n) c t) else n
--whileN (>7) (\x -> x+1)

--Definir la función until, que dado un valor, una función que represente una condición y una función de transformación, devuelva la aplicación sucesiva de la
--función de transformación sobre el valor dado HASTA que se cumpla la función-condición dada. La función de transformación debe aplicarse una vez como mínimo.
untilN :: a -> (a -> Bool) -> (a -> a) -> a
untilN n c t = if (c n) then n else (untilN (t n) c t)
--untilN 5 (>7) (\x -> x+1)

-- Definir ultimo usando while. fst primer elemento del par
ultimoLN :: [a] -> a
ultimoLN [] = error "Lista Vacia"
ultimoLN (x:xs) = fst(whileN (x,xs) (\(x,xs) -> not(null xs)) (\(x,xs) -> (head(xs), tail(xs))))

--Definir long, append, sumaLista y genLista usando while.
longL :: [a] -> Int
longL [] = 0
longL xs = fst( whileN (0,xs) (\(r,xs) -> not(null xs)) (\(r,xs) -> (r+1,tail xs)))

-- append es unir dos listas. snd segundo elemento del par
appendN :: [a] -> [a] -> [a]
appendN xs ys = snd (whileN (xs, ys) (\(xs,ys) -> not(null xs)) (\(xs,ys) -> (tail(xs), (head xs: ys))))

--sumaLista
sumaListaN :: [Int] -> Int
sumaListaN xs = fst(whileN (0, xs) (\(r,xs) -> not(null xs)) (\(r,xs) -> (r+(head xs), tail(xs))))

--Definir una función currificada map, que dada una función y una lista como argumentos, devuelve otra lista que es el resultado de aplicar la función original
--(elemento a elemento) a cada uno de los elementos de la lista original.
--		Ej: map f [1,2,3] -> [f 1,f 2,f 3].

mapN :: (a -> a) -> [a] -> [a]
mapN f [] = []
mapN f (x:xs) = ((f x): (map f xs)) 

--Definir una función no currificada mapn2, que aplique una función a todos los elementos de todas las listas de una lista de listas.
mapNListas :: (a->a) -> [[a]] -> [[a]]
mapNListas f [] = []
mapNListas f (xs:xss) = ((mapN f xs) : (mapNListas f xss))

--Definir una función mapArb, una versión de map que en lugar de recibir una lista, recibe un elemento de tipo ArbBin (árbol binario).
data ArbBin a = Hoja a | ArbNoVac a (ArbBin a) (ArbBin a) deriving (Eq, Show)

mapArbBin :: (a -> a) -> ArbBin a -> ArbBin a 
mapArbBin f (Hoja a) = Hoja (f a)
mapArbBin f (ArbNoVac a (x) (y)) = (ArbNoVac (f a) (mapArbBin f x) (mapArbBin f y))

--Definir una función mapo, una versión de map que toma una función de dos argumentos y una lista de pares de valores, y devuelve la lista de aplicaciones
--de la función a cada par.
mapo :: (a -> b -> c) -> [(a,b)] -> [c]
mapo f [] = []
mapo f ((x,y): xs) = ((f x y): mapo f xs)

--mapo (\x y -> y+x) [(2,3),(4,5),(3,8)]

--Definir una función mapo2, una versión de mapo que toma una función currificada de dos argumentos y dos listas (de igual longitud), y
--devuelve una lista de aplicaciones de la función a cada elemento correspondiente a las dos listas. Esta función en Haskell se llama zipWith.
mapo2 :: ((a,b) -> c) -> [a] -> [b] -> [c]
mapo2 f [] [] = []
mapo2 f (x:xs) (y:ys) = if length(x:xs) /= length(y:ys) then error "no son de igual longitud las listas" else ( (f (x,y)): mapo2 f xs ys )

--mapo2 (\(x,y) -> x+y) [2,3,4] [2,3,4]

--Modificar la función mapo2 anterior (si lo cree necesario) para que pueda ser usada por una función sumamat, que suma dos matrices y devuelve otra matriz.
--Asumir que las dos matrices de entrada tienen la misma cantidad de filas y de columnas.

data Matriz a = Mat Int Int [a] deriving (Eq, Show)

--sumarListas :: (Num a) => [a] -> [a] -> [a]
--sumarListas x [] = x
--sumarListas [] y = y
--sumarListas (x:xs) (y:ys) = (x+y : sumarListas xs ys)
--
--sumaMat :: (Num a) => Matriz a -> Matriz a -> Matriz a 
--sumaMat (Mat f1 c1 x) (Mat f2 c2 y) = if (f1 == f2) && (c1 == c2) then (Mat f1 c1 (sumarListas x y)) else error "No se pueden sumar matrices"
--
--mapo2Mat :: (([a],[b]) -> [c]) -> Matriz a -> Matriz b -> Matriz c 
--mapo2Mat f (Mat f1 c1 xs) (Mat f2 c2 ys) = if (f1 == f2) && (c1 == c2) then (Mat f1 c1 (f (xs,ys))) else error "No se puede realizar la funcion"


--Si xs es una lista, la evaluación de map f (map g xs), xs requiere recorrer dos veces la lista xs. Simplificar la expresión de modo que sólo deba recorrérsela
--una vez, definiendo una función simplif, que reciba las dos funciones y la lista.

simplif :: (a -> a) -> (a -> a) -> [a] -> [a]
simplif f g [] = []
simplif f g (x:xs) = ((f(g x)): (simplif f g xs))

--simplif (\x -> x*2) (\y -> y+1) [1,2,3]

--Definir una función sigma que calcule la suma de una serie. Los parámetros son lower (límite inferior de la sumatoria), upper (límite superior) y
--una función que indique el término general en la serie. Usar las funciones sumaLista, <-> y map.

sigma :: Int -> Int -> (Int -> Int) -> Int
sigma l h f = sumaListaN (map f (l <-> h))

--Definir una función pascal que devuelva en forma de listas el triángulo de Pascal (o de Tartaglia) hasta la altura pedida.
--No se permite el uso de números combinatorios. Usar ultimo, paresConsec (función que dada una lista devuelve todos los pares de elementos
--junto a su sucesor) y map.
--		Ejemplo de paresConsec: paresConsec [7,3,2,5]  [(7,3),(3,2),(2,5)].
--		Ej: pascal 4  [ [1], [1,1], [1,2,1], [1,3,3,1], [1,4,6,4,1] ]

--No lo entendi 


--Definir la función de orden superior mapearF, que dada una lista de funciones y una lista (donde ambas tienen la misma cantidad de elementos),
--devuelva otra lista con cada resultado de la aplicación de cada función a su elemento correspondiente de la lista
mapearF :: [(a->a)] -> [a] -> [a]
mapearF [] [] = []
mapearF (x:xs) (y:ys) = if length (x:xs) /= length (y:ys) then error "No tienen la misma cantidad de elementos" else ((x y): mapearF xs ys)

--mapearF [(\x -> x+1),(\x -> x*2)] [1,2]
--mapearF [(\x -> x+1),(\x -> x*2)] [1,2,3]

--Definir las funciones factorial, suma (todos los elementos de una lista), rev, append y aparear de manera que sean recursivas de cola.
sumaL :: Num a => [a] -> a -> a 
sumaL [] n = n
sumaL (x:xs) n = sumaL xs (x+n)

sacarultimo :: [a] -> [a]
sacarultimo (x:[]) = []
sacarultimo (x:xs) = (x: (sacarultimo xs))

rev :: [a] -> [a] -> [a]
rev [] xs = xs
rev (x:xs) ys = rev xs (x:ys)

aparear :: [a] -> [a] -> [a]
aparear x [] = x
aparear [] y = y
aparear (x:xs) (y:ys) = (x:(y: (aparear xs ys)))

--Definir las funciones par (devuelve si un número es par o no) e impar usando recursión mutua (indirecta).
par :: Int -> Bool
par n = if impar n then False else True

impar :: Int -> Bool
impar n = if (mod n 2) == 0 then False else True

--Definir las funciones cong0, cong1 y cong2 (devuelve si un número es congruente a cero, uno o dos respectivamente) usando recursión mutua.
cong0 :: Int -> Int -> Bool
cong0 n m = if (mod n m) == 0 then True else False

-- ??

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f b [] = b
foldr2 f b (x:xs) = f x (foldr2 f b xs)

foldl2 :: (b -> a -> b) -> b -> [a] -> b
foldl2 f b [] = b
foldl2 f b (x:xs) = foldl2 f (f b x) xs

--i)	Evaluar a mano foldr (+) 0 [7,3,13] y foldl (+) 0 [7,3,13]
--   Recursion de pila:
--   + 7 (foldr + 0 [3,13])
--   + 7 (+ 3 (foldr + 0 [13]))
--   + 7 (+ 3 (+ 13 (foldr + 0 [])))
--   + 7 (+ 3 (+ 13 ( 0 ))) = 23

--   Recursion de cola:
--   foldl + (+ 0 7) [3,13]
--   foldl + (+ 7 3) [13]
--   foldl + (+ 10 13) []
--   foldl + (23) [] = 23

--ii)	Evaluar a mano foldr append [1] [ [2],[3],[4] ] y foldl append [1] [ [2],[3],[4] ]

--    append [2] (foldr append [1] [[3],[4]])
--    append [2] (append [3] (foldr [1] [[4]]))
--    append [2] (append [3] (append [4] (fold [1] [])))
--    append [2] (append [3] (append [4] [1])) = [2,3,4,1]
--
--    foldl append (append [1] [2]) [[3],[4]]
--    foldl append (append [2,1] [3] ) [[4]]
--    foldl append (append [3,2,1] [4]) [] = [3,2,1,4]

--iii)	Sacar conclusiones sobre las evaluaciones anteriores.
    
--    Que hay que ser muy conciente acerca de que es lo que queremos lograr, o resolver, y como lo vamos a resolver, porque dependiendo de la manera en que lo hagamos
--    puede variar o diferir el resultado.

--iv)	Clasificar a foldr y foldl según el tipo de recursión.

-- foldr recursion de pila, foldl recursión de cola.

-- Definir usando foldr las funciones:
-- sumaLista (todos los elementos de una lista),
sumaListaF :: Num a => [a] -> a
sumaListaF (zs) = foldr2 (+) (0) (zs)

-- idL (identidad de una lista),
idL :: [a] -> [a]
idL xs = foldr2 (\xs -> xs) xs []

-- member,
member :: a -> [a] -> Bool
member x ys = (foldr2 (x==y) x ys) 

-- append,
-- rev,
-- norma2 (de un vector representado como lista de números), flat, insort (ordenamiento de una lista según el método de inserción), partes,
-- compFuncs (composición de cero, una o más funciones), filter y map.


