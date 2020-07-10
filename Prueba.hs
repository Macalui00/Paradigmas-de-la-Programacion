-- i)	Definir una función pares, que dado un número entero positivo, devuelva una lista de pares de números enteros positivos, con el primer componente del par menor o igual que el segundo componente, cuya suma sea igual al número entero positivo dado. Por ejemplo:
-- 			pares 7 = [(1,6),(2,5),(3,4)]
-- 			pares 10 = [(1,9),(2,8),(3,7),(4,6),(5,5)]
-- 		Definirla utilizando recursión de pila.

pares :: Int -> [(Int,Int)]
pares n = if n <= 1 then [] else (pares2 n (n-1) 1)

pares2 :: Int -> Int -> Int -> [(Int,Int)]
pares2 p m u = if (m == u) && ((m+u) == p) then ((m,u): []) else if (m+u) == p then ((u,m):(pares2 p (m-1) (u+1))) else pares2 p (m-1) (u+1)

-- paresMonad :: Int -> Maybe[(Int,Int)]
-- paresMonad n = if n <= 1 then Nothing else pares2 n (n-1) 1
-- evaluacion aplicativa, impaciente o eager:
-- take 2 [x+1 | x <- [2,5..], even x] =
-- take 2 (concat [[x+1 | even x] | x <- [2,5 ..]]) =
-- take 2 (foldr (++) [] (map (\x -> [x+1 | even x]) [2,5..])) =
-- take 2 (foldr (++) [] (((\x -> [x+1 | even x]) 2): (map (\x -> [x+1 | even x]) [5,8..]))) =
-- take 2 ((++) ((\x -> [x+1 | even x]) 2) (foldr (++) [] (map (\x -> [x+1 | even x]) [5,8..]))) =
    
-- evaluacion normal order o lazy:
-- take 2 [x+1 | x <- [2,5..], even x] = []

--3)	i)	Definir el tipo de datos ArbolNRot, que representa un árbol n-ario donde los nodos poseen un valor de un tipo dado, y donde los arcos (rótulos) que unen un nodo con cada subárbol también poseen un valor de eventualmente otro tipo dado. 
-- Considerar que el tipo puede ser paramétrico.

data ArbolNRot a b = Hoja a | Nodo a b [ArbolNRot a b] deriving (Eq,Show)

obtenerRotulosRama :: ArbolNRot a b -> [b]
obtenerRotulosRama (Hoja n) = []
obtenerRotulosRama (Nodo n m (x:xs)) = (m: (obtenerRotulosRama x)) ++ (obtenerRotulosRama2 xs)

obtenerRotulosRama2 :: [ArbolNRot a b] -> [b]
obtenerRotulosRama2 [] = []
obtenerRotulosRama2 (x:xs) = (obtenerRotulosRama x) ++ (obtenerRotulosRama2 xs)

esHoja :: ArbolNRot a b -> Bool
esHoja (Hoja a) = True
esHoja (Nodo a b xs) = False

-- obtenerRotulosRama (Nodo 4 "b" [(Nodo 2 "c" [Hoja 5]), (Nodo 3 "j" [(Nodo 6 "f" [Hoja 3])])])

-- 	ii)	Definir la función de orden superior foldrANR que dado un árbol ArbolNRot en forma análoga a la función foldr de las listas (recursión de pila),
-- y que posea parámetros adecuados que apliquen a este tipo de datos.

-- foldr :: (a -> b -> b) -> b -> [a] -> b
foldrANR :: (b -> b -> b) -> b -> ArbolNRot a b -> b
foldrANR f x (y) = foldr f x (obtenerRotulosRama y)

-- foldrANR1 :: (b -> b -> b) -> b -> ArbolNRot a b -> ArbolNRot a b
-- foldrANR1 f x (Nodo m n (xs)) = Nodo m (f x n) (foldrANR2 f x xs)
-- 
-- foldrANR2 :: (b -> b -> b) -> b -> [ArbolNRot a b] -> [ArbolNRot a b]
-- foldrANR2 f x [] = []
-- foldrANR2 f x ((Hoja m):xs) = (Hoja m: foldrANR2 f x xs)
-- foldrANR2 f x ((Nodo m n ys):xs) = (foldrANR1 f x (Nodo m n ys): foldrANR2 f x xs)

-- la version posta:
foldrANR1 :: (b -> b -> b) -> b -> ArbolNRot a b -> b
foldrANR1 f x (Hoja m) = x
foldrANR1 f x (Nodo m n (xs)) = (f n (foldrANR2 f x xs))

foldrANR2 :: (b -> b -> b) -> b -> [ArbolNRot a b] -> b
foldrANR2 f x [] = x
foldrANR2 f x ((Hoja m):xs) = foldrANR2 f x xs
foldrANR2 f x ((Nodo m n ys):xs) = f n (f (foldrANR2 f x xs) (foldrANR2 f x ys))

--foldrANR (+) 0 (Nodo "p" 1 [(Nodo "c" 2 [Hoja "r"]), (Nodo "j" 3  [(Nodo "f" 6 [Hoja "m"])])])


--foldrANR f x (Nodo a b (y:ys)) =  if esHoja y then f b (foldrANR f x (foldrANR2 f x (ys))) else f b (foldrANR f x (foldrANR2 f x (y:ys)))

--foldrANR2 :: (a -> b -> b) -> b -> [ArbolNRot a b] -> b
--foldrANR2 f x (y: [])= foldrANR f x y
--foldrANR2 f x (y: ys) = if esHoja y then (foldrANR2 f x ys) else (foldrANR f x y)

-- 	iii)	Definir la función rotulosRamas, que dado un árbol ArbolNRot con nodos de un tipo arbitrario y rótulos enteros, retorne una lista de enteros donde 
-- cada elemento entero se asocia a cada rama del árbol y corresponde a la suma de todos los rótulos que corresponden a la misma rama. Usar foldrANR.
rotulosRamas :: ArbolNRot a Int -> [Int]
rotulosRamas (Nodo m n []) = (n:[])
rotulosRamas (Nodo m n (x:xs)) = if esHoja x then rotulosRamas (Nodo m n xs) else 
    if (n == foldrANR (+) 0 x) then rotulosRamas (Nodo m n xs)
     else error "suma de rotulos erronea"

-- rotulosRamas (Hoja m) = []
-- rotulosRamas (Nodo m n (xs)) =if n == (foldrANR (+) -n (Nodo m n xs)) then (n: rotulosRamas x) ++ rotulosRamas2 xs else error "suma de rotulos erronea"
-- 
-- rotulosRamas2 :: [ArbolNRot a Int] -> [Int]
-- rotulosRamas2 [] = []
-- rotulosRamas2 (x:xs) = if esHoja x then rotulosRamas2 xs else rotulosRamas x ++ rotulosRamas2 xs

--rotulosRamas (Nodo "p" 4  [(Nodo "c" 2 [(Nodo "c" 1 [Hoja "r"]), (Nodo "b" 1 [Hoja "r"])]), (Nodo "j" 2  [(Nodo "p" 1 [Hoja "m"]),(Nodo "j" 1 [Hoja "m"])])])


-- rotulosRamas :: ArbolNRot a Int -> [Int]
-- rotulosRamas (Nodo m n []) = (n:[])
-- rotulosRamas (Nodo m n (x:xs)) = if esHoja x then rotulosRamas (Nodo m n xs) else 
--     if (n == suma(x:xs)) then rotulosRamas (Nodo m n xs)
--      else error "suma de rotulos erronea"
-- 
-- suma :: [ArbolNRot a b] -> Int
-- suma [] = 0
-- suma (x:xs) = (foldrANR (+) 0 x) + (suma xs)

-- rotulosRamas :: ArbolNRot a Int -> [Int]
-- rotulosRamas (Nodo m n []) = (n:[])
-- rotulosRamas (Nodo m n (x:xs)) = rotulosRamas2 (Nodo m n (x:xs)) ++ rotulosRamas2 x ++ rotulosRamas3 xs
-- 
-- rotulosRamas2 :: ArbolNRot a Int -> [Int]
-- rotulosRamas2 (Nodo m n []) = (n:[])
-- rotulosRamas2 (Nodo m n (x:xs)) =
--      if esHoja x then rotulosRamas (Nodo m n xs) else 
--     if (n == (foldrANR1 (+) 0 (Nodo m n (x:xs)) + foldrANR2 (+) 0 xs)) then rotulosRamas (Nodo m n xs)
--      else error "suma de rotulos erronea"
-- 
-- rotulosRamas3 :: [ArbolNRot a Int] -> [Int]
-- rotulosRamas3 [] = []
-- rotulosRamas3 (x:xs) = rotulosRamas2 x ++ rotulosRamas3 xs
 

contarHijos :: ArbolNRot a b -> Int
contarHijos (Hoja a) = 0
contarHijos (Nodo a b []) = 0
contarHijos (Nodo a b (x:xs)) = 1 + contarHijos (Nodo a b xs)

-- sumaHijos: suma los hijos de n que son m hijos.
sumaHijos :: Int -> [Int] -> Int -> Int
sumaHijos n [] m = 0
sumaHijos n (x:xs) m = if n == x then sumaHijos2 xs m else sumaHijos n xs m

sumaHijos2 :: [Int] -> Int -> Int
sumaHijos2 (x:xs) 0 = 0
sumaHijos2 (x:xs) m = x + sumaHijos2 xs (m-1)

-- obtenerHijos :: ArbolNRot a b -> ArbolNRot a b
-- obtenerHijos (nodo m n (x: xs)) : Nodo 
-- sumaHijos n (obtenerRotulosRama (Nodo m n (x:xs))) (contarHijos (Nodo m n (x:xs)))

paresMonada :: Int -> [(Int, Int)]
paresMonada n = enumFromTo 1 (div n 2) >>= (\x -> [(x,n-x)])


foldrANRArcos :: (b -> b -> b) -> b -> ArbolNRot a b -> b
foldrANRArcos f x (Hoja m) = x
foldrANRArcos f x (Nodo m n (xs)) = (f n (foldrANRArcos2 f x xs))

foldrANRArcos2 :: (b -> b -> b) -> b -> [ArbolNRot a b] -> b
foldrANRArcos2 f x [] = x
foldrANRArcos2 f x (y:ys) = f (foldrANRArcos f x y) (foldrANRArcos2 f x ys)

foldrANRNodos :: (a -> a -> a) -> a -> ArbolNRot a b -> a
foldrANRNodos f x (Hoja m) = x
foldrANRNodos f x (Nodo m n (xs)) = (f m (foldrANRNodos2 f x xs))

foldrANRNodos2 :: (a -> a -> a) -> a -> [ArbolNRot a b] -> a
foldrANRNodos2 f x [] = x
foldrANRNodos2 f x (y:ys) = f (foldrANRNodos f x y) (foldrANRNodos2 f x ys)

rotulosRamas2 :: ArbolNRot a Int -> [Int]
rotulosRamas2 (Hoja m) = []
rotulosRamas2 (Nodo m n (x:xs)) = map (+ n) ((contarRotulos x):(contarRotulos2 xs))

contarRotulos :: ArbolNRot a Int -> Int
calcularRotuloRama x = foldrANRArcos (+) 0 x

contarRotulos2 :: [ArbolNRot a Int] -> [Int]
calcularRotuloRamas [] = []
calcularRotuloRamas (x:xs) = ((calcularRotuloRama x):(calcularRotuloRamas xs))

-- foldrANRArcos (+) 0 (Nodo ("j") (4) [Hoja "p", Hoja "m"]])


