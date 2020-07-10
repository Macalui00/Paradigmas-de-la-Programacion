import Data.Maybe
import Control.Monad.State
import Data.List
import Data.Char

-- Definir las funciones dividirPorDos y dividirPorTres, que dado un número entero, lo divide por dos o tres 
-- respectivamente. Si el número entero no es divisible por el número propio de la función,
-- deberá retornar un valor “vacío/inválido”.

dividirPorDos :: Int -> Maybe Int
dividirPorDos n = if mod n 2 == 0 then Just(div n 2) else Nothing

dividirPorTres :: Int -> Maybe Int
dividirPorTres n = if mod n 3 == 0 then Just(div n 3) else Nothing

-- Definir las funciones dividirPorDieciocho y dividirPorVeinticuatro a partir de las funciones del punto i).
-- Realizar las definiciones sin utilizar y utilizando la función (>>=).

dividirPorDieciocho :: Int -> Maybe Int

-- fromJust :: HasCallStack => Maybe a -> aSource#
-- The fromJust function extracts the element out of a Just and throws an error if its argument is Nothing.

dividirPorDieciocho n = if isNothing (dividirPorDos n) -- es divisible por 2
                        then Nothing
                        else if isNothing (dividirPorTres (fromJust(dividirPorDos n))) -- divisible por 2 y por 3 osea 3*2
                             then Nothing
                             else dividirPorTres (fromJust (dividirPorTres(fromJust (dividirPorDos n)))) -- divisible 2 veces por 3, 1 por 2 osea 3*3*2 = 18

-- A modo de resumen:

-- >>= pasa el resultado de la expresión de la izquierda como argumento a la expresión de la derecha, de una manera que
-- respete el contexto, el argumento y la función el uso

-- >> se usa para ordenar la evaluación de expresiones dentro de algún contexto; hace que la evaluación de la derecha
-- dependa de la evaluación de la izquierda

divPor18B :: Int -> Maybe Int
divPor18B x = dividirPorDos x >>= dividirPorTres >>= dividirPorTres

-- El resultado de dividirPorDos pasa a dividirPorTres y el resultado de este pasa de nuevo por dividirPorTres y obtengo el resultado que quiero.

-- 2*2*2*3 = 24
dividirPorVeinticuatro :: Int -> Maybe Int
dividirPorVeinticuatro n = if isNothing(dividirPorDos n)
                            then Nothing
                                else if isNothing(dividirPorDos (fromJust (dividirPorDos n)))
                                    then Nothing 
                                        else if isNothing(dividirPorDos (fromJust (dividirPorDos (fromJust(dividirPorDos n)))))
                                            then Nothing
                                                else dividirPorTres (fromJust (dividirPorDos (fromJust (dividirPorDos (fromJust(dividirPorDos n))))))

divPor24B :: Int -> Maybe Int
divPor24B x = dividirPorDos x >>= dividirPorDos >>= dividirPorDos >>= dividirPorTres

-- Idem punto ii), pero utilizando la notación do.
divPor24D :: Int -> Maybe Int
divPor18D :: Int -> Maybe Int
divPor18D x = do {
                   res <- dividirPorDos x;
                   res <- dividirPorTres res;
                   dividirPorTres res
                 }


divPor24D x = do {
                   res <- dividirPorDos x;
                   res <- dividirPorDos res;
                   res <- dividirPorDos res;
                   dividirPorTres res
                 }


-- Generar cuadradoMagico, que consiste en diversas combinaciones de 9 números
-- distintos entre 1 y 9 dispuestos en una matriz de 3x3 tales que la suma de todos los
-- valores en las filas, en las columnas y en las dos diagonales principales dé el
-- mismo valor para todos los casos. Usar listas por comprensión.
-- ii) Idem punto i) pero utilizando la notación do.

data MatrizCuadrada a = M Int [a] deriving Show
-- los elemento de la matriz son a, el Int es el índice de la matriz (cantidad de filas y columnas)

esMatrizVacia :: MatrizCuadrada a -> Bool
esMatrizVacia (M x ys) = null ys            -- funcion null verifica si una lista es vacia

-- Suma los elementos de la fila que se le indique
sumarFila :: Int -> MatrizCuadrada Int -> Int 
sumarFila n (M x xs) = if n > x || n <= 0 then error "índice inválido" else sumarFila2 n (M x xs) 

sumarFila2 :: Int -> MatrizCuadrada Int -> Int 
-- voy sacando una cantidad n de elementos de la lista y los voy sumando: Sum (take x xs)
-- y con drop saco una cantidad n de elementos
sumarFila2 n (M x xs) = if n == 1 then sum (take x xs) else sumarFila2 (n-1) (M x (drop x xs))

-- Suma los elementos de la fila que se le indique.
sumarColumna :: Int -> MatrizCuadrada Int -> Int
sumarColumna n (M x xs) = if n > x || n <= 0 then error "índice inválido" else sumarColumna2 n (M x xs)

-- !! obtener el elemento n–ésimo de una lista.
sumarColumna2 :: Int -> MatrizCuadrada Int -> Int
sumarColumna2 n (M x xs) = if null xs then 0 else (xs !! (n-1)) + (sumarColumna2 n (M x (drop x xs)))

-- sumarDiagonal :: Int -> MatrizCuadrada Int -> Int
-- sumarDiagonal n (M x xs) = if n > x || n <= 0 then error "índice inválido" else sumarDiagonal2 n (M x xs) 

sumarDiagonal :: MatrizCuadrada Int -> Int
sumarDiagonal (M x xs) = if null xs || x == 0 then 0 else (xs !! ((length xs)-1)) + (sumarDiagonal2 2 (M (x-1) (borrarUltimos x xs)))

sumarDiagonal2 :: Int -> MatrizCuadrada Int -> Int
sumarDiagonal2 n (M x xs) = if null xs || x == 0 then 0 else (xs !! ((length xs)-n)) + (sumarDiagonal2 (n+1) (M (x-1) (borrarUltimos x xs)))

borrarUltimos :: Int -> [a] -> [a]
borrarUltimos n xs = if n == 0 then xs else borrarUltimos (n-1) (init xs)

-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
-- [1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7]