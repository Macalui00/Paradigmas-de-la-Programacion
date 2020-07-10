--1 IMPORTANTE ESCRIBIR DATA Y TYPE EN MINUSCULA A LA HORA DE DEFINIRLAS.
data Poste = Origen | Destino | Auxiliar deriving (Eq,Show)
type Movimiento = (Poste,Poste)
hanoi :: Int -> [Movimiento]
hanoi x = hanoi2 x Origen Destino Auxiliar

hanoi2 :: Int -> Poste -> Poste -> Poste -> [Movimiento]
hanoi2 n o d a 
    | n == 0 = []
    | n > 0  = (hanoi2 (n-1) o a d) ++ [(o,d)] ++ (hanoi2 (n-1) a d o)

--2
data ExtensIxn a = Nada | Solo a  deriving (Eq, Show)

head :: [a] -> ExtensIxn a
head [] =  Nada
head (x:_) = Solo x

--3
data Grupo = A | B | AB | O deriving (Eq, Show)
data Factor = Positivo | Negativo deriving (Eq, Show)
data TipoDeSangre = Ts Grupo Factor deriving (Eq, Show)

donarG :: Grupo -> Grupo -> Bool
donarG O x = True
donarG A x = elem x [A, AB]
donarG B x = elem x [B, AB]
donarG AB x = x == AB

puedeDonarA :: TipoDeSangre -> TipoDeSangre  -> Bool
puedeDonarA (Ts g1 f1) (Ts g2 f2) = (f1 == f2) && (donarG g1 g2)

--4
data Nat = Cero | Suc Nat deriving (Eq, Show)

suma :: Nat -> Nat -> Nat
suma Cero x = x
suma (Suc x) y = Suc (suma x y)

--IMPORTANTES ACOTACIxNES:
-- SUMA SUC (X) Y = ... ESTA MAL LA FORMA CORRECTA ES: SUMA (SUC X) Y =...
-- Y FORMA CORRECTA DE ESCRIBIR LOS NATURALES:
--  Cero , Suc Cero, Suc (Suc Cero), Suc(Suc(Suc Cero))...
menor :: Nat -> Nat -> Bool
menor Cero (Suc x) = True
menor (Suc x) Cero = False
menor Cero Cero = False
menor (Suc x) (Suc y) = menor x y

menorigual :: Nat -> Nat -> Bool
menorigual Cero (Suc x) = True
menorigual (Suc x) Cero = False
menorigual Cero Cero = True
menorigual (Suc x) (Suc y) = menorigual x y

producto :: Nat -> Nat -> Nat
producto Cero x = Cero
producto x Cero = Cero
producto x (Suc Cero) = x 
producto (Suc x) (Suc y) = producto (Suc(Suc x)) y  

resta :: Nat -> Nat -> ExtensIxn Nat
resta x Cero = Solo x
resta Cero x = Solo Cero
resta (Suc x) (Suc y) = if (menor(Suc x) (Suc y)) then Nada else resta x y

resta2 :: Nat -> Nat -> Nat
resta2 x Cero = x
resta2 Cero x = Cero
resta2 (Suc x) (Suc y) = resta2 x y

nat2Int :: Nat -> Int
nat2Int Cero = 0
nat2Int (Suc x) = 1 + (nat2Int x)

--6
data ListOrd a = ListOrdVacia | ListOrdNoVacia a (ListOrd a) deriving (Eq, Ord, Show)
cabeza :: ListOrd a -> a 
cabeza (ListOrdNoVacia x xs) = x 

cola :: ListOrd a -> ListOrd a
cola (ListOrdNoVacia x xs) = xs

insertarOrd :: (Ord a, Eq a) => a -> ListOrd a -> ListOrd a
insertarOrd a ListOrdVacia = (ListOrdNoVacia a (ListOrdVacia))
insertarOrd a (ListOrdNoVacia b xs) = if (a == cabeza (ListOrdNoVacia b xs)) then
    (ListOrdNoVacia b xs) else if a < cabeza (ListOrdNoVacia b xs) then
        ListOrdNoVacia b (insertarOrd a xs) else ListOrdNoVacia a(ListOrdNoVacia b xs)

convertirALista :: ListOrd a -> [a]
convertirALista ListOrdVacia = []
convertirALista (ListOrdNoVacia x xs) = (x: (convertirALista xs))

data Pila a = Vacia | PilaNoVacia a (Pila a) deriving (Eq,Show)
crear :: [a] -> Pila a 
crear [] = Vacia
crear xs = (PilaNoVacia (last xs) (crear (init xs)))
--crear (x:xs) = (PilaNoVacia x (crear xs))

esVacia :: Pila a -> Bool
esVacia Vacia = True
esVacia (PilaNoVacia x xs) = False

cabezaPila :: Pila a -> a 
cabezaPila (PilaNoVacia x xs) = x

agregar :: a -> Pila a -> Pila a
--agregar a Vacia = (PilaNoVacia a Vacia)
agregar a  (xs) = (PilaNoVacia a xs)

sacarPila :: Pila a -> Pila a 
sacarPila (PilaNoVacia x xs) = xs

cantidadPila :: Pila a -> Int
cantidadPila Vacia = 0
cantidadPila (PilaNoVacia x xs) = 1 + cantidadPila xs

data Cola a = ColaVacia | ColaNoVacia a (Cola a) deriving (Eq, Show)

crearCola :: [a] -> Cola a 
crearCola [] = ColaVacia
crearCola (x:xs) =  (ColaNoVacia x (crearCola xs))

esVaciaCola :: Cola a -> Bool
esVaciaCola ColaVacia = True
esVaciaCola (ColaNoVacia x xs) = False

cabezaCola :: Cola a -> a
cabezaCola (ColaNoVacia x xs) = x

agregarCola :: a -> Cola a -> Cola a
agregarCola a ColaVacia = (ColaNoVacia a ColaVacia)
agregarCola a (ColaNoVacia x xs) = (ColaNoVacia x (agregarCola a xs) )

sacarCola :: Cola a -> a 
sacarCola (ColaNoVacia x xs) = x

cantidadCola :: Cola a -> Int
cantidadCola ColaVacia = 0
cantidadCola (ColaNoVacia x xs) = 1 + (cantidadCola xs)

--9 Arbol BinarIx Rotulado
data ArbBin a = ArbolVacIx | ArbolNoVacIx a (ArbBin a) (ArbBin a) deriving (Eq, Show)

--NroNodos
contarArbol :: ArbBin a -> Int
contarArbol (ArbolVacIx) = 0
contarArbol (ArbolNoVacIx x (xs) (ys)) = 1 + (contarArbol xs) + (contarArbol ys)

--crearArbBin :: [a] -> ArbBin a -> ArbBin a
--crearArbBin (x:xs) ArbolVacIx  = crearArbBin( xs (ArbolNoVacIx x (ArbolVacIx) (ArbolVacIx)))
--crearArbBin [] ArbolVacIx = ArbolVacIx
--crearArbBin (x:xs) (ArbolNoVacIx y (ys) (zs)) = if contarArbol (ys) > contarArbol (zs) then
  --  crearArbBin xs (ArbolNoVacIx y (ys) (agregarAArbBin x zs)) else crearArbBin xs (ArbolNoVacIx y (agregarAArbBin x ys) (zs))

agregarAArbBin :: a -> ArbBin a -> ArbBin a
agregarAArbBin a ArbolVacIx = ArbolNoVacIx a (ArbolVacIx) (ArbolVacIx)
agregarAArbBin a (ArbolNoVacIx x (xs) (ys)) = if contarArbol (xs) > contarArbol (ys) then
    (ArbolNoVacIx x (xs) (agregarAArbBin a ys)) else (ArbolNoVacIx x (agregarAArbBin a xs) (ys))

nroHojas :: ArbBin a -> Int
nroHojas (ArbolNoVacIx x (ArbolVacIx) (ArbolVacIx)) = 1
nroHojas ArbolVacIx = 0
nroHojas (ArbolNoVacIx x (xs) (ys)) = nroHojas xs + nroHojas ys

altura :: ArbBin a -> Int
altura ArbolVacIx = 0
altura (ArbolNoVacIx x (xs) (ys)) = if contarArbol (xs) > contarArbol (ys) then
    1 + altura(xs) else 1 + altura(ys)

--Parte 2

--Forma "Correcta" de escribir arboles binarIxs rotulados con hojas

data ArbBinRot a = Hoja a | ArbNoVacIx a (ArbBinRot a) (ArbBinRot a) deriving (Eq, Show)

nroNodos :: ArbBinRot a -> Int
nroNodos (Hoja a) = 1
nroNodos (ArbNoVacIx a (x) (y)) = 1 + nroNodos(x) + nroNodos(y)

--crearArbBinRot :: [a] -> ArbBinRot a
--crearArbBinRot (x:[]) = Hoja a
--crearArbBinRot (x:(y:(z:zs))) = ArbNoVacIx x (crearArbBinRot (y:z)) (crearArbBinRot zs)

--agregarArbBinRot :: a -> ArbBinRot a -> ArbBinRot a
--agregarArbBinRot a Hoja b = ArbNoVacIx b (Hoja a) ()

nroHojasBin :: ArbBinRot a -> Int
nroHojasBin (Hoja a) = 1
nroHojasBin (ArbNoVacIx x (y) (z)) = (nroHojasBin y) + (nroHojasBin z)

alturaBin :: ArbBinRot a -> Int
alturaBin (Hoja x) = 1
alturaBin (ArbNoVacIx x (xs) (ys)) = if nroNodos(xs) > nroNodos (ys) then 1 + alturaBin (xs) else 1 + alturaBin (ys)

--Preorden
preorden :: ArbBinRot a -> [a]
preorden (Hoja a) = [a]
preorden (ArbNoVacIx a (ys) (zs)) = (a: preorden ys) ++ (preorden zs)

--Postorden
postorden :: ArbBinRot a -> [a]
postorden (Hoja a) = [a]
postorden (ArbNoVacIx x (ys) (zs)) = postorden (ys) ++ postorden (zs) ++ [x]

--inorden
inorden :: ArbBinRot a -> [a]
inorden (Hoja a) = [a]
inorden (ArbNoVacIx x (ys) (zs)) = inorden (ys) ++ [x] ++ inorden (zs)

--Arboles de Misma estructura no importa rotulos
igEstructura :: ArbBinRot a -> ArbBinRot a -> Bool
igEstructura (Hoja a) (ArbNoVacIx x (ys) (zs)) = False
igEstructura (ArbNoVacIx x (ys) (zs)) (Hoja a) = False
igEstructura (Hoja a) (Hoja b) = True
igEstructura (ArbNoVacIx x (y)(z)) (ArbNoVacIx o (p) (w)) = (igEstructura (y) (p)) && (igEstructura (z) (w))


--ArbBinRotHoj: con rótulos sólo en las hojas.
data ArbBinRotHoj a = HojaRot (a) | ArbRotNoVacIx (ArbBinRotHoj a) (ArbBinRotHoj a) deriving (Eq, Show)

rotulos :: ArbBinRotHoj a -> [a]
rotulos (HojaRot (a)) = [a]
rotulos (ArbRotNoVacIx (xs) (ys)) = (rotulos xs) ++ (rotulos ys)

--ArbGen (árbol genérico), donde cada nodo puede tener una cantidad no fija (arbitraria) de hijos.
data ArbGen a = HojaGen a | ArbGenNoVacIx a [ArbGen a] deriving (Eq,Show)

--ii)
nroNodosGen :: ArbGen a -> Int
nroNodosGen (HojaGen a) = 1
nroNodosGen (ArbGenNoVacIx a []) = 1
nroNodosGen (ArbGenNoVacIx a (x:xs)) = nroNodosGen (ArbGenNoVacIx a xs) + nroNodosGen x

nroHojasGen :: ArbGen a -> Int
nroHojasGen (HojaGen a) = 1
nroHojasGen (ArbGenNoVacIx a []) = 0
nroHojasGen (ArbGenNoVacIx a (x:xs)) = (nroHojasGen x) + nroHojasGen (ArbGenNoVacIx a (xs))

buscarRamaLarga :: [ArbGen a] -> ArbGen a 
buscarRamaLarga (x: []) = x
buscarRamaLarga (x:xs) = if (nroNodosGen x > nroNodosGen (Prelude.head(xs))) then buscarRamaLarga (x:Prelude.tail(xs)) else buscarRamaLarga (xs)

alturaArbGen :: ArbGen a -> Int
alturaArbGen (HojaGen a) = 1
alturaArbGen (ArbGenNoVacIx a (x:xs)) = 1 + alturaArbGen (buscarRamaLarga (x:xs))

--alturaArbGen (ArbGenNoVacIx 2 [HojaGen 3, HojaGen 4])
--alturaArbGen (ArbGenNoVacIx 2 [HojaGen 3, HojaGen 4, (ArbGenNoVacIx 5 [HojaGen 6, HojaGen 7])])


--igualEstructura
igEstrucGen :: ArbGen a -> ArbGen a -> Bool
igEstrucGen (HojaGen a) (HojaGen b) = True
igEstrucGen (HojaGen a) (ArbGenNoVacIx b xs) = False
igEstrucGen (ArbGenNoVacIx b xs) (HojaGen a) = False
igEstrucGen (ArbGenNoVacIx a (x:xs)) (ArbGenNoVacIx b (y:ys)) = (igEstrucGen x y) && (listaArboles xs ys)

listaArboles :: [ArbGen a] -> [ArbGen a] -> Bool
listaArboles [] [] = True
listaArboles (x:_) [] = False
listaArboles [] (y:ys) = False
listaArboles (x:xs) (y:ys) = (igEstrucGen x y) && (listaArboles xs ys)

--igEstrucGen (ArbGenNoVacIx 2 [HojaGen 3, HojaGen 4, (ArbGenNoVacIx 5 [HojaGen 6, HojaGen 7])]) (ArbGenNoVacIx 1 [HojaGen 4, HojaGen 7, (ArbGenNoVacIx 8 [HojaGen 9, HojaGen 5])])
--igEstrucGen (ArbGenNoVacIx 2 [HojaGen 3, HojaGen 4, (ArbGenNoVacIx 5 [HojaGen 6, HojaGen 7])]) (ArbGenNoVacIx 1 [HojaGen 4, HojaGen 7])

--Conjunto:
data Conjunto a = ConjVacIx | ConjNovacIx a (Conjunto a) deriving (Eq, Show)

unIxn :: (Eq a) => Conjunto a -> Conjunto a -> Conjunto a 
unIxn (ConjVacIx) (x) = x
unIxn (x) (ConjVacIx) = x
unIxn (ConjNovacIx a x) (ConjNovacIx b y) = if (pertenece a (ConjNovacIx b y)) then (unIxn x (ConjNovacIx b y)) else (unIxn x (ConjNovacIx a (ConjNovacIx b y)))

pertenece :: (Eq a) => a -> Conjunto a -> Bool
pertenece (x) (ConjVacIx) = False
pertenece (x) (ConjNovacIx y ys) = if (x == y) then True else (pertenece x ys)

interseccIxn :: (Eq a) => Conjunto a -> Conjunto a -> Conjunto a 
interseccIxn (ConjVacIx) (x) = ConjVacIx
interseccIxn (x) (ConjVacIx) = ConjVacIx
interseccIxn (ConjNovacIx a (x)) (ConjNovacIx b (y)) = if ((pertenece a (ConjNovacIx b y)) && (pertenece b (ConjNovacIx a x))  && (a == b)) 
    then (ConjNovacIx a (interseccIxn (x) (y)))
    else 
        if ((pertenece a (ConjNovacIx b y)) && (pertenece b (ConjNovacIx a x))  && (a /= b)) 
            then (ConjNovacIx a (ConjNovacIx b (interseccIxn x y))) else
        if (pertenece a (ConjNovacIx b y)) then (ConjNovacIx a (interseccIxn x y)) else
            if (pertenece b (ConjNovacIx a x)) then (ConjNovacIx b (interseccIxn x y)) else (interseccIxn x y)

esVacIx :: Conjunto a -> Bool
esVacIx (ConjVacIx) = True
esVacIx (ConjNovacIx a x) = False

agElem :: (Eq a) => a -> Conjunto a -> Conjunto a 
agElem a x = if (pertenece a x) then x else (ConjNovacIx a x)

sacElem :: (Eq a) => a -> Conjunto a -> Conjunto a 
sacElem a (ConjVacIx) = ConjVacIx
sacElem a (ConjNovacIx b x) = if a == b then x else (ConjNovacIx b (sacElem a x))

-- ¿Qué problema ocurre con la estructura de este tipo?

--Definir el tipo Matriz de números con operacIxnes de suma, trasposición y producto.
data Matriz a = Mat Int Int [a] deriving (Eq, Show)

sumarListas :: (Num a) => [a] -> [a] -> [a]
sumarListas x [] = x
sumarListas [] y = y
sumarListas (x:xs) (y:ys) = (x+y : sumarListas xs ys)

sumaMat :: (Num a) => Matriz a -> Matriz a -> Matriz a 
sumaMat (Mat f1 c1 x) (Mat f2 c2 y) = if (f1 == f2) && (c1 == c2) then (Mat f1 c1 (sumarListas x y)) else error "No se pueden sumar matrices"

--transposicIon :: Matriz a -> Matriz a


--producto :: Matriz a -> Matriz a -> Matriz a


--Definir el tipo Secuencia de números con las siguientes funcIxnes asociadas:

data Secuencia a = Sec [a] (Int) deriving (Eq, Show)

--crearSecVac :	Crea una secuencia vacía de elementos.
crearSecVacia :: Secuencia a
crearSecVacia = (Sec [] (-1))
 
--regAct :	Dado un número de registro de una secuencia (según su orden de aparición) lo pone activo. Si ese número de registro no existe, debe devolver error.
regAct :: Int -> Secuencia a -> Secuencia a
regAct n (Sec [] (-1)) = error "Secuencia Vacia"
regAct n (Sec x m) = if (n > (length x)) then error "Numero de registro inexistente" else (Sec x n)


--leerReg :	Lee el registro activo (si existe) y activa el siguiente registro en el orden. Si antes ya se había leído el último registro, se devolverá una señal
-- de fin de secuencia y desactiva el registro activo si existía. Si no existía registro activo, se devolverá una señal de error.
obtenerReg :: Secuencia a -> a
obtenerReg (Sec (x:_) 0) = x
obtenerReg (Sec (x:xs) n) = obtenerReg (Sec xs (n-1))

leerReg :: Secuencia a -> a
leerReg (Sec x n) = if n == -1 then error "No hay registro activo" else if (n > (length x)) then  error "Fin de secuencia" else obtenerReg (Sec x n)

--borrReg :	Borra el registro activo y no deja activo ningún registro.
borrarReg :: Secuencia a -> Secuencia a
borrarReg (Sec (xs) (-1)) = error "No hay registro activo"
borrarReg (Sec (xs) n) = (Sec (borrElemLista xs n) (-1))

borrElemLista :: [a] -> Int ->[a]
borrElemLista (x:xs) 0 = xs
borrElemLista (x:xs) n = (x: (borrElemLista xs (n-1)))

--agReg :	Agrega un registro al final de la secuencia. Si había un registro activo, éste se desactiva.
agReg :: a -> Secuencia a -> Secuencia a 
agReg a (Sec (xs) n) = (Sec (agregFinal a xs) (-1))

agregFinal :: a -> [a] -> [a]
agregFinal x [] = (x: [])
agregFinal x (y:ys) = (y: agregFinal x ys)

--numAct :	Devuelve el número de registro activo. Si no existe, deberá devolver una señal de error.
numAct :: Secuencia a -> Int
numAct (Sec xs n) = if n < 0 then error "No hay registro activo" else n

--ConjInf (conjuntos posiblemente infinitos) ??

--GNO (grafo no orientado) con sus constructores.
data GNO a = G [a] [(a,a)] deriving (Eq, Show)

crearGrafo :: [a] -> [(a,a)] -> GNO a
crearGrafo [] [] = (G [] [])
crearGrafo xs ys = (G xs ys)

--que retorne de las relacIxnes de un nodo
retornarRel :: (Num a, Eq a)=> a -> GNO a -> [(a,a)]
retornarRel n (G xs []) = []
retornarRel n (G xs (y:ys)) = if (coincide n y) then (y: (retornarRel n (G xs ys))) else (retornarRel n (G xs ys))

coincide :: (Num a, Eq a)=> a -> (a,a) -> Bool
coincide n (m,o) = (m == n) || (o == n)

pertenGrafo :: (Num a, Eq a) => a -> GNO a-> Bool
pertenGrafo n (G [] ys) = False
pertenGrafo n (G (x:xs) ys) = if n == x then True else (pertenGrafo n (G xs ys))

existeCamino :: (Num a, Eq a) => a -> a -> GNO a  -> Bool
existeCamino n m (G xs []) = False
existeCamino n m (G xs ((p,q):ys)) = if n == p then if m == q then True else (existeCamino q m (G xs ys)) else (existeCamino n m (G xs ys))

--esArbol


--Elemento
data Elemento = E String [Int] deriving (Eq,Show)

cantValencias :: Elemento -> Int
cantValencias (E n []) = 0
cantValencias (E n (x:xs)) = 1 + (cantValencias (E n xs))

revisarValencias :: Elemento -> Bool
revisarValencias (E n []) = True
revisarValencias (E n (x:xs)) = if ((cantValencias (E n (x:xs))) > 1) then if (x == 0) then False else (revisarValencias (E n xs)) else True

--ii)	Definir la función esMolecula, que dada una lista de elementos (que pueden estar repetidos) devuelva si entre ellos pueden
--formar una molécula. El resultado será verdadero si ningún elemento es gas noble (tiene valencia cero) y además existe una combinación
--de valencias de cada uno de los elementos tal que su suma sea cero.

hayGasNoble:: [Elemento] -> Bool
hayGasNoble [] = False
hayGasNoble (x:xs) = if revisarValencias x then hayGasNoble xs else True

sumaValencias :: [Elemento] -> Int
sumaValencias [] = 0
sumaValencias (x:xs) = sumaValenciasElem x + sumaValencias xs

sumaValenciasElem :: Elemento -> Int
sumaValenciasElem (E n []) = 0
sumaValenciasElem (E n (x:xs)) = x + sumaValenciasElem (E n xs)

esMolecula :: [Elemento] -> Bool
esMolecula [] = error "Lista de elementos vacia"
esMolecula (x:xs) = if hayGasNoble (x:xs) then False else if (sumaValencias (x:xs) == 0) then True else False
--esMolecula [E "Oxigeno" [2,3], E "Oxigeno" [2,3], E "Hidrogeno" [-5,-5]]