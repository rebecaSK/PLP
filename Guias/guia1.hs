-- punto 1

--max2 :: (Float, Float) -> Float
--max2 (x, y) | x >= y = x
--            | otherwise = y
-- Esta funcion no esta currificada ya que recibe dos argumentos de una vez y no, secuencialmente. 
-- Su version currificada es:

max2 :: Float -> Float -> Float
max2 x y | x > y = x
         | otherwise = y

--normaVectorial :: (Float, Float) -> Float 
--normaVectorial (x, y) = sqrt (x^2 + y^2)
-- Por la misma razon de antes, esta funcion tampoco esta currificada. Su version currificada es:

normaVectorial :: Float -> Float -> Float
normaVectorial x y = sqrt (x^2 + y^2)

subtract1 :: Float -> Float -> Float
subtract1 = flip (-)
-- funcion currificada

predecesor :: Float -> Float
predecesor = subtract1 1
-- funcion currificada

evaluarenCero :: (Float -> Float) -> Float
evaluarenCero = \f -> f 0
--funcion currificada

dosVeces :: (Float -> Float) -> (Float -> Float) 
dosVeces = \f -> f.f
--funcion currificada


flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip
--funcion currificada

flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip
--funcion currificada (notar que el primer flip intercambio los valores del segundo flip)

---------------------------------------------------------------------------------

--punto 2

curry1 :: a -> b -> ((a,b)-> c) -> c
curry1 x y f = f (x,y)

uncurry1 :: (a,b) -> (a -> b -> c) -> c
uncurry1 (x,y) f = f x y

-- El tipo de curryN no podria implementarse en haskell de forma estándar 
-- ya que no hay manera de definir el tipo de una funcion de aridad arbitraria

---------------------------------------------------------------------------------------

--punto 3

--I) Redefinir usando foldr las funciones sum, elem, (++), filter y map.

sum1 :: [Int] -> Int
sum1 = foldr (+) 0

elem1 :: Ord a => a -> [a] -> Bool
elem1 e = foldr(\x rec -> x == e || rec ) False

-- (++) = pp
pp :: [a] -> [a] -> [a]
pp = flip (foldr (:)) 

filter1 :: (a -> Bool) -> [a] -> [a] 
filter1 f = foldr (\x rec -> if f x then x : rec else rec) []

map1 :: (a -> b) -> [a] -> [b]
map1 f = foldr (\x rec -> f x : rec) [] 


--II) 

mejorSegún :: (a -> a -> Bool) -> [a] -> a
mejorSegún f = foldr1 (\x r -> if f x r then x else r) 

--III)
sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl(\acc x -> if null acc then [x] else acc ++ [last acc + x]) [] 

--IV)
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

sumaAlt2 :: Num a => [a] -> a
sumaAlt2 = foldl (flip (-)) 0

------------------------------------------------------------------

--punto 5

--elementosEnPosicionesPares :: [a] -> [a]
--elementosEnPosicionesPares [] = []
--elementosEnPosicionesPares (x:xs) = if null xs
--                                    then [x]
--                                    else x : elementosEnPosicionesPares (tail xs)

--La recursión de esta funcion es global, ya que accede a un
--resultado anterior: el de la recursión sobre la cola de la cola de la lista (es decir
--tail xs).


--entrelazar :: [a] -> [a] -> [a]
--entrelazar [] = id
--entrelazar (x:xs) = \ys ->  if null ys
--                            then x : entrelazar xs []
--                            else x : head ys : entrelazar xs (tail ys)

-- La recursion de esta funcion es primitiva, ya que trabaja con las subestructucturas
-- de los argumentos (head ys , tail ys). Sin embargo, no es global porque no opera 
-- con resultados de recursiones anteriores.

---------------------------------------------------------

--punto 6

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- caracteristicas de la recursion primitiva de recr:
-- recibe una funcion que toma la cabeza de la lista, la subestructura recursiva, el resutlado y recursivo
-- y finalmente devuelve el resultado del mismo tipo de la recursion.

-- Luego toma un elementp de tipo b que corresponde al caso base, una lista y finalmente devuelve el resultado de tipo b.

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs rec -> if x==e then xs else x:rec) []

-- implementacion con foldr:
--sacarUna :: Eq a => a -> [a] -> [a]
--sacarUna e = foldr (\x rec -> if x==e then rec else x:rec) []

-- Notar que foldr no es adecuado para implementar esta funcion ya que
-- no permite retornar xs directamente (lo que nos ayuda a cortar la recursion).
-- sino que devuelve rec, haciendo que el proceso se repita sin parar hasta llegar al
-- caso base. Quitando TODAS las apraciones del elemento indicado (que no es lo que queremos).

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado n = recr(\x xs rec -> if n<=x then n:(x:xs) else (if null xs then (x:xs)++[n] else x:rec)) []

-----------------------------------------------------------------------------------

--punto 7

--I) 
mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
--mapPares f = map (uncurry f)
mapPares f = foldr(\(x,y) rec -> f x y : rec) [] 

--II)
armarPares :: [a] -> [b] -> [(a,b)]
armarPares = foldr (\x rec ys -> if null ys then [] else (x,head ys) : rec (tail ys) ) (const []) 

-- Aprovecho la evaluacion parcial en rec.
-- Recordemos que el tipo de la de foldr es : a -> b -> b
-- Entonces al pasarle tres argumentos a la funcion, rec matchea con : b -> b, mas exactamente [b] -> [(a,b)]
-- Permitiendo asi, pasarle ys y que esta se consuma durante la recursion.
-- Dado que rec es una funcion, el caso base tambien debe serlo.


mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
--mapDoble f xs ys = map (uncurry f) (zip xs ys)  
mapDoble f = foldr(\x rec ys -> if null ys then [] else f x (head ys): rec(tail ys)) (const [])

------------------------------------------------------------------------------------------------

--punto 9

--I)
foldNat :: (Integer -> b -> b) -> b -> Integer -> b 
foldNat f z 0 = z
foldNat f z n = f n (foldNat f z (n-1) ) 

--II)
potencia :: Integer -> Integer -> Integer
potencia x = foldNat (\n rec -> x*rec)  1 

------------------------------------------------------------------------------------------------

--punto 12

data AB a = Nil | Bin (AB a) a (AB a)

--I)
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB fNil fAB ab = case ab of
                    Nil -> fNil
                    Bin izq r der -> fAB (foldAB fNil fAB izq) r (foldAB fNil fAB der) 

recAB :: b -> (b -> AB a -> a -> b -> AB a -> b) -> AB a -> b
recAB fNil fAB ab = case ab of
                    Nil -> fNil
                    Bin izq r der -> fAB (recAB fNil fAB izq) izq r (recAB fNil fAB der) der 

--II)
esNil :: AB a -> Bool
esNil ab = case ab of
            Nil -> True
            _ -> False

altura :: AB a -> Int
altura = foldAB (-1) (\recI r recD -> 1 + max recI recD)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\recI r recD -> 1 + recI + recD)

--III)
--asumo para este ejercicio que, en principio, el ab no es vacio
mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB f (Bin izq r der) = foldAB r (esMejor f) (Bin izq r der)

--funcion aux 
esMejor :: (a -> a -> Bool) -> a -> a -> a -> a
esMejor f recI r recD | f r recI = if f r recD then r else recD
                      | otherwise = if f recI recD then recI else recD  

--IV)
esABB :: Ord a => AB a -> Bool
esABB = recAB True (\rI izq r rD der -> comparar r (<) der && comparar r (>=) izq && rI && rD )  

--funcion aux
comparar :: a -> (a -> a -> Bool) -> AB a -> Bool
comparar r _ Nil = True
comparar r f (Bin _ r1 _) = f r r1

--V)
-- En todos los puntos, menos el IV, usé el equema de rec. estructural ya que no necesitaba acceder a la subestructura del AB
-- En cambio, para el IV, necesitaba comparar la raiz con la raiz de sus hijos (las subestructuras) y no solo con la recursion de ellos. 
-- Esto solo me lo permite hacer el esquemma de recursion primitiva

data RoseT a = Rose a [RoseT a]

foldRose :: (a -> [b] -> b) -> RoseT a -> b
foldRose f (Rose a xs) = f a (map (foldRose f) xs) 

hojas :: RoseT a -> [a]
hojas = foldRose (\r hijos -> if null hijos 
                                then [r] 
                                else concat hijos  )

-- cada elemento de la lista hijos es un resultado recursivo, por ello, si la clausula del if es falsa,
-- hijos tiene la forma de [[r0],[r1],...,[rn]]
-- cuando la recursion empieza a componer el resultado final (es decir, ya llegó al caso base y va a "hacia atras")
-- aplica concat al resultado recursivo para que siempre esté aplanado.

distancias :: RoseT a -> [Int]
distancias = foldRose (\r hijos -> if null hijos
                                    then [0]
                                    else map (+1) (concat hijos) )

alturaRT :: RoseT a -> Int
alturaRT = foldRose (\r hijos -> if null hijos
                                then 1 
                                else 1 + maximum hijos)
