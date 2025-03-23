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