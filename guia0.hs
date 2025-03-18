-- punto 2

valorAbsoluto :: Float -> Float
valorAbsoluto n | n > 0 = n
                | n < 0 = - n

valorAbsoluto1 :: Float -> Float
valorAbsoluto1 = abs

bisiesto :: Int -> Bool
bisiesto n = mod n 4 == 0

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length(divisoresPrimos n 1)

--funciones aux
divisoresPrimos :: Int -> Int -> [Int]
divisoresPrimos n div | div > n = []
                      | mod n div == 0 && esPrimo div = div : divisoresPrimos n (div+1)
                      | otherwise = divisoresPrimos n (div+1)

esPrimo :: Int -> Bool
esPrimo n | n<2 = False
          | otherwise = not(tieneDivisores n (n-1))
          where
            tieneDivisores _ 1 = False
            tieneDivisores num div | mod num div == 0 = True
                                   | otherwise = tieneDivisores num (div-1)
-- fin de funciones aux

-- punto 3
inverso :: Float -> Maybe Float 
inverso 0 = Nothing
inverso n = Just (1/n)

aEntero :: Either Int Bool -> Int
aEntero (Left n) = n
aEntero (Right n) | n  = 1
                  | not n = 0

--punto 4
limpiar :: String -> String -> String
limpiar [] q = q
limpiar (p:ps) q | p `notElem` q = limpiar ps q
                 | otherwise = limpiar ps (sacar p q) 
--funcion aux
sacar :: Char -> String -> String
sacar e [] = []
sacar e (p:ps) | e == p = sacar e ps  
               | otherwise = p : sacar e ps
--fin de aux

difPromedio :: [Float] -> [Float] 
difPromedio l = auxDifPromedio l (promedio l)

auxDifPromedio :: [Float] -> Float -> [Float]
auxDifPromedio [] _ = []
auxDifPromedio (p:ps) x = (p-x) : auxDifPromedio ps x

promedio :: [Float] -> Float
promedio [] = 0
promedio x = sum x / fromIntegral (length x) 

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [p] = True
todosIguales (p:ps) | p == head ps = todosIguales ps
                    | otherwise = False

--punto 5

data AB a = Nil | Bin (AB a) a (AB a) deriving (Show)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin hi r hd) = Bin (negacionAB hi) (not r) (negacionAB hd)
    
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin hi r hd) = productoAB hi * r * productoAB hd 
    
