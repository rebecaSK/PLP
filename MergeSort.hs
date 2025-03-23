merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort (x:xs) = merge (msort (fst (dividir xs))) (msort (snd (dividir  xs)))
            

dividir :: [a] -> ([a],[a])
dividir xs = splitAt mitad xs
            where mitad = div (length xs) 2


