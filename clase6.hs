type Racional = (Integer, Integer)
type Conjunto = [Integer]




tuplas :: [a] -> [b] -> [(a,b)]
tuplas _ [] = []
tuplas [] _ = []
tuplas (x:xs) (y:ys) = (x,y):(tuplas xs ys)

potencia :: Racional -> Integer -> Racional
potencia (a,b) c=(a^c,b^c)

{-union :: Conjunto -> Conjunto -> Conjunto
union (x:xs) y = (union [x] y): (union xs y)
union (x:[]) y:ys	| x==y = union [] (y:ys)
					| otherwise = union [x] ys
union (x:[]) [] = x-}


{-EMPAQUETAR  CLASE 7-}

empaquetar :: [Integer] -> [[Integer]]
empaquetar []
empaquetar (x:[]) = [[x]]
empaquetar (x:xs)	| pertenece x (head (empaquetar xs)) = (x:head (empaquetar xs)):(tail (empaquetar xs))
					| otherwise = [x]:(empaquetar xs)

pertenece :: Integer -> [Integer] -> Bool
pertenece a [] = False
pertenece a (x:xs)	|a==x =True
					|otherwise = pertenece a xs


comprimir :: [[Integer]] -> [(Integer, Integer)]
comprimir [] = []
comprimir ((x:xs):ys) =((1+(fromIntegral $ length xs)),x):(comprimir ys)