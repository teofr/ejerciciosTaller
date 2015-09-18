potencia :: Float -> Integer -> Float
potencia _ 0 = 1
potencia a n = a* potencia a (n-1)



division :: Integer -> Integer -> (Integer, Integer)
division a d  | 0<=a && a<d = (0,a)
              | 0<=a && 0<d = (fst (division (a-d) d) + 1,snd (division (a-d) d) )



esDivisor :: Integer -> Integer -> Bool
esDivisor a d = snd (division a d) == 0


divParcial :: Integer -> Integer -> [Integer]
divParcial n m  | m==0 = []
                | esDivisor n m = [m]++ divParcial n (m-1)
                | otherwise = divParcial n (m-1)


divisores :: Integer -> [Integer]
divisores n = divParcial n n

esPrimo :: Integer -> Bool
esPrimo n = [n,1] == (divisores n)



produc :: [Integer] -> Integer
produc [] = 1
produc a = (head a) * produc (tail a)


reverso :: [a]->[a]
reverso [] = []
reverso b = (reverso (tail b)) ++ [(head b)]



capicua :: [Integer] -> Bool
capicua a = a==(reverso a)
