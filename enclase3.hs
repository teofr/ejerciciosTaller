fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


par :: Integer -> Bool
par 0 = True
par 1 = False
par n = par (abs n - 2)



imparN :: Integer -> Integer
imparN n = 2*n -1


sumaImpares :: Integer -> Integer
sumaImpares 0 = 0
sumaImpares n = imparN n + sumaImpares (n-1)


multi3 :: Integer -> Bool
multi3 n 	| n==0 = True
			| abs (n)>2 = multi3 (abs (n)-3)
			| otherwise = False


multiploDeN :: Integer -> Integer -> Bool
multiploDeN n x	| x==0 = True
				| abs (x)>=abs (n) = multiploDeN (abs (n)) (abs (x)-abs (n))
				| otherwise = False




iniciales :: [Char] -> [Char]
iniciales a = sacarIn (words a)

sacarIn :: [String] -> [Char]
sacarIn [] = ""
sacarIn b = head (head b) : '.' sacarIn (tail b)