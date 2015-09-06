par :: Integer -> Bool
par 0 = True
par 1 = False
par n = par (abs n - 2)




dobleFact :: Integer -> Integer
dobleFact 0 = 1
dobleFact n | par n = n * dobleFact (n-2)
            | otherwise = undefined

combinatorio :: Integer -> Integer -> Integer
combinatorio n m  | n == m = 1
                  | m == 0 = 1
                  | otherwise = combinatorio (n -1) m + combinatorio (n-1) (m-1)


funcionBoba :: Integer -> Integer
funcionBoba 0 = 0
funcionBoba n = funcionBoba (n-1)



parteEntera :: Float -> Integer
parteEntera n | n < 1 = 0
              | otherwise = 1 + parteEntera (n-1)



imparN :: Integer -> Integer
imparN n = 2*n -1


sumaImpares :: Integer -> Integer
sumaImpares 0 = 0
sumaImpares n = imparN n + sumaImpares (n-1)


sumaImparesMenores :: Integer -> Integer
sumaImparesMenores n = sumaImpares (div ( parteEntera (sqrt (fromIntegral n)) + 1) 2)
