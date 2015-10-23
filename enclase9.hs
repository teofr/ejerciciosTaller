data Arbol = Hoja Integer | Ramificacion Arbol Integer Arbol
	
instance Show Arbol where
	show (Hoja a) = show a
	show (Ramificacion a b c)=(show b)++"====="++(show a)++"\n#\n#\n#======"++(show c)
data Dir = Der | Izq

arb1 :: Arbol
arb1 = Ramificacion (Ramificacion (Hoja 2) 3 (Hoja 3)) 4 (Hoja 4)  


esHoja :: Arbol -> Bool
esHoja (Hoja _) = True
esHoja _ = False

sumaNodos :: Arbol -> Integer
sumaNodos (Hoja a)= a
sumaNodos (Ramificacion b a c)= a + (sumaNodos b) + (sumaNodos c)

maxLis :: [Integer] -> Integer
maxLis [] = 0
maxLis (x:[]) = x
maxLis (x:y:[]) = max x y
maxLis (x:xs) = max x (maxLis xs)

altura :: Arbol -> Integer
altura (Hoja _) = 1
altura (Ramificacion a b c) = 1 + (max d e)
	where 
		d = (altura a)
		e = (altura c)


pertenece :: Integer -> Arbol -> Bool
pertenece a (Hoja c)= a==c
pertenece a (Ramificacion b c d) = (a==c) || (pertenece a b) || (pertenece a d)

valor :: Arbol -> Integer
valor (Hoja a) = a
valor (Ramificacion a b c) = b


busqueda :: [Dir] -> Arbol -> Integer
busqueda [] a = valor a
busqueda ((Der):xs) (Ramificacion i _ d) = busqueda xs d
busqueda ((Izq):xs) (Ramificacion i _ d) = busqueda xs i


espejo :: Arbol -> Arbol
espejo (Hoja a) = Hoja a
espejo (Ramificacion a b c) = Ramificacion (espejo c) b (espejo a) 