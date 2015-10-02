data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo  deriving Show



mdc :: Integer -> Integer -> Integer
mdc a b	|b==0 = a
		|otherwise = mdc b (snd (division a b))



division :: Integer -> Integer -> (Integer, Integer)
division a d  | 0<=a && a<d = (0,a)
              | 0<=a && 0<d = (fst (division (a-d) d) + 1,snd (division (a-d) d) )




esFinde :: Dia -> Bool
esFinde Sabado = True
esFinde Domingo = True
esFinde _ = False

diaHabil :: Dia -> Bool
diaHabil a = not(esFinde a)


soloAlgebra :: [Dia] -> [Dia]
soloAlgebra [] = []
soloAlgebra (Martes:xs) = Martes:(soloAlgebra xs)
soloAlgebra (Viernes:xs) = Viernes:(soloAlgebra xs)
soloAlgebra (_:xs) = soloAlgebra xs