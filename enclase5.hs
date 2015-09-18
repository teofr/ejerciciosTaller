pertenece :: Integer -> [Integer] -> Bool
pertenece _ [] = False
pertenece a lista = (a== (head lista)) || (pertenece a (tail lista))

{-FORMA ORIGINAL, MAS FEA
pertenece a lista	|a == (head lista) = True
					|otherwise = pertenece a (tail lista)-}




hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos lista = (pertenece (head lista) (tail lista)) || (hayRepetidos (tail lista))

{-FORMA ORIGINAL, MAS FEA
hayRepetidos lista	| pertenece (head lista) (tail lista) = True
					| otherwise = hayRepetidos (tail lista)-}




menores :: Integer -> [Integer] -> [Integer]
menores _ [] = []
menores a lista	| (head lista) < a = (head lista) : menores a (tail lista)
				| otherwise = menores a (tail lista)



quitar :: Integer -> [Integer] -> [Integer]
quitar _ [] = []
quitar a lista	| a == (head lista) = tail lista
				| otherwise = (head lista) : (quitar a (tail lista))



maximo :: [Integer] -> Integer
maximo lista	| (tail lista)== (menores (head lista) (tail lista)) = head lista
				| otherwise = maximo (tail lista)


{-DEFINO DIVISON-}
division :: Integer -> Integer -> (Integer, Integer)
division a d  | 0<=a && a<d = (0,a)
              | 0<=a && 0<d = (fst (division (a-d) d) + 1,snd (division (a-d) d) )



enBase :: Integer -> Integer -> [Integer]
enBase num bas =calculador num bas (menorQue num bas 0)


calculador :: Integer -> Integer -> Integer -> [Integer]
calculador num bas pot	| pot == (-1) = []
						| otherwise =[fst (division num (bas^pot))]++ (calculador (snd (division num (bas^pot))) bas (pot-1))



menorQue :: Integer -> Integer -> Integer -> Integer
menorQue num base cont	| num < (base^cont) = cont-1
						|otherwise = menorQue num base (cont+1)



deBase :: Integer -> [Integer] -> Integer
deBase base lista = sumaBase base lista 0

sumaBase :: Integer -> [Integer] -> Integer -> Integer
sumaBase _ [] _ = 0
sumaBase base lista cont = (last lista)*(base^cont) + sumaBase base (init lista) (cont + 1)
