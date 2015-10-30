data Arbol t = Hoja t | Ramif (Arbol t) t (Arbol t)
{-instance Show Arbol t where
  show (Hoja t) = t
  show-}

arb1= Ramif (Hoja 2) 3 (Hoja 5)

arb2 = Ramif (Ramif (Hoja 6) 10 arb1) 34 arb1

arb3 = Ramif (Ramif arb1 43 arb2) 43 arb2

arb4 = Ramif (Ramif arb3 43 arb2) 43 (Ramif arb2 32 arb3)



alturaDe :: Arbol t -> Integer
alturaDe (Hoja _) = 0
alturaDe (Ramif a _ c) = 1 + (max d e)
	where
		d = (alturaDe a)
		e = (alturaDe c)



showteo :: Show t => Arbol t -> String
showteo a = graficador (crearGrafico [""] a 1 0 (alturaDe a))

graficador :: [String] -> String
graficador [] = ""
graficador (x:xs) = x ++ "\n\n\n" ++ (graficador xs)





crearGrafico :: Show t => [String] -> Arbol t -> Integer -> Integer -> Integer -> [String]
crearGrafico anterior (Hoja valor) fila inde altura = agregarValor anterior (show valor) fila (inde+indeexpo-1)
	where
		indeexpo = 2^(altura)
crearGrafico anterior (Ramif izq valor der) fila inde altura = crearGrafico (crearGrafico (agregarValor anterior (show valor) fila (indeexpo+inde-1)) izq (fila+1) (inde) (altura-1)) der (fila+1) (inde+indeexpo) (altura-1)
  where
    indeexpo = 2^(altura)

    indenueva = contarHojas izq
    indeentera =contarHojas (Ramif izq valor der)
    indemedia = (fromIntegral (div (contarHojas (Ramif izq valor der)) 2 ))




agregarValor :: [String] -> String -> Integer -> Integer -> [String]
agregarValor (x:xs) valor 1 inde = (x++(nEspacios (inde-(fromIntegral (length x))))++valor++"  "):xs
agregarValor (x:[]) valor fila inde = agregarValor (x:"":[]) valor fila inde
agregarValor (x:xs) valor fila inde = (x:(agregarValor xs valor (fila-1) inde))

nEspacios :: Integer -> String
nEspacios a | a>0 =" "++(nEspacios (a-1))
            |otherwise = ""


contarHojas :: Arbol t -> Integer
contarHojas (Hoja _) = 1
contarHojas (Ramif a _ b) = (contarHojas a) + (contarHojas b)
