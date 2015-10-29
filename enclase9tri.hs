data Arbol t = Hoja t | Ramif (Arbol t) t (Arbol t)
{-instance Show Arbol t where
  show (Hoja t) = t
  show-}

arb1= Ramif (Hoja 'a') 'c' (Hoja 'd')

arb2 = Ramif (Ramif (Hoja 'K') 'h' arb1) 'r' arb1


showteo :: Show t => Arbol t -> String
showteo a = graficador (crearGrafico [""] a 1 0)

graficador :: [String] -> String
graficador [] = ""
graficador (x:xs) = x ++ "\n\n\n" ++ (graficador xs)


altura :: Arbol t -> Integer
altura (Hoja _) = 0
altura (Ramif a _ c) = 1 + (max d e)
	where
		d = (altura a)
		e = (altura c)


crearGrafico :: Show t => [String] -> Arbol t -> Integer -> Integer -> [String]
crearGrafico anterior (Hoja valor) fila inde = agregarValor anterior (show valor) fila inde
crearGrafico anterior (Ramif izq valor der) fila inde = crearGrafico (crearGrafico (agregarValor anterior (show valor) fila (indeexpo+inde)) izq (fila+1) (inde)) der (fila+1) (inde+indeexpo+2)
  where
    indeexpo = 2^(altura (Ramif izq valor der))
    indemediaexpo = 2^((altura (Ramif izq valor der))-1)
    indenueva = contarHojas izq
    indeentera =contarHojas (Ramif izq valor der)
    indemedia = (fromIntegral (div (contarHojas (Ramif izq valor der)) 2 ))




agregarValor :: [String] -> String -> Integer -> Integer -> [String]
agregarValor (x:xs) valor 1 inde = (x++(nEspacios (inde-(fromIntegral (length x))))++valor++"  "):xs
agregarValor (x:[]) valor fila inde = agregarValor (x:"":[]) valor fila inde
agregarValor (x:xs) valor fila inde = (x:(agregarValor xs valor (fila-1) inde))

nEspacios :: Integer -> String
nEspacios a | a>0 =(show a)++(nEspacios (a-1))
            |otherwise = ""


contarHojas :: Arbol t -> Integer
contarHojas (Hoja _) = 1
contarHojas (Ramif a _ b) = (contarHojas a) + (contarHojas b)
