data Arbol t = Hoja t | Ramif (Arbol t) t (Arbol t)
{-instance Show Arbol t where
  show (Hoja t) = t
  show-}

arb1= Ramif (Hoja 10) 20 (Hoja 2)

arb2 = Ramif (Ramif (Hoja 3) 4 arb1) 4 arb1


showteo :: Show t => Arbol t -> String
showteo a = graficador (crearGrafico [""] a 1 0)

graficador :: [String] -> String
graficador [] = ""
graficador (x:xs) = x ++ "\n\n" ++ (graficador xs)


crearGrafico :: Show t => [String] -> Arbol t -> Integer -> Integer -> [String]
crearGrafico anterior (Hoja valor) fila inde = agregarValor anterior (show valor) fila inde
crearGrafico anterior (Ramif izq valor der) fila inde = crearGrafico (crearGrafico (agregarValor anterior (show valor) fila (indemedia+inde)) izq (fila+1) inde) der (fila+1) (inde+indemedia+2)
  where
    indenueva = contarHojas izq
    indemedia = (fromIntegral (div (contarHojas (Ramif izq valor der)) 2 )) +1




agregarValor :: [String] -> String -> Integer -> Integer -> [String]
agregarValor (x:xs) valor 1 inde = (x++(nEspacios (inde-(fromIntegral (length x))))++valor):xs
agregarValor (x:[]) valor fila inde = agregarValor (x:"":[]) valor fila inde
agregarValor (x:xs) valor fila inde = (x:(agregarValor xs valor (fila-1) inde))

nEspacios :: Integer -> String
nEspacios a | a>0 =" "++(nEspacios (a-1))
            |otherwise = ""


contarHojas :: Arbol t -> Integer
contarHojas (Hoja _) = 1
contarHojas (Ramif a _ b) = (contarHojas a) + (contarHojas b)
