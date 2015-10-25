data Arbol t = Hoja t | Ramif (Hoja t) t (Hoja t)
instance Show Arbol t where
  show (Hoja t) = t
  show


graficador :: [String] -> String
graficador [] = ""
graficador (x:xs) = x ++ "\n" ++ (graficador xs)

crearGrafCFilInd :: [String] -> Arbol -> Integer -> Integer -> [String]
crearGrafCFilInd (x:xs) (Ramif a valor b) inde fila | length 


agregarAFilaCInd :: [String] -> String -> Integer -> Integer -> [String]
agregarAFilaCInd (x:xs) agregar 0 inde = (x++(nEspacios (inde-(length x)))++agregar):xs
agregarAFilaCInd (x:xs) agregar fila inde = x: (agregarAFilaCInd xs agregar (fila -1) inde)

crearFilaCInd :: [String] -> Integer -> String -> [String]
crearFilaCInd graf inde agregar = graf ++ ((nEspacios inde) ++ agregar)

contarHojas :: Arbol -> Integer
contarHojas (Hoja _) = 1
contarHojas (Ramif a _ b) = (contarHojas a) + (contarHojas b)


nEspacios :: Integer -> String
nEspacios 1 = " "
nEspacios a = " "++(nEspacios (a-1))
