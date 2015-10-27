data Arbol t = Hoja t | Ramif (Arbol t) t (Arbol t)
{-instance Show Arbol t where
  show (Hoja t) = t
  show-}

arb1= Ramif (Hoja 10) 20 (Hoja 2)

arb2 = Ramif (Ramif (Hoja 3) 4 arb1) 4 arb1

showteo :: Show t => Arbol t -> String
showteo a = graficador (crearGrafCFilInd [""] a 0 1)

graficador :: [String] -> String
graficador [] = ""
graficador (x:xs) = x ++ "\n" ++ (graficador xs)

crearGrafCFilInd :: Show t => [String] -> Arbol t -> Integer -> Integer -> [String]
crearGrafCFilInd (x:xs) (Hoja a) inde fila  | (fromIntegral (length (x:xs))) == fila = agregarAFilaCInd (x:xs) (show a) fila inde
                                            | otherwise = crearFilaCInd (x:xs) inde (show a)
crearGrafCFilInd (x:xs) (Ramif a valor b) inde fila | (fromIntegral (length (x:xs))) == fila = crearGrafCFilInd (crearGrafCFilInd (agregarAFilaCInd (x:xs) (show valor) fila ((contarHojas a)+inde)) a inde (fila+1) ) b ((contarHojas a)+inde) (fila+1)
                                                    | otherwise = crearGrafCFilInd (crearGrafCFilInd (crearFilaCInd (x:xs) ((contarHojas a)+inde) (show valor) ) a inde (fila+1) ) b ((contarHojas a)+inde) (fila+1)


agregarAFilaCInd :: [String] -> String -> Integer -> Integer -> [String]
agregarAFilaCInd (x:xs) agregar 1 inde = (x++(nEspacios (inde-(fromIntegral (length x))))++agregar):xs
agregarAFilaCInd (x:xs) agregar fila inde = x: (agregarAFilaCInd xs agregar (fila -1) inde)

crearFilaCInd :: [String] -> Integer -> String -> [String]
crearFilaCInd graf inde agregar = graf ++ [((nEspacios inde) ++ agregar)]

contarHojas :: Arbol t -> Integer
contarHojas (Hoja _) = 1
contarHojas (Ramif a _ b) = (contarHojas a) + (contarHojas b)


nEspacios :: Integer -> String
nEspacios 1 = " "
nEspacios a = " "++(nEspacios (a-1))
