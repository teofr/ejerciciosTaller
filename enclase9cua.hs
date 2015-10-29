data Arbol t = Hoja t | Ramif (Arbol t) t (Arbol t)
{-instance Show Arbol t where
  show (Hoja t) = t
  show-}

arb1= Ramif (Hoja 6) 20 (Hoja 2)

arb2 = Ramif (Ramif (Hoja 3) 4 arb1) 4 arb1

mostrarIzq :: Arbol t -> Integer






altura :: Arbol t -> Integer
altura (Hoja _) = 1
altura (Ramif a b c) = 1 + (max d e)
	where
		d = (altura a)
		e = (altura c)




graficador :: [String] -> String
graficador [] = ""
graficador (x:xs) = x ++ "\n\n" ++ (graficador xs)
