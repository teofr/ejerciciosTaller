data Polinomio t = Mono t Integer | Suma (Polinomio t) (Polinomio t) | Producto (Polinomio t) (Polinomio t)

pol1= Mono 2 3
pol2= Suma (Mono 4 1) pol1
pol3 = Producto pol2 pol1
pol4= Suma (Mono 1 1) (Suma (Mono 1 3) (Suma (Mono (-1) 1) (Mono 3 0)))
pol5 = Producto (Suma pol3 pol2) (pol3)

evaluar :: Num t => Polinomio t -> t -> t
evaluar (Mono a b) c = a*(c^b)
evaluar (Suma p1 p2) c = (evaluar p1 c)+(evaluar p2 c)
evaluar (Producto p1 p2) c = (evaluar p1 c)*(evaluar p2 c)


sumaListas :: Num t => [t] -> [t] -> [t]
sumaListas (x:[]) (y:ys) = (x+y):ys
sumaListas (x:xs) (y:[]) = (x+y):xs
sumaListas (x:xs) (y:ys) = (x+y):(sumaListas xs ys)

sumaListasInf :: Num t => [[t]] -> [t]
sumaListasInf (x:[]) =x
sumaListasInf (x1:x2:xs) = sumaListasInf ((sumaListas x1 x2):xs)

{-moverNEspacios :: [t] -> Integer -> [t]
moverNEspacios a 0=a
moverNEspacios a n= 0:(moverNEspacios a (n-1))-}

multiplicarLista :: Num t => [t] -> t -> [t]
multiplicarLista (x:[]) a = (a*x):[]
multiplicarLista (x:xs) a = (a*x):(multiplicarLista xs a)

armarLista :: Num t => [t] -> [t] -> [[t]]
armarLista (x:[]) b = (multiplicarLista b x):[]
armarLista (x:xs) b = (multiplicarLista b x):(armarLista xs (0:b))

coeficientes :: Num t => Polinomio t -> [t]
coeficientes (Mono co 0) = [co]
coeficientes (Mono co b) = 0:(coeficientes (Mono co (b-1)))
coeficientes (Suma p1 p2) = sumaListas (coeficientes p1) (coeficientes p2)
coeficientes (Producto p1 p2) = sumaListasInf (armarLista (coeficientes p1) (coeficientes p2))


{- buscar cor.to/89AY -}

{-derivar:: Num t => Polinomio t -> Polinomio t
derivar (Mono _ 0) = Mono (0 0)
derivar (Mono a b) = Mono (a*b) (b-1)
derivar (Suma p1 p2) = Suma (derivar p1) (derivar p2)

-}

instance Num t => Num (Polinomio t) where
	(+) p q = Suma p q
	(*) p q = Producto p q
	negate (Mono a b) = Mono (negate a) b
	fromInteger n = Mono (fromInteger n) 0
	abs p = undefined
	signum p =undefined

instance (Eq t, Num t, Show t) => Show (Polinomio t) where
	show p = show2 (coeficientes p) 0


show2 :: (Eq t,Num t,Show t) => [t] -> Integer -> String
show2 []  _ = ""
show2 (0:xs) a = show2 xs (a+1)
show2 (x:[]) a =(show x)++"*x^"++(show a)
show2 (x:xs) a = (show x)++"*x^"++(show a)++" + "++ (show2 xs (a+1))

x= Mono 1 1