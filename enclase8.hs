data Figura = Rectangulo Float Float Float Float | Circulo Float Float Float deriving Show



c1 :: Figura
c1 = Circulo pi 0 0


r1 :: Float -> Figura
r1 x = Rectangulo 0 0 (x/( sqrt 2)) (x/( sqrt 2))

r2 :: Float -> Figura
r2 x = Rectangulo 0 0 (x*sin(pi/4)) (x*cos(pi/4))

area :: Figura -> Float
area (Rectangulo a b c d) = abs $ (c-a)*(d-b)
area (Circulo r a b)=pi*r**2


perimetro :: Figura -> Float
perimetro (Rectangulo a b c d) = 2*(c-a+d-b)
perimetro (Circulo r a b) =2*pi*r


encerrado :: Figura -> Figura
encerrado (Rectangulo a b c d) = Circulo (min (abs (dy/2)) (abs (dx/2))) ((a+c)/2) ((d+b)/2)
	where
		dy = d-b
		dx = c-a
encerrado (Circulo r a b) = Rectangulo (a-r*sin(pi/4)) (b-r*sin(pi/4)) (a+r*sin(pi/4)) (b+r*sin(pi/4))

encerrar :: Figura -> Figura
encerrar (Rectangulo a b c d) = Circulo (min (abs (dy/2)) (abs (dx/2))) ((a+c)/2) ((d+b)/2)

		where
		dy = d-b
		dx = c-a


data Punto = Point Float Float
data Figura2 = Rectangulo2 Punto Punto | Circulo2 Punto Float


area2 :: Figura2 -> Float
area2 (Rectangulo2 (Point a b) (Point c d)) = abs $ (c-a)*(d-b)
area2 (Circulo2 _ r) = pi*r**2


data ProgAritmetica = Vacio | CongruentesA Integer Integer
instance Show ProgAritmetica where
	show Vacio = "{}"
	show (CongruentesA a b) = "{a en Z / a = " ++ (show a) ++ " (mod "++(show b)++")}"
instance Eq ProgAritmetica where
	(==) a b = iguales a b



esMultiplo :: Integer -> Integer -> Bool
esMultiplo a b = 0 == (mod a b)

pertenece :: Integer -> ProgAritmetica -> Bool
pertenece _ Vacio = False
pertenece x (CongruentesA a b) = esMultiplo (x-a) b

incluido :: ProgAritmetica -> ProgAritmetica -> Bool
incluido (CongruentesA a b) (CongruentesA c d) = (esMultiplo b d) && (pertenece a (CongruentesA c d))
incluido Vacio _ = True
incluido _ Vacio = False


{-TAREAS-}

suma :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica
suma (CongruentesA a b) (CongruentesA c d) = CongruentesA (a+c) (gcd b d)
suma _ _ = Vacio

iguales :: ProgAritmetica -> ProgAritmetica -> Bool
iguales (CongruentesA a b) (CongruentesA c d) = (b==d) && (pertenece a (CongruentesA c d))
iguales Vacio Vacio = True
iguales _ _ = False

tieneSolucion :: Integer -> ProgAritmetica -> Bool
tieneSolucion t (CongruentesA a b) = esMultiplo a (gcd b t)
tieneSolucion _ Vacio = False

interseccion :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica
interseccion (CongruentesA a b) (CongruentesA c d)	| e<0 = Vacio
																										| otherwise = CongruentesA e (abs f-e)
	where
		e= primerPertenece 0 (lcm b d) (CongruentesA a b) (CongruentesA c d)
		f= primerPertenece (e+1) (2*(lcm b d)) (CongruentesA a b) (CongruentesA c d)
interseccion _ _ = Vacio

primerPertenece :: Integer -> Integer -> ProgAritmetica -> ProgAritmetica -> Integer
primerPertenece inicio fin a b	| (pertenece inicio a) && (pertenece inicio b) = inicio
																| inicio > fin = (-1)
																| otherwise = primerPertenece (inicio + 1) fin a b
