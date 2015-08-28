doble x =x+x

cuadruple x = doble (doble x)

dist x1 y1 x2 y2 = sqrt( (x2-x1)**2 + (y2-y1)**2)



crearPar :: a->b->(a,b)
crearPar a b = (a,b)

invertir :: (a,b)->(b,a)
invertir p=(snd p, fst p)

distancia :: (Float,Float)->(Float,Float)->Float
distancia p1 p2 = sqrt((fst p2 - fst p1)**2 + (snd p2 -snd p1)**2)

raices :: Float->Float->Float->(Float,Float)
raices a b c = ((-b+sqrt(b**2-4*a*c))/(2*a), (-b-sqrt(b**2-4*a*c))/(2*a))
