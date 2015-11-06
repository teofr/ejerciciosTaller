data RT t = Rose t [RT t] deriving Show

r1 = Rose 20 []
r2 = Rose 20 [Rose 33 [], Rose 17 [], Rose 34 [], Rose 33 []]
r3 = Rose 15 [Rose 16 [Rose 17 [], Rose 18 []], Rose 8 [Rose 3 []]]
r4 = Rose '1' [Rose 'a' [Rose '6' [Rose 't'[]], Rose '1' [], Rose '0' []], Rose 's' []]
r7 = Rose '1' [Rose 'a' [Rose 'b' [r4]]]


raiz :: RT t -> t
raiz (Rose a _) = a

hijos :: RT t -> [RT t]
hijos (Rose _ b) = b

sumarTodo :: Num t => RT t -> t
sumarTodo (Rose a []) = a
sumarTodo (Rose a (x:xs)) = sumarTodo (Rose a xs) + sumarTodo x

hojas :: RT t -> [t]
hojas (Rose a []) = [a]
hojas (Rose _ a) = hojasLista a

hojasLista :: [RT t] -> [t]
hojasLista [] = []
hojasLista (x:xs) = (hojas x) ++ (hojasLista xs)

espejar :: RT t -> RT t
espejar (Rose val hij) = (Rose val (reverse (espejarLista hij)))

espejarLista :: [RT t] -> [RT t]
espejarLista [] = []
espejarLista (x:xs) = (espejar x):(espejarLista xs)

altura ::  RT t -> Integer
altura (Rose _ []) =0
altura  (Rose _ a)= 1+ (maximum (alturaLista a))

alturaLista :: [RT t] -> [Integer]
alturaLista [] = []
alturaLista (x:xs) = (altura x):(alturaLista xs)

maximo :: Ord t => RT t -> t
maximo a = maximum $ listarValores a

listarValores :: RT t -> [t]
listarValores (Rose a hij) = [a]++(listarValoresList hij)

listarValoresList :: [RT t] -> [t]
listarValoresList [] = []
listarValoresList (x:xs)= (listarValores x) ++ (listarValoresList xs)

{-camino :: RT t -> [[t]]
camino (Rose val [])= [[val]]
camino (Rose val (x:[])) = [[val]++(camino x)]
camino (Rose val (x:xs)) = camino (Rose val [x]) ++ (camino (Rose val xs))-}

camino2 :: RT t -> [[t]]
camino2 (Rose val [])=[[val]]
camino2 (Rose val (x:xs))= (agregarValor val (camino2 x))++(camino2 (Rose val xs))

agregarValor :: t -> [[t]] -> [[t]]
agregarValor a [[]] = []
agregarValor a (x:xs)=(a:x):(agregarValor a xs)

camino3 :: RT t -> [[t]]
camino3 (Rose val []) = [[val]]
camino3 (Rose val hij)= agregarValor val (camino3Lis hij)

camino3Lis :: [RT t] -> [[t]]
camino3Lis []=[[]]
camino3Lis (x:xs)= (camino3 x)++(camino3Lis xs)
