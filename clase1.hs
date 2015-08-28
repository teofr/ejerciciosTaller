f n1 n2 n3   | n2 < 10 = n1
             | n2 >= 10 =n1+n3

nand True False = True
nand False True = True
nand False False = True
nand True True = False

nor True False = False
nor False True = False
nor False False = True
nor True True = False

cuadra a b c = (-b + sqrt(b**2-4*a*c))/(2*a)

esPitagorica a b c = a**2+b**2==c**2
