suma :: [Integer] -> [Integer] -> [Integer]
suma [] [] = []
suma a b = [(head a)+(head b)] ++ suma (tail a) (tail b)



prodInterno :: [Float] -> [Float] -> Float
prodInterno [] [] = 0
prodInterno a b = (head a)*(head b) + (prodInterno (tail a) (tail b))
