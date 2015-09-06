import Data.Char



pendiente :: (Float,Float) -> (Float,Float) -> Float

pendiente p1 p2 = (snd p2 - snd p1)/ (fst p2 - fst p1)



iniciales :: String -> String -> String

iniciales no ap = [head no, '.', head ap,'.']
