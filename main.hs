import Data.Map (Map)

data Zdanie = Z Char | N Zdanie | C Zdanie Zdanie | A Zdanie Zdanie | K Zdanie Zdanie

drukuj :: Zdanie -> [Char]
-- drukuj dopasuje sie do formatu zdania (dekonstrukcja typu), wystarczy obsluzyc podstawowe przypadki
drukuj (Z znak) = [znak]
drukuj (N zdanie) = "~" ++ (drukuj zdanie)
drukuj (C zdanie1 zdanie2) = "(" ++ (drukuj zdanie1) ++ " => " ++ (drukuj zdanie2) ++ ")"
drukuj (A zdanie1 zdanie2) = "(" ++ (drukuj zdanie1) ++ " | " ++ (drukuj zdanie2) ++ ")"
drukuj (K zdanie1 zdanie2) = "(" ++ (drukuj zdanie1) ++ " & " ++ (drukuj zdanie2) ++ ")"

usun_duplikaty :: [Char] -> [Char]
usun_duplikaty s = ""

wypisz_zmienne :: Zdanie -> [Char]
wypisz_zmienne z = ""

sprawdz :: Zdanie -> Map Char Bool -> Bool
sprawdz z m = False

--Funkcja do zad4

testowe_zdanie = (C (N (Z 'p')) (A (K (Z 'p') (Z 'q')) (Z 'r')))
main :: IO ()
main = putStrLn (drukuj testowe_zdanie)