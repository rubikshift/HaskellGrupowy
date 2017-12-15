data Zdanie = Z Char | N Zdanie | C Zdanie Zdanie | A Zdanie Zdanie | K Zdanie Zdanie

drukuj :: Zdanie -> [Char]
-- definicje drukuj dla roznych werjsi zdania, haskell dopasuje sie do ktoregos z nich
-- analogicznie trzeba zrobic dla nastepnych
drukuj (Z znak) = ""
drukuj (N zdanie) = ""
drukuj (C zdanie1 zdanie2) = ""
drukuj (A zdanie1 zdanie2) = ""
drukuj (K zdanie1 zdanie2) = ""

usun_duplikaty :: [Char] -> [Char]
usun_duplikaty s = ""

wypisz_zmienne :: Zdanie -> [Char]
wypisz_zmienne z = ""

sprawdz :: Zdanie -> [(Char, Bool)] -> Bool
sprawdz z m = False

--Funkcja do zad4
main :: IO ()

main = putStrLn "Hello!"