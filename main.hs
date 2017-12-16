import Data.Map (Map)

data Zdanie = Z Char | N Zdanie | C Zdanie Zdanie | A Zdanie Zdanie | K Zdanie Zdanie

drukuj :: Zdanie -> [Char]
-- drukuj dopasuje sie do formatu zdania (dekonstrukcja typu), wystarczy obsluzyc podstawowe przypadki
drukuj (Z znak) = [znak]
drukuj (N zdanie) = "~" ++ (drukuj zdanie)
drukuj (C zdanie1 zdanie2) = "(" ++ (drukuj zdanie1) ++ " => " ++ (drukuj zdanie2) ++ ")"
drukuj (A zdanie1 zdanie2) = "(" ++ (drukuj zdanie1) ++ " | " ++ (drukuj zdanie2) ++ ")"
drukuj (K zdanie1 zdanie2) = "(" ++ (drukuj zdanie1) ++ " & " ++ (drukuj zdanie2) ++ ")"

zmienne :: Zdanie ->[Char] 
zmienne (Z znak)  =[znak]
zmienne  (N zdanie) = zmienne zdanie 
zmienne  (C zdanie1 zdanie2)  = (zmienne zdanie1 )++ (zmienne zdanie2 )
zmienne  (A zdanie1 zdanie2)  = (zmienne zdanie1 )++ (zmienne zdanie2 )
zmienne  (K zdanie1 zdanie2) = (zmienne zdanie1 )++ (zmienne zdanie2 )

usun_duplikaty :: [Char] ->[Char]
usun_duplikaty [] = []
usun_duplikaty (a:b) 
	|  czypowtorka b a = usun_duplikaty b 
	| otherwise =  a : usun_duplikaty b

czypowtorka :: [Char] -> Char -> Bool
czypowtorka [] x= False
czypowtorka (a:b)  x
	|  a == x = True 
	|  otherwise = czypowtorka b x


dodaj_granice :: [Char] -> [Char]
dodaj_granice [] = []
dodaj_granice x = '[' : x ++ ']': []


wypisz_zmienne :: [Char] -> [Char]
wypisz_zmienne[]=[]
wypisz_zmienne (a:b)
	| a == '[' = a: wypisz_zmienne b
	| a == ']' = a: wypisz_zmienne b
	| b == [']'] = a: wypisz_zmienne b
	| otherwise = a :[','] ++ wypisz_zmienne b

sprawdz :: Zdanie -> Map Char Bool -> Bool
sprawdz z m = False

--Funkcja do zad4

testowe_zdanie = (C (N (Z 'p')) (A (K (Z 'p') (Z 'q')) (Z 'r')))
listka = []
main :: IO ()
main = do
putStrLn (drukuj testowe_zdanie)
putStrLn (wypisz_zmienne(dodaj_granice(usun_duplikaty(zmienne testowe_zdanie))))
