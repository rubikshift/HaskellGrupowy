import Data.Maybe
import Data.List
import Data.Map 



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


formatuj :: [Char] -> [Char]
formatuj []=[]
formatuj  (a:b)
	| a == '[' = a: formatuj b
	| a == ']' = a: formatuj  b
	| b == [']'] = a: formatuj b
	| otherwise = a :[','] ++ formatuj  b

wypisz_zmienne :: Zdanie -> IO()
wypisz_zmienne z = putStrLn (formatuj(dodaj_granice(usun_duplikaty(zmienne z))))



sprawdz :: Zdanie -> Map Char Bool -> Bool
sprawdz (Z znak) m = findWithDefault False znak m
sprawdz  (N zdanie) m=not(sprawdz zdanie m)
sprawdz  (C zdanie1 zdanie2) m = not(sprawdz zdanie1 m) || (sprawdz zdanie2 m)
sprawdz  (A zdanie1 zdanie2) m =(sprawdz zdanie1 m) || (sprawdz zdanie2 m)
sprawdz  (K zdanie1 zdanie2) m= (sprawdz zdanie1 m) && (sprawdz zdanie2 m)


--Funkcja do zad4

testowe_zdanie = (C (N (Z 'p')) (A (K (Z 'p') (Z 'q')) (Z 'r')))
mapa_wartosci =  fromList [('p', False), ('q', True), ('r', False)]

main :: IO ()
main = do
putStrLn (drukuj testowe_zdanie)
wypisz_zmienne testowe_zdanie
print(sprawdz testowe_zdanie mapa_wartosci)







