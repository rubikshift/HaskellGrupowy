import Data.List
import qualified Data.Map as Map

data Zdanie = Z Char | N Zdanie | C Zdanie Zdanie | A Zdanie Zdanie | K Zdanie Zdanie

--zadanie1
drukuj :: Zdanie -> [Char]
-- drukuj dopasuje sie do formatu zdania (dekonstrukcja typu), wystarczy obsluzyc podstawowe przypadki
drukuj (Z znak) = [znak]
drukuj (N zdanie) = "~" ++ (drukuj zdanie)
drukuj (C zdanie1 zdanie2) = "(" ++ (drukuj zdanie1) ++ " => " ++ (drukuj zdanie2) ++ ")"
drukuj (A zdanie1 zdanie2) = "(" ++ (drukuj zdanie1) ++ " | " ++ (drukuj zdanie2) ++ ")"
drukuj (K zdanie1 zdanie2) = "(" ++ (drukuj zdanie1) ++ " & " ++ (drukuj zdanie2) ++ ")"

--zadanie2
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

-- zadanie3
sprawdz :: Zdanie -> Map.Map Char Bool -> Bool
sprawdz z m = False

-- zadanie4
--konwersja liczby na 'reprezentacje binarna' (tablica wartosci logicznych) => wygenerowanie n-tej kombinacji wartosci
zapisBinarny :: Int -> Int -> [Bool]
zapisBinarny 0 x = []
zapisBinarny d 0 = [False | i <- [1..d]]
zapisBinarny d x = do
    if k == 0 then False : (zapisBinarny n x) 
    else True : (zapisBinarny n (x - k*m))
    where 
        n = d - 1
        m = 2^n
        k = x `div` m

--wygenerowanie wszystkich mozliwych wartosci
generuj_wartosci :: [Char] -> [[(Char, Bool)]]
generuj_wartosci [] = []
generuj_wartosci z = [ zip z w | w <- (map (zapisBinarny n) [0 .. 2^n - 1])]
    where
        n = length(z)

--konwersja wygenerowanych wartosci na liste hashmap
stworz_mapy :: [[(Char, Bool)]] -> [Map.Map Char Bool]
stworz_mapy m = [Map.fromList x | x <- m]

--sprawdzenie zdania dla kazdej wygenerowanej mapy wartosci, nastepnie iloczyn logiczny wszystkich wartosci
jest_tautologia :: Zdanie -> Bool
jest_tautologia z = (foldr (&&) True [ sprawdz z x | x <- (stworz_mapy . generuj_wartosci . usun_duplikaty . zmienne) z ]) == True

testowe_zdanie = (C (N (Z 'p')) (A (K (Z 'p') (Z 'q')) (Z 'r')))

main :: IO ()
main = do
    putStrLn (drukuj testowe_zdanie)
    putStrLn (wypisz_zmienne(dodaj_granice(usun_duplikaty(zmienne testowe_zdanie))))
