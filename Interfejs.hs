module Interfejs (
      dispatch
    , pomoc
) where

import Dziennik
import Parser
import Zadanie
import System.IO
import Data.Char
import Data.Time
import Data.Time.Calendar
import Text.Regex.Posix
import Data.Fixed



pomoc = "Dostepne komendy:\n" ++
        "\t [pomoc|p]\t - wyswietla ten ekran\n" ++
   --     "\t koniec, k\t - wyjscie z programu\n" ++
        "\t [wyswietl|ww]\t - wyswietla wszystkie zadania\n" ++
        "\t [zakonczone|wz] - wyswietla zakonczone zadania\n" ++
        "\t [dzis|wd]\t - wyswietla zadania na dzis i przeterminowane\n" ++
        "\t [import|i]\t - wczytuje dane z pliku\n" ++
        "\t [eksport|e]\t - zapisuje dane do pliku\n" ++
        "\t [usun|u] [nr|\"wszystkie\"|\"zakonczone\"] \t - usuwa zadanie o numerze nr lub wszystkie lub zakonczone\n" ++
        "\t [zrobione|z] nr  - oznacza zadanie o numerze nr jako zrobione\n" ++
        "\t [nowe|n] nazwa data czas cyklicznosc\t - nowe zadanie\n" ++
        "\t [nowe|n] nazwa \"dzis\" cyklicznosc\t - nowe zadanie z dzisiejsza data\n" ++
        "\n\t Przyklady:" ++
        "\t nowe \"testowe zadanie 1\" 2014-01-12 22:22:00 b\n" ++
        "\t\t\t n test dzis r\n\n" ++
        "\t Cyklicznosc - mozliwe wartosci: b - brak, d - dzienne, t-tygodniowe, m-miesieczne, r-roczne\n"

naglowek =           uz 5  "Id"         <|>
                     uz 12 "Data"       <|>
                     uz 10 "Godzina"    <|>
                     uz 60 "Nazwa"      <|>
                     uz 7  "Zreal"      <|>
                     uz 12  "Powt"      ++ "\n"


drukujDziennik :: [Zadanie] -> String
drukujDziennik x = do
                    naglowek
                    ++ (replicate (length naglowek) '-') ++ "\n"
                    ++ (unlines $ zipWith (\f s -> (uz 5(show f)) <|> s :: String) [1..] (map row x))

               where
               row t =  foldl1 (<|>)[ ( uz 12 $ show $ dataz t        )
                                    , ( uz 10 $ take 8 $ show $ godz t )
                                    , ( uz 60 (nazwa t)                )
                                    , ( uz 7  (dz $ zrealizowano t)    )
                                    , ( uz 12 (dc $ powt t)            )
                                    ]
x <|>  y = x ++  "|" ++ y

uz n s = let xx = length s
             x = (fromIntegral (n-xx))/2.0
         in (replicate (ceiling x) ' ') ++ s ++ (replicate (floor x) ' ')


drukujZrobione :: [Zadanie] -> String
drukujZrobione dziennik = drukujDziennik (filter zrobione dziennik)

drukujDzisiejsze :: [Zadanie] -> Day-> String
drukujDzisiejsze dziennik d = "Zalegle zadania:"
                           ++ "\n"
                           ++ (drukujDziennik (filter (\t -> zalegle t d) dziennik))
                           ++ "Dzisiejsze zadania:"
                           ++ "\n"
                           ++ (drukujDziennik (filter (\t -> dzisiejsze t d) dziennik))
podzielStr:: String -> Char -> [String]
podzielStr "" _ = []
podzielStr str c = let
                      (str', rest) = span (/=c) str
                   in [str'] ++ (podzielStr (drop 1 rest) c)

parsujDate str = let [d1,d2,d3] = (podzielStr str '-')
                 in fromGregorian (read d1 :: Integer) (read d2 :: Int) (read d3 :: Int)

parsujCzas str = let [c1,c2,c3] =  (podzielStr str ':')
                 in TimeOfDay (read c1 :: Int) (read c2 :: Int) (read c3 :: Data.Fixed.Pico)

poprawnaData dat dzis = if dat =~ "^(19|20)[0-9]{2}[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$" then
                            if (parsujDate dat) < dzis then
                                False
                            else
                                True
                        else
                            False

poprawnyCzas czas = if czas =~ "^((([01]?[0-9]|2[0-3]):)?([0-5]?[0-9]):)?([0-5]?[0-9])$" then
                        True
                    else
                        False

poprawnyTyp typ =
                if typ =~ "^[bdtrm]$" then
                    True
                else
                    False

dodajZadanie' :: Token-> Token -> Token -> Token -> Dziennik -> Day-> Either  (IO String) (IO Dziennik)
dodajZadanie' a1 a2 a3 a4 d dzis
        | poprawnaData (arg a2) dzis == False = Left $ return "Zla data"
        | poprawnyCzas (arg a3) == False      = Left $ return "Zly czas"
        | poprawnyTyp  (arg a4) == False      = Left $ return "Zla cyklicznosc"
        | otherwise                           = Right$ return $ dodajZadanie (noweZadanie' (arg a1) (parsujDate $ arg a2) (parsujCzas $ arg a3) (arg a4)) d

dodajZadanie'' :: Token-> Day -> TimeOfDay -> Token -> Dziennik -> Either (IO String) (IO Dziennik)
dodajZadanie''  a1 a2 a3 a4 d
        | poprawnyTyp  (arg a4) == False      = Left $ return "Zla cyklicznosc"
        | otherwise                           = Right (return (dodajZadanie (noweZadanie' (arg a1) a2 a3 (arg a4)) d))

przeprowadzAkcje :: Token -> Dziennik -> (Int -> Dziennik -> Dziennik) -> Either (IO String) (IO Dziennik)
przeprowadzAkcje ar dziennik akcja
        | all isDigit (arg ar) == False = Left $ return "Niepoprawny parametr"
        | otherwise = let ar' = read $ arg ar :: Int
                          len = length dziennik
                      in
                          if (ar' <= len && ar'>0 ) then
                            Right $ return $ akcja ar' dziennik
                          else
                            Left $ return "Zly numer zadania"

wczytajZPliku :: String -> IO Dziennik
wczytajZPliku plik = do
                     handle <- openFile plik ReadMode
                     contents <- hGetContents handle  --wczytanie pliku
                     let zad = lines contents  --podzial na linies
                     let dz =  zwinZdarzenia zad []
                     putStrLn $ "Wczytano: " ++ (show $ length dz) ++ " zadania"
                     hClose handle
                     return dz

zapiszDoPliku' [] = []
zapiszDoPliku' (x:zadanie) = (drukujZadanie x):(zapiszDoPliku' zadanie)

zapiszDoPliku :: Dziennik -> String -> IO Dziennik
zapiszDoPliku dziennik plik = do
        handle <- openFile plik WriteMode
        --let todoTasks = drukujZadanie dziennik
        let dziennik' = unlines $ zapiszDoPliku' dziennik
        hPutStr handle dziennik'
        hClose handle
        return dziennik


zwinZdarzenia [] dz = dz
zwinZdarzenia (x:xs) dz = zwinZdarzenia xs ((zparsuj x):dz)


--2012-12-12 | 15:00:00 | test2 | z | d
zparsuj wpis = do
        let dat = head $ words wpis  -- rok
        let rok = read $ take 4 dat :: Integer
        let    miesiac = read $ take 2 $ drop 5 dat :: Int
        let    dzien = read $ take 2 $ drop 8 dat :: Int
        let    czas = last $ take 3 $ words wpis -- godzina
        let    godz = read $ take 2 czas ::Int
        let    minuty = read $ take 2 $ drop 3 czas ::Int
        let    sekundy = read $ take 2 $ drop 6 czas ::Data.Fixed.Pico
        let    nazwa = last $ take 5 $ words wpis -- nazwa
        let    z = last $ take 7 $ words wpis -- zrealizowano
               realizacja | z == "nie" = False
                          | otherwise  = True
        let    powt = last $ take 9 $ words wpis -- powtarzalnosc
               cyklicznosc | powt == "r" = Roczna
                           | powt == "m" = Miesieczna
                           | powt == "t" = Tygodniowa
                           | powt == "d" = Dzienna
                           | otherwise = Brak
        let    zad = Zadanie nazwa (fromGregorian rok miesiac dzien) (TimeOfDay godz minuty sekundy) cyklicznosc realizacja
        zad

blad1 = "Bledne polecenie. Aby uzyskac pomoc, wpisz pomoc lub p"
blad2 = "Zla liczba parametrow"

dispatch :: [Token] -> [Zadanie] -> (Day, TimeOfDay) -> (Either (IO String)  (IO Dziennik))
dispatch [AddCom,a1, a2,a3] p (dzis, czas)
         | (arg a2) == "dzis"                       = dodajZadanie'' a1 dzis czas a3 p
         | otherwise                                = Left  $ return blad1
dispatch [AddCom,a1,a2,a3,a4] p (dzis, _)           = dodajZadanie' a1 a2 a3 a4 p dzis
dispatch [ImportCom] p _                            = Right $ wczytajZPliku "dziennik.dat"
dispatch [ExportCom] p _                            = Right $ zapiszDoPliku p "dziennik.dat"
dispatch [DelCom, a] p _
         | (arg a) == "wszystkie"                   = Right $ return []
         | (arg a) == "zakonczone"                  = Right $ return $ filter (\t -> not $ zrobione t) p
         | otherwise                                = przeprowadzAkcje a p usunZadanie --usunZadanie' a p
dispatch [MarkAsDone, a] p _                        = przeprowadzAkcje a p oznaczJakoZrobione

dispatch (ImportCom:tokens) p _                     = Left $ return blad2
dispatch (ExportCom:tokens) p _                     = Left $ return blad2
dispatch (PrintAllCom:tokens) p _                   = Left $ return $ drukujDziennik p
dispatch (PrintDoneCom:tokens) p _                  = Left $ return $ drukujZrobione p
dispatch (PrintToday:tokens) p (dzis,_)             = Left $ return $ drukujDzisiejsze p dzis
dispatch (DelCom:_) p _                             = Left $ return blad2
dispatch (HelpCom:_) p _                            = Left $ return $ pomoc
dispatch (AddCom:_) p _                             = Left $ return blad2
dispatch _ p _                                      = Left $ return blad1

--dispatch (EndCom,_) p _                             = exitSuccess

