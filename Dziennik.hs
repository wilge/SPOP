module Dziennik (
      dodajZadanie
    --, drukujDziennik
 --   , drukujDziennik2 -- todo
    --, uz
    , usunZadanie
    , oznaczJakoZrobione
    --, drukujZrobione
    --, drukujDzisiejsze
    --,wczytajZPliku
    , Dziennik(..)
    --, wypiszZadanie
) where
import Data.Time
import Data.Time.Calendar
import Zadanie
import System.IO
import System.Directory
import Data.List
import Data.Fixed
import Text.PrettyPrint


type Dziennik = [Zadanie]

dodajZadanie :: Zadanie -> Dziennik -> Dziennik
dodajZadanie z d = z : d

usunZadanie :: Int -> [Zadanie] -> Dziennik
usunZadanie _ [] = []
usunZadanie n (x:xs)
    | n == 1 = usunZadanie (n-1) xs
    | otherwise = x : usunZadanie (n-1) xs

oznaczJakoZrobione :: Int -> [Zadanie] -> Dziennik
oznaczJakoZrobione _ [] = []
oznaczJakoZrobione n (x:xs)
    | n == 1 = (zmienZrealizowano x True) : oznaczJakoZrobione (n-1) xs
    | otherwise = x : oznaczJakoZrobione (n-1) xs

