module Zadanie (
      Zadanie(..)
    , CyklicznoscZadania(..)
    , drukujZadanie
    , zmienZrealizowano
    , zrobione
    , dzisiejsze
    , zalegle
   --,castToInt
  --  , noweZadanie
    , noweZadanie'
    , dc
    , dz
) where
import Data.Time
import Data.Time.Calendar
import Data.Fixed

type NazwaZadania = String
data CyklicznoscZadania = Brak
                        | Dzienna
                        | Tygodniowa
                        | Miesieczna
                        | Roczna
                        deriving (Show, Eq)


data Zadanie = Zadanie {nazwa :: NazwaZadania 
                      , dataz :: Day
                      , godz  :: TimeOfDay
                      , powt  :: CyklicznoscZadania 
                      , zrealizowano :: Bool
                      } deriving (Eq,Show)

cyklicznosc :: String -> CyklicznoscZadania
cyklicznosc c
        | c == "b" = Brak
        | c == "d" = Dzienna
        | c == "t" = Tygodniowa
        | c == "m" = Miesieczna
        | c == "r" = Roczna
        | otherwise = Brak

dc :: CyklicznoscZadania -> String
dc Brak = " "
dc Dzienna = "dzienna"
dc Tygodniowa = "tygodniowa"
dc Miesieczna = "miesieczna"
dc Roczna = "roczna"

drukujZadanie :: Zadanie -> String
drukujZadanie (Zadanie nazwa dataz godz powt zrealizowano) = show dataz ++ " | " ++ (take 8 $ show godz) ++ " | " ++ nazwa ++ " | " ++ dz zrealizowano ++ " | " ++ dc powt

dz :: Bool -> String
dz False = "nie"
dz True  = "tak"

zmienZrealizowano :: Zadanie -> Bool -> Zadanie
zmienZrealizowano (Zadanie nazwa dataz godz powt zrealizowano) False = Zadanie nazwa dataz godz powt False
zmienZrealizowano (Zadanie nazwa dataz godz Brak zrealizowano) True = Zadanie nazwa dataz godz Brak True
zmienZrealizowano (Zadanie nazwa dataz godz powt zrealizowano) True = Zadanie nazwa (nowaData dataz powt) godz powt False

nowaData :: Day -> CyklicznoscZadania -> Day
nowaData d Brak       = d
nowaData d Dzienna    = addDays 1 d
nowaData d Tygodniowa = addDays 7 d
nowaData d Miesieczna = addGregorianMonthsClip 1 d
nowaData d Roczna     = addGregorianYearsClip 1 d

zrobione :: Zadanie -> Bool
zrobione (Zadanie nazwa dataz godz powt zrealizowano) = zrealizowano


dzisiejsze :: Zadanie -> Day -> Bool
dzisiejsze (Zadanie nazwa dataz godz powt zrealizowano) today = dataz == today && zrealizowano == False

zalegle :: Zadanie -> Day -> Bool
zalegle (Zadanie nazwa dataz godz powt zrealizowano) today = dataz < today && zrealizowano == False


noweZadanie' :: String -> Day -> TimeOfDay -> String -> Zadanie
noweZadanie' a1 a2 a3 a4 =  Zadanie a1 a2 a3 (cyklicznosc a4) False



