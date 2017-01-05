module Main (
    main
) where
import Parser
import Dziennik
import Control.Monad
import System.Console.Readline
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Interfejs
import Control.Exception



mainLoop dziennik = do
		    line' <- getCommand ">"
	            currentTime <- getCurrentTime
		    let today = utctDay currentTime :: Day
		    let now   = utctDayTime currentTime :: DiffTime
	            let result = dispatch (joinQuoteTokens $ tokenize line') dziennik (today, timeToTimeOfDay now)

                    case result of
                        Left s ->  do
                                   s' <- s
                                   putStrLn s'
                                   mainLoop dziennik
                        Right d -> do
                                   dziennik' <- d
                                   putStrLn "Operacja wykonana prawidlowo"
                                   mainLoop dziennik'
                    putStrLn "koniec"


main = let dziennik = [] :: Dziennik
       in do
            putStrLn pomoc
            mainLoop dziennik

