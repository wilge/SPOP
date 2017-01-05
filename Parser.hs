module Parser (
   joinQuoteTokens
   ,tokenize
   ,getCommand
   ,Token(..)
   --,dispatch
) where


import System.IO
import Data.Char
--import System.Exit
import Data.Time --todo
import Data.Time.Calendar
import Zadanie
import Dziennik

data Token = AddCom
            | DelCom
            | PrintAllCom
            | PrintDoneCom
            | PrintToday
            | MarkAsDone
            | HelpCom
            | BadCom
            | EndCom
            | ImportCom
            | ExportCom
            | Argument {arg :: String}
            | QuoteArg

    deriving(Show, Eq)




getCommand :: String -> IO String
getCommand prompt = do
    putStr prompt
    hFlush stdout
    getLine

argument :: String -> [Token]
argument [] = []
argument (t:text) = let (arg, text') = span (\x -> (isSpace x == False && not (x == '\"') )) text  in
                Argument (t:arg) : arguments text'

arguments :: String -> [Token]
arguments [] = []
arguments (t:text)
    | t == '\"' = QuoteArg : arguments text
    | isSpace t == False = argument (t:text)
    | isSpace t = arguments text
    | otherwise = []

command :: String -> [Token]
command [] = []
command (t:text) = let (com, text') = span isAlphaNum text in
                   command' (t:com) text'
         where
         command' com text
                   | com == "nowe"       || com == "n"  = AddCom : arguments text
                   | com == "wyswietl"   || com == "ww" = PrintAllCom : arguments text
                   | com == "zakonczone" || com == "wz" = PrintDoneCom : arguments text
                   | com == "dzis"       || com == "wd" = PrintToday : arguments text
                   | com == "usun"       || com == "u"  = DelCom : arguments text
                   | com == "zrobione"   || com == "z"  = MarkAsDone : arguments text
                   | com == "pomoc"      || com == "p"  = HelpCom : arguments text
                   | com == "import"     || com == "i"  = ImportCom : arguments text
                   | com == "eksport"    || com == "e"  = ExportCom : arguments text
                  -- | com == "koniec"     || com == "k"  = EndCom : arguments text
                   | otherwise                          = BadCom : arguments text
tokenize :: String -> [Token]
tokenize [] = []
tokenize (t : text)
    | isSpace t == False = command (t:text)
    | otherwise = []

argumentC (Argument x) = x
argumentC _ = []

joinQuoteTokens' (t:tokens) = let (tokens', rest) = span (/=QuoteArg) (t:tokens)
                              in
                                  case drop 1 rest of
                                       []     -> tokens'
                                       (x:xs) ->
                                            let (tokens'', rest') = span (/=QuoteArg) (drop 1 rest)
                                                tokens''' = (concat $ (map (\t -> argumentC t ++ " ") $ init tokens'')) ++ (argumentC $ last tokens'')
                                            in
                                                case drop 1 rest' of
                                                     [] -> tokens'++ [Argument tokens''']
                                                     _  -> tokens'++ [Argument tokens'''] ++ joinQuoteTokens (drop 1 rest')


joinQuoteTokens :: [Token] -> [Token]
joinQuoteTokens [] = []
joinQuoteTokens (t:tokens)
    | elem QuoteArg (t:tokens) = t:joinQuoteTokens' tokens
    | null (t:tokens) = []
    | otherwise = t:tokens




