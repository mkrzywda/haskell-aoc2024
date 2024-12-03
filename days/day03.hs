import Data.List
import Data.List.Split
import System.IO
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Debug.Trace

main :: IO ()
main = do
    -- Odczyt danych testowych
    testInput <- readInput "../inputs/day03_test.txt"
    print $ part1 testInput
    print $ part2 ("do()" ++ testInput)

    -- Odczyt właściwych danych
    input <- readInput "../inputs/day03.txt"
    print $ part1 input
    print $ part2 ("do()" ++ input)

-- Funkcja odczytująca dane z pliku jako jeden ciąg tekstu
readInput :: FilePath -> IO String
readInput path = do
    content <- readFile path
    return (unlines (lines content))

-- Funkcja pomocnicza dopasowująca wyrażenie regularne
matchedOrNot :: Maybe [String] -> [String]
matchedOrNot Nothing = []
matchedOrNot (Just a) = a

-- Funkcja obliczająca wynik dla części 1
part1 :: String -> Int
part1 text = sum $ map ((\[a, b] -> a * b) . map read . splitOn "," . takeWhile (/= ')') . drop 4) matches
  where
    reg = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
    matches = getAllTextMatches (text =~ reg) :: [String]

-- Funkcja pomocnicza do analizy tekstu
takeWhileString :: String -> String -> String
takeWhileString _ [] = []
takeWhileString pattern rest@(a:ta) =
    if pattern `isPrefixOf` rest
        then []
        else a : takeWhileString pattern ta

-- Funkcja przetwarzająca tekst na listę fragmentów
parse :: String -> [String]
parse s
    | "do()" `isPrefixOf` s    = keep : parse rDo
    | "don't()" `isPrefixOf` s = parse rDont
    | "" == s                  = []
  where
    keep = takeWhileString "don't()" s
    rDo = drop (length keep) s
    keepDont = takeWhileString "do()" s
    rDont = drop (length keepDont) s

-- Funkcja obliczająca wynik dla części 2
part2 :: String -> Int
part2 = sum . map part1 . parse

