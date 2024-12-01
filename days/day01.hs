import System.IO (readFile)
import Data.List (sort, group, sortOn, findIndex, isPrefixOf, tails)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import qualified Data.Map as Map

main :: IO ()
main = do
    testInput <- readInput "../inputs/day01_test.txt"
    print $ part1 testInput
    print $ part2 testInput
    
    input <- readInput "../inputs/day01.txt"
    print $ part1 input
    print $ part2 input

readInput :: FilePath -> IO [String]
readInput path = do
    content <- readFile path
    return (lines content)

parseLine :: String -> (Int, Int)
parseLine line =
    let parts = splitOnSpaces line
    in case parts of
        [a, b] -> (read a, read b)
        _      -> error $ "Niepoprawny format linii: " ++ line

splitOnSpaces :: String -> [String]
splitOnSpaces s = case dropWhile (== ' ') s of
    "" -> []
    s' -> w : splitOnSpaces s''
        where (w, s'') = breakOn "   " s'

breakOn :: String -> String -> (String, String)
breakOn sep s =
    case findIndex (isPrefixOf sep) (tails s) of
        Just idx -> (take idx s, drop idx s)
        Nothing  -> (s, "")

part1 :: [String] -> Int
part1 input =
    let inputElements = map parseLine input
        firstList = sort $ map fst inputElements
        secondList = sort $ map snd inputElements
    in sum $ zipWith (\a b -> abs (a - b)) firstList secondList

part2 :: [String] -> Int
part2 input =
    let inputElements = map parseLine input
        firstList = map fst inputElements
        occurrenceMap = Map.fromListWith (+) [(snd pair, 1) | pair <- inputElements]
        getCount x = Map.findWithDefault 0 x occurrenceMap
    in sum [a * getCount a | a <- firstList]

