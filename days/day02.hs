import System.IO
import Control.Exception (catch, IOException)

main :: IO ()
main = do

    testInput <- readFile "../inputs/day02_test.txt"
    let testParsed = parse (lines testInput)
    print (part1 testParsed)
    print (part2 testParsed)

    input <- readFile "../inputs/day02.txt"
    let parsed = parse (lines input)
    print (part1 parsed)
    print (part2 parsed)

allIncreasing :: [Int] -> Bool
allIncreasing xs = all (\(a, b) -> a > b && a < b + 4) (zip xs (tail xs))

checkRow :: [Int] -> Bool
checkRow xs = allIncreasing xs || allIncreasing (reverse xs)

rowGenerator :: [Int] -> [[Int]]
rowGenerator xs = [ take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1] ]

part1 :: [[Int]] -> Int
part1 input = length $ filter checkRow input

part2 :: [[Int]] -> Int
part2 input = length $ filter id [ any checkRow (rowGenerator row) | row <- input ]

parse :: [String] -> [[Int]]
parse = map (map read . words)

safeReadFile :: FilePath -> IO (Either String String)
safeReadFile path = (Right <$> readFile path) `catch` handleReadError
  where
    handleReadError :: IOException -> IO (Either String String)
    handleReadError e = return $ Left (show e)
