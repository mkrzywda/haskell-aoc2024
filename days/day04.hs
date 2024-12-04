import Data.List (tails)
import Data.Maybe (fromMaybe)

readInput :: FilePath -> IO [String]
readInput path = lines <$> readFile path

countInLine :: String -> Int
countInLine line = countPattern line "XMAS" + countPattern line (reverse "XMAS")

countPattern :: String -> String -> Int
countPattern xs pattern
    | length xs < length pattern = 0
    | take (length pattern) xs == pattern = 1 + countPattern (tail xs) pattern
    | otherwise = countPattern (tail xs) pattern

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose xs = map head xs : transpose (map tail xs)

diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals xss = diagonalsUpper xss ++ diagonalsUpper (map reverse xss)
  where
    diagonalsUpper m = [[m !! (i + d) !! d | d <- [0 .. min (length m - 1 - i) (length (head m) - 1)]] | i <- [0 .. length m - 1]]
                     ++ [[m !! d !! (i + d) | d <- [0 .. min (length m - 1) (length (head m) - 1 - i)]] | i <- [1 .. length (head m) - 1]]

allLines :: [[Char]] -> [[Char]]
allLines grid =
    let rows = grid
        cols = transpose grid
        diags = diagonals grid
     in rows ++ cols ++ diags

countXMAS :: [[Char]] -> Int
countXMAS grid = sum $ map countInLine (allLines grid)

part1 :: [String] -> Int
part1 input = countXMAS input


getGridChar :: [[Char]] -> Int -> Int -> Maybe Char
getGridChar grid row col
    | row < 0 || row >= length grid = Nothing
    | col < 0 || col >= length (head grid) = Nothing
    | otherwise = Just ((grid !! row) !! col)

isMAS :: [Char] -> Bool
isMAS seq = seq == "MAS" || seq == "SAM"
countXMASPatterns :: [[Char]] -> Int
countXMASPatterns grid = length [ () 
    | row <- [0..rows-1]
    , col <- [0..cols-1]
    , (grid !! row) !! col == 'A'
    , let nwRow = row - 1
          nwCol = col - 1
          seRow = row + 1
          seCol = col + 1
          neRow = row - 1
          neCol = col + 1
          swRow = row + 1
          swCol = col - 1
          nwChar = getGridChar grid nwRow nwCol
          seChar = getGridChar grid seRow seCol
          neChar = getGridChar grid neRow neCol
          swChar = getGridChar grid swRow swCol
          seq1 = maybe "" (:[]) nwChar ++ "A" ++ maybe "" (:[]) seChar
          seq2 = maybe "" (:[]) neChar ++ "A" ++ maybe "" (:[]) swChar
      in isMAS seq1 && isMAS seq2
  ]
  where
    rows = length grid
    cols = if null grid then 0 else length (head grid)

part2 :: [String] -> Int
part2 input = countXMASPatterns input

main :: IO ()
main = do
    testInput <- readInput "../inputs/day04_test.txt"
    putStrLn "Test Input Results:"
    print $ part1 testInput
    print $ part2 testInput

    input <- readInput "../inputs/day04.txt"
    putStrLn "Real Input Results:"
    print $ part1 input
    print $ part2 input

