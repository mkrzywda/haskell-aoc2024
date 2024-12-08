{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import Data.List (foldl', nub)
import qualified Data.Set as Set
import Data.Char (isAlphaNum)
import Control.Monad (guard)

type Position = (Int, Int)
type Frequency = Char
type Antenna = (Position, Frequency)
type Antinode = Position
type Line = (Int, Int, Int)

main :: IO ()
main = do
    args <- getArgs
    let testPath = if length args > 0 then args !! 0 else "../inputs/day07_test.txt"
    let realPath = if length args > 1 then args !! 1 else "../inputs/day07.txt"

    testInput <- readInput testPath
    putStrLn "Test Input Results:"
    putStrLn $ "Part 1: " ++ show (part1 testInput)
    putStrLn $ "Part 2: " ++ show (part2 testInput)

    input <- readInput realPath
    putStrLn "Real Input Results:"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

readInput :: FilePath -> IO [String]
readInput path = do
    contents <- readFile path
    return (lines contents)

part1 :: [String] -> Int
part1 grid =
    let antennas = extractAntennas grid
        grouped = groupByFrequency antennas
        gridWidth = maximum (map length grid)
        gridHeight = length grid
        antinodes = Set.fromList $ concatMap (computeAntinodesPart1 (gridWidth, gridHeight)) grouped
    in Set.size antinodes

part2 :: [String] -> Int
part2 grid =
    let antennas = extractAntennas grid
        grouped = groupByFrequency antennas
        gridWidth = maximum (map length grid)
        gridHeight = length grid
        antinodesPart2 = Set.fromList $ concatMap (computeAntinodesPart2 (gridWidth, gridHeight)) grouped
    in Set.size antinodesPart2

extractAntennas :: [String] -> [Antenna]
extractAntennas grid =
    [ ((x, y), c)
    | (y, row) <- zip [0..] grid
    , (x, c) <- zip [0..] row
    , isValidFrequency c
    ]

isValidFrequency :: Char -> Bool
isValidFrequency c = isAlphaNum c

groupByFrequency :: [Antenna] -> [(Frequency, [Position])]
groupByFrequency antennas =
    foldl' insertAntenna [] antennas
  where
    insertAntenna acc ((pos, freq)) =
        case lookup freq acc of
            Just positions -> (freq, pos : positions) : filter ((/= freq) . fst) acc
            Nothing        -> (freq, [pos]) : acc

computeAntinodesPart1 :: (Int, Int) -> (Frequency, [Position]) -> [Antinode]
computeAntinodesPart1 (width, height) (_, positions) =
    filter (isWithinBounds (width, height)) $
    concatMap antinodesForPair pairs
  where
    pairs = [(a, b) | a <- positions, b <- positions, a < b]
    antinodesForPair ((x1, y1), (x2, y2)) =
        [ (2 * x2 - x1, 2 * y2 - y1)
        , (2 * x1 - x2, 2 * y1 - y2)
        ]

computeAntinodesPart2 :: (Int, Int) -> (Frequency, [Position]) -> [Antinode]
computeAntinodesPart2 (width, height) (_, positions)
    | length positions < 2 = []
    | otherwise =
        let pairs = [(a, b) | a <- positions, b <- positions, a < b]
            lines = nub [normalizeLine x1 y1 x2 y2 | ((x1, y1), (x2, y2)) <- pairs]
            positionsOnLines = [ (x, y)
                               | (a, b, c) <- lines
                               , x <- [0..width -1]
                               , y <- [0..height -1]
                               , a * x + b * y + c == 0
                               ]
        in positionsOnLines

normalizeLine :: Int -> Int -> Int -> Int -> Line
normalizeLine x1 y1 x2 y2 =
    let a = y2 - y1
        b = x1 - x2
        c = x2 * y1 - x1 * y2
        gcdABC = gcd a (gcd b c)
        (a', b', c') = if gcdABC /= 0
                       then (a `div` gcdABC, b `div` gcdABC, c `div` gcdABC)
                       else (a, b, c)
        (aFinal, bFinal, cFinal) = if a' < 0 || (a' == 0 && b' < 0)
                                   then (-a', -b', -c')
                                   else (a', b', c')
    in (aFinal, bFinal, cFinal)

isWithinBounds :: (Int, Int) -> Position -> Bool
isWithinBounds (w, h) (x, y) = x >= 0 && x < w && y >= 0 && y < h

