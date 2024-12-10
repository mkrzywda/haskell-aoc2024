{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Control.Monad.State
import Data.Char (digitToInt)

type Grid = Map.Map (Int, Int) Int
type Position = (Int, Int)
type Memo = Map.Map Position Int

main :: IO ()
main = do
    args <- getArgs
    let testPath = if length args > 0 then args !! 0 else "../inputs/day10_test.txt"
    let realPath = if length args > 1 then args !! 1 else "../inputs/day10.txt"

    testInput <- readInput testPath
    putStrLn "Test Input Results:"
    putStrLn $ "Part 1: " ++ show (part1 testInput)
    putStrLn $ "Part 2: " ++ show (part2 testInput)

    input <- readInput realPath
    putStrLn "Real Input Results:"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

-- Reads the input file and converts it into a Grid (Map of Position to Height)
readInput :: FilePath -> IO Grid
readInput path = do
    contents <- readFile path
    let linesOfFiles = lines contents
        gridList = [ ((row, col), digitToInt c)
                   | (row, line) <- zip [0..] linesOfFiles
                   , (col, c) <- zip [0..] line
                   ]
    return $ Map.fromList gridList

-- Part 1: Count the number of trailheads (positions with height 0)
part1 :: Grid -> Int
part1 grid = Map.size $ Map.filter (==0) grid

-- Part 2: Sum of the ratings of all trailheads
part2 :: Grid -> Int
part2 grid =
    let trailheads = Map.keys $ Map.filter (==0) grid
        -- Initialize memoization map
        initialMemo = Map.empty
        -- Compute number of paths for each trailhead with memoization
        (ratings, _) = runState (mapM (countPaths grid) trailheads) initialMemo
    in sum ratings

-- Count the number of distinct hiking trails from a given position to any height 9 with memoization
countPaths :: Grid -> Position -> State Memo Int
countPaths grid pos = do
    memo <- get
    case Map.lookup pos memo of
        Just n  -> return n
        Nothing -> do
            let currentHeight = Map.findWithDefault (-1) pos grid
            if currentHeight == 9
                then do
                    modify (Map.insert pos 1)
                    return 1
                else do
                    let neighbors = getNeighbors grid pos
                        validNext = [ p | p <- neighbors
                                        , let h = Map.findWithDefault (-1) p grid
                                        , h == currentHeight + 1 ]
                    pathCounts <- mapM (countPaths grid) validNext
                    let total = sum pathCounts
                    modify (Map.insert pos total)
                    return total

-- Get valid neighbors (up, down, left, right) within grid bounds
getNeighbors :: Grid -> Position -> [Position]
getNeighbors grid (r, c) =
    let possible = [ (r-1, c), (r+1, c), (r, c-1), (r, c+1) ]
    in filter (`Map.member` grid) possible

