{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)

-- Punkt wejściowy programu
main :: IO ()
main = do
    args <- getArgs
    let testPath = if length args > 0 then args !! 0 else "../inputs/day12_test.txt" 
    let realPath = if length args > 1 then args !! 1 else "../inputs/day12.txt" 

    testInput <- readInput testPath
    putStrLn "Test Input Results:"
    putStrLn $ "Total Fence Price: " ++ show (part1 testInput)

    input <- readInput realPath
    putStrLn "Real Input Results:"
    putStrLn $ "Total Fence Price: " ++ show (part1 input)

readInput :: FilePath -> IO [[Char]]
readInput path = do
    contents <- readFile path
    let grid = lines contents
    return grid

part1 :: [[Char]] -> Int
part1 grid = 
    let regions = findRegions grid
        fencePrices = map (fencePrice grid) regions
    in sum fencePrices

part2 :: [[Char]] -> Int
part2 _ = 0

findRegions :: [[Char]] -> [[(Int, Int)]]
findRegions grid = go Set.empty [] allPositions
  where
    rows = length grid
    cols = if null grid then 0 else length (head grid)
    allPositions = [(r, c) | r <- [0..rows-1], c <- [0..cols-1]]
    
    go :: Set (Int, Int) -> [[(Int, Int)]] -> [(Int, Int)] -> [[(Int, Int)]]
    go _ regions [] = regions
    go visited regions (p:ps)
        | Set.member p visited = go visited regions ps
        | otherwise = 
            let (region, newVisited) = floodFill grid p visited
            in go newVisited (region : regions) ps

floodFill :: [[Char]] -> (Int, Int) -> Set (Int, Int) -> ([(Int, Int)], Set (Int, Int))
floodFill grid (r, c) visited = floodFill' [ (r, c) ] Set.empty
  where
    plantType = (grid !! r) !! c
    
    floodFill' :: [(Int, Int)] -> Set (Int, Int) -> ([(Int, Int)], Set (Int, Int))
    floodFill' [] region = (Set.toList region, Set.union visited region)
    floodFill' (x:xs) region
        | Set.member x region = floodFill' xs region
        | (grid !! fst x !! snd x) /= plantType = floodFill' xs region
        | otherwise = 
            let neighbors = getNeighbors grid x
                sameTypeNeighbors = filter (\n -> (grid !! fst n !! snd n) == plantType) neighbors
                newRegion = Set.insert x region
                newPositions = sameTypeNeighbors ++ xs
            in floodFill' newPositions newRegion

getNeighbors :: [[Char]] -> (Int, Int) -> [(Int, Int)]
getNeighbors grid (r, c) = 
    let rows = length grid
        cols = if null grid then 0 else length (head grid)
        potential = [ (r-1, c), (r+1, c), (r, c-1), (r, c+1) ]
    in filter (\(r', c') -> r' >=0 && r' < rows && c' >=0 && c' < cols) potential

fencePrice :: [[Char]] -> [(Int, Int)] -> Int
fencePrice grid region = area * perimeter
  where
    area = length region
    regionSet = Set.fromList region
    perimeter = sum [ cellPerimeter grid regionSet cell | cell <- region ]

cellPerimeter grid regionSet (r, c) = 
    let neighbors = getNeighbors grid (r, c)
        sameRegionNeighbors = filter (`Set.member` regionSet) neighbors
        numShared = length sameRegionNeighbors
    in 4 - numShared

