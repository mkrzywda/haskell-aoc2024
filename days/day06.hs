import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad (guard)
import System.IO

data Direction = North | East | South | West deriving (Show, Eq, Ord)
type Position = (Int, Int)

data MapState = MapState {
    widthMap :: Int,
    heightMap :: Int,
    obstacles :: Set Position,
    guardPos :: Position,
    guardDir :: Direction
} deriving (Show)

main :: IO ()
main = do
    testInput <- readInput "../inputs/day06_test.txt"
    putStrLn "Test Input Results:"
    putStrLn $ "Part 1: " ++ show (part1 testInput)
    putStrLn $ "Part 2: " ++ show (part2 testInput)

    input <- readInput "../inputs/day06.txt"
    putStrLn "Real Input Results:"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

readInput :: FilePath -> IO [String]
readInput path = do
    content <- readFile path
    return (lines content)

parseMap :: [String] -> Maybe MapState
parseMap linesMap = do
    let h = length linesMap
    let w = if null linesMap then 0 else length (head linesMap)
    let obstacleSet = Set.fromList [ (x, y) | (y, line) <- zip [0..] linesMap
                                           , (x, c) <- zip [0..] line
                                           , c == '#']
    (gy, gx, dir) <- listToMaybe [ (y, x, directionFromChar c) 
                                 | (y, line) <- zip [0..] linesMap
                                 , (x, c) <- zip [0..] line
                                 , c `elem` ['^', 'v', '<', '>']]
    let guardPosition = (gx, gy)
    return $ MapState w h obstacleSet guardPosition dir

directionFromChar :: Char -> Direction
directionFromChar '^' = North
directionFromChar 'v' = South
directionFromChar '>' = East
directionFromChar '<' = West
directionFromChar _   = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

moveForward :: Position -> Direction -> Position
moveForward (x, y) North = (x, y - 1)
moveForward (x, y) East  = (x + 1, y)
moveForward (x, y) South = (x, y + 1)
moveForward (x, y) West  = (x - 1, y)

isObstacle :: Set Position -> Position -> Bool
isObstacle obs pos = pos `Set.member` obs

isWithinMap :: MapState -> Position -> Bool
isWithinMap mapState (x, y) =
    x >= 0 && x < widthMap mapState &&
    y >= 0 && y < heightMap mapState

simulateGuard :: MapState -> Set Position
simulateGuard mapState = simulate Set.empty (guardPos mapState) (guardDir mapState) (Set.singleton (guardPos mapState))
  where
    simulate :: Set (Position, Direction) -> Position -> Direction -> Set Position -> Set Position
    simulate visitedStates pos dir visited
        | not (isWithinMap mapState pos) = visited
        | otherwise =
            if (pos, dir) `Set.member` visitedStates
            then visited
            else
                let visitedStates' = Set.insert (pos, dir) visitedStates
                    visited' = Set.insert pos visited
                    nextPos = moveForward pos dir
                in if isWithinMap mapState nextPos && isObstacle (obstacles mapState) nextPos
                   then
                       simulate visitedStates' pos (turnRight dir) visited'
                   else
                       simulate visitedStates' nextPos dir visited'

part1 :: [String] -> Int
part1 input =
    case parseMap input of
        Nothing -> 0
        Just mapState -> Set.size (simulateGuard mapState)

simulateLoop :: MapState -> Bool
simulateLoop mapState = simulate Set.empty (guardPos mapState) (guardDir mapState)
  where
    simulate :: Set (Position, Direction) -> Position -> Direction -> Bool
    simulate visitedStates pos dir
        | not (isWithinMap mapState pos) = False
        | (pos, dir) `Set.member` visitedStates = True
        | otherwise =
            let visitedStates' = Set.insert (pos, dir) visitedStates
                nextPos = moveForward pos dir
            in if isWithinMap mapState nextPos && isObstacle (obstacles mapState) nextPos
               then
                   let newDir = turnRight dir
                   in simulate visitedStates' pos newDir
               else
                   simulate visitedStates' nextPos dir

causesLoop :: MapState -> Position -> Bool
causesLoop originalMapState obstructionPos =
    let newObstacles = Set.insert obstructionPos (obstacles originalMapState)
        newMapState = originalMapState { obstacles = newObstacles }
    in simulateLoop newMapState

part2 :: [String] -> Int
part2 input =
    case parseMap input of
        Nothing -> 0
        Just mapState ->
            let allPositions = [ (x, y) | y <- [0..heightMap mapState - 1],
                                         x <- [0..widthMap mapState - 1] ]
                potentialObstacles = filter (\pos -> not (isObstacle (obstacles mapState) pos) && pos /= guardPos mapState) allPositions
                loopPositions = filter (causesLoop mapState) potentialObstacles
            in length loopPositions

