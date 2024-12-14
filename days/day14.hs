import System.Environment (getArgs)
import System.IO
import Data.List
import Data.Maybe
import Text.Read (readMaybe)
import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Robot = Robot {
    posX :: Int,
    posY :: Int,
    velX :: Int,
    velY :: Int
} deriving (Show, Eq)

parseRobot :: String -> Maybe Robot
parseRobot line =
    let parts = words line
        parseCoord s = case s of
            ('p':'=':rest) -> parsePair rest
            ('v':'=':rest) -> parsePair rest
            _ -> Nothing
        parsePair s =
            case break (==',') s of
                (a, ',':b) -> do
                    x <- readMaybe a
                    y <- readMaybe b
                    return (x, y)
                _ -> Nothing
        maybePos = parseCoord (head parts)
        maybeVel = parseCoord (parts !! 1)
    in do
        (x, y) <- maybePos
        (vx, vy) <- maybeVel
        return $ Robot x y vx vy

readInput :: FilePath -> IO [Robot]
readInput path = do
    contents <- readFile path
    let linesOfFile = lines contents
        robots = mapMaybe parseRobot linesOfFile
    return robots

computePosition :: Int -> Int -> Int -> Int -> Int
computePosition coord vel t maxCoord =
    let pos = coord + vel * t
        wrappedPos = ((pos `mod` maxCoord) + maxCoord) `mod` maxCoord
    in wrappedPos

simulate :: [Robot] -> Int -> Int -> Int -> [(Int, Int)]
simulate robots t width height =
    map (\r -> (computePosition (posX r) (velX r) t width,
                computePosition (posY r) (velY r) t height)) robots

countQuadrants :: [(Int, Int)] -> Int -> Int -> (Int, Int, Int, Int)
countQuadrants positions width height =
    let
        centerX = width `div` 2
        centerY = height `div` 2
        inQuadrant (x, y)
            | x < centerX && y < centerY = (1, 0, 0, 0)
            | x > centerX && y < centerY = (0, 1, 0, 0)
            | x < centerX && y > centerY = (0, 0, 1, 0)
            | x > centerX && y > centerY = (0, 0, 0, 1)
            | otherwise = (0, 0, 0, 0)
        counts = map inQuadrant positions
        (q1, q2, q3, q4) = foldl' (\(a, b, c, d) (w, x, y, z) -> (a + w, b + x, c + y, d + z)) (0, 0, 0, 0) counts
    in (q1, q2, q3, q4)

safetyFactor :: (Int, Int, Int, Int) -> Int
safetyFactor (q1, q2, q3, q4) = q1 * q2 * q3 * q4

boundingBoxArea :: [(Int, Int)] -> Int
boundingBoxArea positions = (maxX - minX) * (maxY - minY)
    where
        xs = map fst positions
        ys = map snd positions
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys

findEasterEggTime :: [Robot] -> Int -> Int -> Int
findEasterEggTime robots width height = go 0 initialArea 0
    where
        initialPos = simulate robots 0 width height
        initialArea = boundingBoxArea initialPos
        go :: Int -> Int -> Int -> Int
        go time minArea stability
            | stability >= 2 = time
            | otherwise =
                let nextTime = time + 1
                    nextPos = simulate robots nextTime width height
                    nextArea = boundingBoxArea nextPos
                in if nextArea < minArea
                    then go nextTime nextArea 0
                    else if nextArea == minArea
                        then go nextTime minArea (stability + 1)
                        else go nextTime minArea stability

part1 :: [Robot] -> Int -> Int -> Int -> Int
part1 robots t width height =
    let positions = simulate robots t width height
        quadrants = countQuadrants positions width height
    in safetyFactor quadrants

part2 :: [Robot] -> Int -> Int -> Int
part2 robots width height = findEasterEggTime robots width height

printPositions :: [(Int, Int)] -> IO ()
printPositions positions = do
    let minX = minimum (map fst positions)
        maxX = maximum (map fst positions)
        minY = minimum (map snd positions)
        maxY = maximum (map snd positions)
        grid = [ [ if (x, y) `elem` positions then '#' else '.' | x <- [minX..maxX] ] | y <- [minY..maxY] ]
    mapM_ putStrLn grid

main :: IO ()
main = do
    args <- getArgs
    let testPath = if length args > 0 then args !! 0 else "../inputs/day14_test.txt"
    let realPath = if length args > 1 then args !! 1 else "../inputs/day14.txt"

    let testWidth = 11
    let testHeight = 7
    let realWidth = 101
    let realHeight = 103

    testInput <- readInput testPath
    putStrLn "Test Input Results:"
    let testSafetyFactor = part1 testInput 100 testWidth testHeight
    putStrLn $ "Part 1 (Safety Factor after 100 seconds): " ++ show testSafetyFactor
    let testEasterEggTime = part2 testInput testWidth testHeight
    putStrLn $ "Part 2 (Easter Egg Time): " ++ show testEasterEggTime
    putStrLn $ "Positions at Easter Egg Time (" ++ show testEasterEggTime ++ " seconds):"
    let testEasterEggPositions = simulate testInput testEasterEggTime testWidth testHeight
    printPositions testEasterEggPositions

    input <- readInput realPath
    putStrLn "\nReal Input Results:"
    let realSafetyFactor = part1 input 100 realWidth realHeight
    putStrLn $ "Part 1 (Safety Factor after 100 seconds): " ++ show realSafetyFactor
    let realEasterEggTime = part2 input realWidth realHeight
    putStrLn $ "Part 2 (Easter Egg Time): " ++ show realEasterEggTime
    putStrLn $ "Positions at Easter Egg Time (" ++ show realEasterEggTime ++ " seconds):"
    let realEasterEggPositions = simulate input realEasterEggTime realWidth realHeight
    printPositions realEasterEggPositions
