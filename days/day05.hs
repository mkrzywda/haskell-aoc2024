import System.IO
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

type Rule = (Int, Int)
type Rules = [Rule]
type Update = [Int]
type Input = (Rules, [Update])

readInput :: FilePath -> IO Input
readInput filePath = do
    contents <- readFile filePath
    let ls = lines contents
        (ruleLines, updateLines) = break null ls
        rules = map parseRule ruleLines
        updates = map parseUpdate (filter (not . null) $ dropWhile null updateLines)
    return (rules, updates)

parseRule :: String -> Rule
parseRule s =
    let (x, yWithSep) = break (== '|') s
        y = tail yWithSep
    in (read x, read y)

parseUpdate :: String -> Update
parseUpdate s = map read $ splitComma s

splitComma :: String -> [String]
splitComma s = case break (== ',') s of
    (a, ',' : rest) -> a : splitComma rest
    (a, _)          -> [a]

isCorrect :: Rules -> Update -> Bool
isCorrect rules update =
    let pageSet = Set.fromList update
        relevantRules = filter (\(x,y) -> Set.member x pageSet && Set.member y pageSet) rules
        posMap = Map.fromList $ zip update [0..]
    in all (\(x,y) -> (posMap Map.! x) < (posMap Map.! y)) relevantRules

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

topoSort :: Rules -> Update -> Maybe Update
topoSort rules update =
    let pageSet = Set.fromList update
        relevantRules = filter (\(x,y) -> Set.member x pageSet && Set.member y pageSet) rules
        adjMap = Map.fromListWith Set.union [(x, Set.singleton y) | (x, y) <- relevantRules]
        nodes = Set.toList pageSet
        inDegrees = Map.fromList [(n, 0) | n <- nodes]
        inDegrees' = foldl' (\m (_, y) -> Map.insertWith (+) y 1 m) inDegrees relevantRules
        process [] sorted _ =
            if length sorted == length nodes
                then Just (reverse sorted)
                else Nothing
        process available sorted degrees =
            case available of
                [] -> Nothing
                (n:ns) ->
                    let sorted' = n : sorted
                        neighbors = Set.toList $ Map.findWithDefault Set.empty n adjMap
                        (degrees', available') = foldl' updateNeighbor (degrees, ns) neighbors
                    in process available' sorted' degrees'
        updateNeighbor (deg, avail) m =
            let deg' = Map.adjust (subtract 1) m deg
                degValue = deg' Map.! m
            in if degValue == 0
                then (deg', avail ++ [m])
                else (deg', avail)
        initialAvailable = [n | n <- nodes, Map.findWithDefault 0 n inDegrees' == 0]
    in process initialAvailable [] inDegrees'

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f xs = [ y | Just y <- map f xs ]

part1 :: Input -> Int
part1 (rules, updates) =
    let correctUpdates = filter (isCorrect rules) updates
        middlePages = map middle correctUpdates
    in sum middlePages

part2 :: Input -> Int
part2 (rules, updates) =
    let correctUpdates = filter (isCorrect rules) updates
        incorrectUpdates = filter (not . isCorrect rules) updates
        correctedUpdates = mapMaybe (topoSort rules) incorrectUpdates
        middlePages = map middle correctedUpdates
    in sum middlePages

main :: IO ()
main = do
    testInput <- readInput "../inputs/day05_test.txt"
    putStrLn "Test Input Results:"
    putStrLn $ "Part 1: " ++ show (part1 testInput)
    putStrLn $ "Part 2: " ++ show (part2 testInput)

    input <- readInput "../inputs/day05.txt"
    putStrLn "Real Input Results:"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

