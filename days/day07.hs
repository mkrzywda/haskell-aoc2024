import qualified Data.Set as Set
import System.Environment (getArgs)

readInput :: FilePath -> IO [(Int, [Int])]
readInput path = do
    content <- readFile path
    return $ map parseLine (lines content)
  where
    parseLine :: String -> (Int, [Int])
    parseLine line =
        let (targetPart:valuesPart) = words (map (\c -> if c == ':' then ' ' else c) line)
        in (read targetPart, map read valuesPart)

generateResults :: [Int] -> Set.Set Int
generateResults [] = Set.empty
generateResults [x] = Set.singleton x
generateResults xs = foldl applyOperators (Set.singleton (head xs)) (tail xs)
  where
    applyOperators :: Set.Set Int -> Int -> Set.Set Int
    applyOperators acc x = Set.fromList $ concatMap (\a -> [a + x, a * x]) (Set.toList acc)

canAchieveTarget :: Int -> [Int] -> Bool
canAchieveTarget target values = Set.member target (generateResults values)

part1 :: [(Int, [Int])] -> Int
part1 equations =
    sum [target | (target, values) <- equations, canAchieveTarget target values]

generateResultsWithConcat :: [Int] -> Set.Set Int
generateResultsWithConcat [] = Set.empty
generateResultsWithConcat [x] = Set.singleton x
generateResultsWithConcat xs = foldl applyOperators (Set.singleton (head xs)) (tail xs)
  where
    applyOperators :: Set.Set Int -> Int -> Set.Set Int
    applyOperators acc x = Set.fromList $ concatMap (\a -> [a + x, a * x, concatNumbers a x]) (Set.toList acc)
    
    concatNumbers :: Int -> Int -> Int
    concatNumbers a b = read (show a ++ show b) :: Int

canAchieveTargetWithConcat :: Int -> [Int] -> Bool
canAchieveTargetWithConcat target values = Set.member target (generateResultsWithConcat values)

part2 :: [(Int, [Int])] -> Int
part2 equations =
    sum [target | (target, values) <- equations, canAchieveTargetWithConcat target values]

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

