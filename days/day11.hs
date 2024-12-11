import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Char (isSpace)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    let testPath = if length args > 0 then args !! 0 else "../inputs/day11_test.txt" 
    let realPath = if length args > 1 then args !! 1 else "../inputs/day11.txt" 

    testInput <- readInput testPath
    putStrLn "Test Input Results:"
    putStrLn $ "After 6 blinks: " ++ show (countStones testInput 6)
    putStrLn $ "After 25 blinks: " ++ show (countStones testInput 25)
    putStrLn $ "After 75 blinks: " ++ show (countStones testInput 75)

    input <- readInput realPath
    putStrLn "Real Input Results:"
    putStrLn $ "After 6 blinks: " ++ show (countStones input 6)
    putStrLn $ "After 25 blinks: " ++ show (countStones input 25)
    putStrLn $ "After 75 blinks: " ++ show (countStones input 75)

readInput :: FilePath -> IO [Int]
readInput path = do
    content <- readFile path
    let tokens = words content
    let numbers = map readMaybe tokens :: [Maybe Int]
    return $ [n | Just n <- numbers]

countStones :: [Int] -> Int -> Integer
countStones initial n = total
  where
    initialMap = foldl' (\acc num -> Map.insertWith (+) num 1 acc) Map.empty initial
    finalMap = iterateBlink n initialMap
    total = sum (Map.elems finalMap)

iterateBlink :: Int -> Map.Map Int Integer -> Map.Map Int Integer
iterateBlink 0 current = current
iterateBlink blinks current = iterateBlink (blinks - 1) next
  where
    next = Map.foldlWithKey' processStone Map.empty current

processStone :: Map.Map Int Integer -> Int -> Integer -> Map.Map Int Integer
processStone acc num cnt
    | num == 0 = Map.insertWith (+) 1 cnt acc
    | even (length (show num)) =
        let [left, right] = splitNumber num
        in Map.insertWith (+) left cnt (Map.insertWith (+) right cnt acc)
    | otherwise = Map.insertWith (+) (num * 2024) cnt acc

splitNumber :: Int -> [Int]
splitNumber num =
    let s = show num
        len = length s
        half = len `div` 2
        leftStr = take half s
        rightStr = drop half s
        left = if null leftStr then 0 else read leftStr :: Int
        right = if null rightStr then 0 else read rightStr :: Int
    in [left, right]

