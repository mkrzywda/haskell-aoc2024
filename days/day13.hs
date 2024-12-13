import System.Environment (getArgs)
import System.IO (readFile)
import Data.List (minimumBy, find)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import Control.Monad (guard)
import Text.Read (readMaybe)
import Data.Char (isDigit)

data Machine = Machine {
    aX :: Int,
    aY :: Int,
    bX :: Int,
    bY :: Int,
    pX :: Int,
    pY :: Int
} deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    let testPath = if length args > 0 then args !! 0 else "../inputs/day13_test.txt" 
    let realPath = if length args > 1 then args !! 1 else "../inputs/day13.txt" 

    testContent <- readFile testPath
    let testMachines = parseInput testContent
    putStrLn "Test Input Parsed Machines:"
    mapM_ print testMachines
    let testResultPart1 = computeTotalMinTokensPart1 testMachines
    putStrLn "Test Input Results:"
    putStrLn $ "Total Tokens Spent (Part 1): " ++ show testResultPart1

    realContent <- readFile realPath
    let realMachines = parseInput realContent
    putStrLn "\nReal Input Parsed Machines:"
    mapM_ print realMachines
    let realResultPart1 = computeTotalMinTokensPart1 realMachines
    putStrLn "Real Input Results:"
    putStrLn $ "Total Tokens Spent (Part 1): " ++ show realResultPart1

    let adjustedTestMachines = adjustPrizePositions testMachines
    let testResultPart2 = computeTotalMinTokensPart2 adjustedTestMachines
    putStrLn "\nTest Input Results (Part 2):"
    putStrLn $ "Total Tokens Spent (Part 2): " ++ show testResultPart2

    let adjustedRealMachines = adjustPrizePositions realMachines
    let realResultPart2 = computeTotalMinTokensPart2 adjustedRealMachines
    putStrLn "Real Input Results (Part 2):"
    putStrLn $ "Total Tokens Spent (Part 2): " ++ show realResultPart2

parseInput :: String -> [Machine]
parseInput content = mapMaybe parseMachine (splitMachines $ lines content)

splitMachines :: [String] -> [[String]]
splitMachines [] = []
splitMachines ls = let (group, rest) = break null ls 
                  in if null group 
                     then splitMachines (dropWhile null rest)
                     else group : splitMachines (dropWhile null rest)

parseMachine :: [String] -> Maybe Machine
parseMachine [lineA, lineB, lineP] = do
    (aDx, aDy) <- parseButton lineA
    (bDx, bDy) <- parseButton lineB
    (pXVal, pYVal) <- parsePrize lineP
    return $ Machine aDx aDy bDx bDy pXVal pYVal
parseMachine _ = Nothing

parseButton :: String -> Maybe (Int, Int)
parseButton line = do
    let parts = words line
    xPart <- lookupPart "X" parts
    yPart <- lookupPart "Y" parts
    xVal <- parseCoord xPart
    yVal <- parseCoord yPart
    return (xVal, yVal)

parsePrize :: String -> Maybe (Int, Int)
parsePrize line = do
    let parts = words line
    xPart <- lookupPart "X=" parts
    yPart <- lookupPart "Y=" parts
    xVal <- readMaybe (filter isDigit xPart) :: Maybe Int
    yVal <- readMaybe (filter isDigit yPart) :: Maybe Int
    return (xVal, yVal)

lookupPart :: String -> [String] -> Maybe String
lookupPart prefix partsList = fmap (drop (length prefix)) $ find (\s -> take (length prefix) s == prefix) partsList

parseCoord :: String -> Maybe Int
parseCoord s = readMaybe (filter (\c -> isDigit c || c == '-') s) :: Maybe Int

adjustPrizePositions :: [Machine] -> [Machine]
adjustPrizePositions = map adjustMachine
    where
        adjustMachine m = m { pX = pX m + 10000000000000, pY = pY m + 10000000000000 }

computeTotalMinTokensPart1 :: [Machine] -> Int
computeTotalMinTokensPart1 machines = sum $ mapMaybe minCostPart1 machines

computeTotalMinTokensPart2 :: [Machine] -> Int
computeTotalMinTokensPart2 machines = sum $ mapMaybe minCostPart2 machines

minCostPart1 :: Machine -> Maybe Int
minCostPart1 machine = 
    let possibleCosts = [ 3 * a + b 
                        | a <- [0..100], 
                          b <- [0..100],
                          aX machine * a + bX machine * b == pX machine,
                          aY machine * a + bY machine * b == pY machine
                        ]
    in if null possibleCosts then Nothing else Just (minimum possibleCosts)

minCostPart2 :: Machine -> Maybe Int
minCostPart2 machine = 
    let a1 = aX machine
        b1 = bX machine
        a2 = aY machine
        b2 = bY machine
        c = pX machine
        d = pY machine
        delta = b2 * a1 - a2 * b1
    in if delta == 0 then
           if (d * a1 - a2 * c) == 0 then
               let possibleBs = [ b | b <- [0..100000], 
                                       let aNumerator = c - b1 * b,
                                       aNumerator >=0,
                                       a1 /=0,
                                       aNumerator `mod` a1 ==0,
                                       let a = aNumerator `div` a1,
                                       a >=0,
                                       aY machine * a + bY machine * b == d
                                     ]
                   possibleCosts = [3 * a + b | b <- possibleBs, let a = (c - b1 * b) `div` a1]
               in if null possibleCosts then Nothing else Just (minimum possibleCosts)
           else
               Nothing
       else
           let numerator = d * a1 - a2 * c
           in if numerator `mod` delta /= 0 then Nothing else
               let b = numerator `div` delta
               in if b < 0 then Nothing else
                   let aNumerator = c - b1 * b
                       aDenominator = a1
                   in if aDenominator == 0 then Nothing else
                       if aNumerator `mod` aDenominator /= 0 then Nothing else
                           let a = aNumerator `div` aDenominator
                           in if a < 0 then Nothing else
                               if a2 * a + b2 * b /= d then Nothing else
                                   Just (3 * a + b)


