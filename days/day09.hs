import System.Environment (getArgs)

readInput :: FilePath -> IO String
readInput path = do
    content <- readFile path
    return $ head $ lines content

parseDiskMap :: String -> ([Int], [Int])
parseDiskMap inputLine =
    let ds = map (read . (:[])) inputLine
        fileLens = [ ds!!i | i <- [0,2..length ds-1], i < length ds ]
        freeLens = [ ds!!i | i <- [1,3..length ds-1], i < length ds ]
    in (fileLens, freeLens)

expandDisk :: ([Int],[Int]) -> [Int]
expandDisk (fileLens, freeLens) = go fileLens freeLens 0
  where
    go [] [] _ = []
    go (f:fs) [] fid = replicate f fid ++ go fs [] (fid+1)
    go (f:fs) (r:rs) fid = replicate f fid ++ replicate r (-1) ++ go fs rs (fid+1)
    go [] (r:rs) _ = replicate r (-1) ++ go [] rs (-1)

hasGap :: [Int] -> Bool
hasGap disk =
    let lastFilePos = last [i | (i,v) <- zip [0..] disk, v /= -1]
    in any (\(i,v)->v == -1 && i < lastFilePos) (zip [0..] disk)

doOneMove :: [Int] -> [Int]
doOneMove disk =
    let leftmostGap = head [i | (i,v) <- zip [0..] disk, v == -1]
        rightmostFilePos = last [i | (i,v) <- zip [0..] disk, v /= -1]
        fileID = disk !! rightmostFilePos
    in disk
       & replaceIdx leftmostGap fileID
       & replaceIdx rightmostFilePos (-1)

(&) :: a -> (a -> b) -> b
x & f = f x

replaceIdx :: Int -> Int -> [Int] -> [Int]
replaceIdx i val xs = take i xs ++ [val] ++ drop (i+1) xs

compactByMoves :: [Int] -> [Int]
compactByMoves disk
    | hasGap disk = compactByMoves (doOneMove disk)
    | otherwise   = disk

computeChecksum :: [Int] -> Int
computeChecksum disk = sum [ i * v | (i,v) <- zip [0..] disk, v /= -1 ]

part1 :: String -> Int
part1 line =
    let parsed   = parseDiskMap line
        expanded = expandDisk parsed
        final    = compactByMoves expanded
    in computeChecksum final


part2 :: String -> Int
part2 line =
    let (fileLens, freeLens) = parseDiskMap line
        disk = expandDisk (fileLens, freeLens)
        fileCount = length fileLens
        finalDisk = compactByWholeFileMoves disk (fileCount - 1)
    in computeChecksum finalDisk

compactByWholeFileMoves :: [Int] -> Int -> [Int]
compactByWholeFileMoves disk (-1) = disk
compactByWholeFileMoves disk fid =
    let newDisk = tryMoveFile disk fid
    in compactByWholeFileMoves newDisk (fid - 1)

tryMoveFile :: [Int] -> Int -> [Int]
tryMoveFile disk fid =
    let fileBlocks = [ i | (i,v) <- zip [0..] disk, v == fid ]
        fileLen = length fileBlocks
    in if fileLen == 0
       then disk
       else
         let leftmostFileBlock = minimum fileBlocks
             maybeSegment = findFreeSegmentToLeft disk fileLen leftmostFileBlock
         in case maybeSegment of
              Nothing -> disk
              Just startIdx ->
                  let clearedDisk = map (\v -> if v == fid then (-1) else v) disk
                  in placeFile clearedDisk fid startIdx fileLen

findFreeSegmentToLeft :: [Int] -> Int -> Int -> Maybe Int
findFreeSegmentToLeft disk needed leftmostFileBlock =
    let freeSegments = findAllFreeSegments disk
        suitableSegments = filter (\(start,len) -> len >= needed && (start + len) <= leftmostFileBlock) freeSegments
    in case suitableSegments of
         [] -> Nothing
         ((start,_):_) -> Just start

findAllFreeSegments :: [Int] -> [(Int,Int)]
findAllFreeSegments disk = go 0 disk
  where
    go _ [] = []
    go idx xs =
      if head xs == (-1)
      then let segLen = length (takeWhile (==(-1)) xs)
           in (idx, segLen) : go (idx + segLen) (drop segLen xs)
      else go (idx+1) (tail xs)

placeFile :: [Int] -> Int -> Int -> Int -> [Int]
placeFile disk fid start fileLen =
    take start disk ++ replicate fileLen fid ++ drop (start + fileLen) disk

main :: IO ()
main = do
    args <- getArgs
    let testPath = if length args > 0 then args !! 0 else "../inputs/day09_test.txt"
    let realPath = if length args > 1 then args !! 1 else "../inputs/day09.txt"

    testInput <- readInput testPath
    putStrLn "Test Input Results:"
    putStrLn $ "Part 1: " ++ show (part1 testInput)
    putStrLn $ "Part 2: " ++ show (part2 testInput)

    input <- readInput realPath
    putStrLn "Real Input Results:"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

