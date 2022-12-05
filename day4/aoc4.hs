import System.Environment

-- maps a function to tuple elements
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

-- made it myself :)
-- splits a string at a selected char
splitOn :: Char -> String -> (String, String)
splitOn _ "" = ("", "")
splitOn sep (x:xs) = if (x == sep)
    then ([], xs)
    else do
        let next = splitOn sep xs
        (x : fst next, snd next)

-- parse a file line into 4 ints
parse :: String -> ((Int, Int), (Int, Int))
parse line = do
    let ranges = splitOn ',' line
    let litterals = mapTuple (splitOn '-') ranges
    mapTuple (mapTuple read) litterals

-- checks if the first range contains the second one
includes :: (Int, Int) -> (Int, Int) -> Bool
includes bigger smaller = 
    (fst bigger <= fst smaller) && 
    (snd bigger >= snd smaller)

-- checks if a pair of ranges is redundant (one contains the other)
areRedundant :: ((Int, Int), (Int, Int)) -> Bool
areRedundant ranges = 
    includes (fst ranges) (snd ranges) ||
    includes (snd ranges) (fst ranges)

contains :: Int -> (Int, Int) -> Bool
contains x range = 
    fst range <= x && 
    snd range >= x

-- thats probably terrible
-- checks if a pair of ranges overlaps
overlaps :: ((Int, Int), (Int, Int)) -> Bool
overlaps ranges = 
    contains (fst (snd ranges)) (fst ranges) ||
    contains (snd (snd ranges)) (fst ranges) ||
    contains (fst (fst ranges)) (snd ranges)

main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let linesOfFile = lines content
    putStrLn $ show (length $ filter (areRedundant) $ map (parse) linesOfFile)
    putStrLn $ show (length $ filter (overlaps) $ map (parse) linesOfFile)

-- Guillaume DEREX