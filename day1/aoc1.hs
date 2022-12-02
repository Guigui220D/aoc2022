import System.Environment
import Data.List
import Data.List.Split

parse :: String -> Int
parse line = if (length line) == 0
    then 0
    else read line

toInts :: [String] -> [Int]
toInts lines = map (parse) lines

squash :: [Int] -> [Int]
squash scores = do
    map (sum) (splitOn [0] scores) 

top3best :: [Int] -> Int
top3best squashes = sum $ take 3 (sortOn (\x -> negate x) squashes)

main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let linesOfFile = lines content
   let squashes = squash (toInts linesOfFile)
   putStrLn (show (maximum squashes))
   putStrLn (show (top3best squashes))

-- Guillaume DEREX