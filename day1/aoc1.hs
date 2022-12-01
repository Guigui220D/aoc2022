import System.Environment
import Data.List

parse :: String -> Integer
parse line = if (length line) == 0
    then 0
    else read line

toInts :: [String] -> [Integer]
toInts lines = map (parse) lines

-- This isn't ideal, it sums all consecutive groups of numbers but sometimes starts midway
-- but it doesn't matter, since starting midway would only give a smaller number
-- and here we want the maximum
squash :: [Integer] -> [Integer]
squash [] = []
squash scores = if (head scores /= 0)
    then sum (takeWhile (\x -> x /= 0) scores) : squash (tail scores)
    else squash (tail scores)

bestSquash :: [Integer] -> Integer
bestSquash scores = maximum (squash scores)

-- Actually it WOULD matter here but still, it got the right answer
top3bestSquash :: [Integer] -> Integer
top3bestSquash scores = sum (take 3 (sortOn (\x -> negate x) (squash scores)))

main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let linesOfFile = lines content
   putStrLn (show (bestSquash (toInts linesOfFile)))
   putStrLn (show (top3bestSquash (toInts linesOfFile)))

-- Guillaume DEREX