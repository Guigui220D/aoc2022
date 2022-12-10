import System.Environment
import Data.List
import Data.List.Split

data Node = Dir String [Node] | File String Integer deriving Show
data Line = CdDown String | CdUp | Ls | LsResult Node deriving Show

freeSize :: Node -> Integer
freeSize fs = 70000000 - (size fs)

sizeToFree :: Node -> Integer
sizeToFree fs = 30000000 - (freeSize fs)

size :: Node -> Integer
size (File _ filesize) = filesize
size (Dir _ contents) = sum $ map (size) contents

sizeSelective :: Node -> Integer
sizeSelective (File _ filesize) = filesize
sizeSelective (Dir name contents) = 
    if s < 100000 then s else 0 
    where s = size (Dir name contents)

sizeSelectiveRecursive :: Node -> Integer
sizeSelectiveRecursive (File _ _) = 0
sizeSelectiveRecursive (Dir name contents) = 
    (sum $ map (sizeSelectiveRecursive) contents) + 
    sizeSelective (Dir name contents)

isDeleteCandidate :: Integer -> Node -> Bool
isDeleteCandidate tofree (File _ _) = False
isDeleteCandidate tofree (Dir name contents) = 
    (size (Dir name contents)) >= tofree

-- UGLYYY
smallestDeletableDirSize :: Integer -> Node -> Integer
smallestDeletableDirSize tofree (File _ _) = 99999999999999
smallestDeletableDirSize tofree (Dir name []) = 99999999999999
smallestDeletableDirSize tofree (Dir name contents) = 
    let sub = map (smallestDeletableDirSize tofree) contents in
        minimum (
            if isDeleteCandidate tofree (Dir name contents)
            then size (Dir name contents) : sub 
            else sub)

parseCommand :: String -> Line
parseCommand line = 
    if isPrefixOf "cd" line
    then let path = drop 3 line in
        case path of
            ".." -> CdUp
            _ -> CdDown path
    else Ls

parseListResult :: String -> Node
parseListResult line =
    if isPrefixOf "dir" line
    then Dir (drop 4 line) []
    else File (words !! 1) (read (words !! 0))
        where words = splitOn " " line

parseLine :: String -> Line
parseLine line = 
    if head line == '$'
    then parseCommand (drop 2 line)
    else LsResult (parseListResult line)

-- This has an issue: it ignores when something already exists
-- Since a dir can be added in a dir for two reasons (going down or listing)
-- there can be duplicates
-- this is not a problem for the size calculation because the listed dirs are empty
addToDir :: Node -> Node -> Node
addToDir node (Dir name contents) = 
    (Dir name (node : contents))
addToDir _ (File _ _) = error "cannot put inside file"

doCommand :: [Node] -> String -> [Node]
doCommand stack command =
    case parseLine command of
        (CdDown path) -> (Dir path []) : stack
        (CdUp) -> folded : drop 2 stack where
            folded = addToDir (head stack) (stack !! 1)
        (Ls) -> stack
        (LsResult node) -> added : tail stack where
            added = addToDir node (head stack) 

parseAllLines :: [String] -> Node
parseAllLines lines = 
    let stack = foldl (doCommand) [] lines in
    foldl1 (addToDir) stack

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let commands = map (parseLine) $ lines input
    let result = parseAllLines $ lines input
    --putStrLn $ show commands
    --putStrLn $ show result
    putStrLn $ show (size result)
    putStrLn $ show (sizeSelectiveRecursive result)
    putStrLn $ show (smallestDeletableDirSize (sizeToFree result) result)