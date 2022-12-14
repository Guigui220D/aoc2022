import System.Environment
import Data.List.Split

type Crate = Char
type Pile = [Crate]
type Warehouse = [Pile]

type Instruction = (Int, Int, Int)

-- starts with 1 because first stack is 1
-- applies a function to the nth element of a list
applyToNth :: Int -> (a -> a) -> [a] -> [a]
applyToNth n f l =
    zipWith (\x y -> if (y == n) then (f x) else x) l [1..]

-- adds a crate to a pile of crates
putCrate :: Crate -> Pile -> Pile
putCrate c p = c : p

-- says what the top crates of a warehouse are
-- used for challenge return code
topCrates :: Warehouse -> [Char]
topCrates w = map (head) w

-- probably a bit clumsy
-- use vectors?
-- applies an instruction to manipulate the crates with the crane
useCrane9000 :: Warehouse -> Instruction -> Warehouse
useCrane9000 w (0, _, _) = w
useCrane9000 w (count, from, to) = do
    let next = (useCrane9000 w (count - 1, from, to))
    let crate = head (next !! (from - 1))
    let putted = applyToNth to (putCrate crate) next
    let taken = applyToNth from (tail) putted
    taken

useCrane9001 :: Warehouse -> Instruction -> Warehouse
useCrane9001 w (0, _, _) = w
useCrane9001 w (count, from, to) = do
    let crates = take count (w !! (from - 1))
    let putted = applyToNth to (crates ++) w
    let taken = applyToNth from (drop count) putted
    taken

-- applies a list of instructions to the crane
followInstructions9000 :: [Instruction] -> Warehouse -> Warehouse
followInstructions9000 list w = foldl (useCrane9000) w list

-- applies a list of instructions to the crane
followInstructions9001 :: [Instruction] -> Warehouse -> Warehouse
followInstructions9001 list w = foldl (useCrane9001) w list

-- adds a new crate line to the crates
stackCrate :: [Crate] -> Warehouse -> Warehouse
stackCrate c w = zipWith (\x y -> if (y /= ' ') then y:x else x) w c

-- parses a line to only get the crates
-- surely there's a better way to do it
parseCrateLine :: String -> [Crate]
parseCrateLine line = filter (/= '_') $ zipWith (\x i -> if (mod i 4 == 1) then x else '_') line [0..]

-- parses the beginning of the file to get the crate stacks
parseWarehouse :: (Int, Int) -> [String] -> Warehouse
parseWarehouse (piles, height) lines = do
    let initial = (replicate piles "")
    foldr (stackCrate) initial (map (parseCrateLine) (take height lines))

-- Parse the file for instructions
parseInstructions :: [String] -> [Instruction]
parseInstructions lines = do
    let words_lines = map (splitOn " ") lines
    let good_word_lines = filter (\x -> length (x !! 0) == 4) $ filter (\x -> length x == 6) words_lines
    let start_with_m = filter (\x -> x !! 0 !! 0 == 'm') good_word_lines
    map (\x -> (read (x !! 1), read (x !! 3), read (x !! 5))) start_with_m


main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let linesOfFile = lines content
    let warehouse = parseWarehouse (9, 8) linesOfFile
    let instructions = parseInstructions linesOfFile
    --putStrLn $ show $ instructions
    --putStrLn $ show $ warehouse
    putStrLn $ show $ topCrates $ followInstructions9000 instructions warehouse
    putStrLn $ show $ topCrates $ followInstructions9001 instructions warehouse
    

-- Guillaume DEREX