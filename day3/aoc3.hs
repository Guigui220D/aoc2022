import System.Environment
import Data.Char

compartments :: String -> (String, String)
compartments rucksack = 
    splitAt (div (length rucksack) 2) rucksack

commonItems :: (String, String) -> String
commonItems (a,b) = filter (\x -> elem x a) b

commonItemsSeveral :: [String] -> String
commonItemsSeveral [] = ""
commonItemsSeveral [x] = x
commonItemsSeveral [x,y] = commonItems (x, y)
commonItemsSeveral (a:b:l) = commonItemsSeveral ((commonItems (a,b)) : l)

take3by3 :: [String] -> [[String]]
take3by3 [] = []
take3by3 (a:b:c:l) = [a, b, c] : (take3by3 l)

priority :: Char -> Int
priority item = do
    let o = ord item
    if (o >= 97)
        then o - 96
        else o - 65 + 27

main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let linesOfFile = lines content
    putStrLn $ show $ sum (map (priority . head . commonItems . compartments) linesOfFile)
    putStrLn $ show $ sum (map (priority . head . commonItemsSeveral) (take3by3 linesOfFile))

-- Guillaume DEREX