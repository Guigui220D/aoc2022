import System.Environment
import Data.Char

compartments :: String -> (String, String)
compartments runsack = 
    splitAt (div (length runsack) 2) runsack

itemInBoth :: (String, String) -> Char
itemInBoth (compA, compB) = 
    head $ head $ filter (\x -> (length x /= 0)) $ (map (\a -> (filter (\b -> a==b) compB)) compA)

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
    putStrLn $ show $ sum (map (priority . itemInBoth . compartments) linesOfFile)

-- Guillaume DEREX