import System.Environment

data Shape = Rock | Paper | Scissors deriving (Enum, Eq, Show)

shapeFromChar :: Char -> Shape
shapeFromChar 'A' = Rock
shapeFromChar 'B' = Paper
shapeFromChar 'C' = Scissors
shapeFromChar 'X' = Rock
shapeFromChar 'Y' = Paper
shapeFromChar 'Z' = Scissors
shapeFromChar other = error "Unexpected shape code"

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

matchScore :: Shape -> Shape -> Int
matchScore him you = if (you == him)
    then 3
    else case (you) of
        Rock -> if (him == Scissors) then 6 else 0
        Paper -> if (him == Rock) then 6 else 0
        Scissors -> if (him == Paper) then 6 else 0

interpretLine :: String -> (Shape, Shape)
interpretLine line = (
    shapeFromChar (line !! 0),
    shapeFromChar (line !! 2))

totalMatchScore :: (Shape, Shape) -> Int
totalMatchScore (him, you) =
    (matchScore him you) + (shapeScore you)

doAllMatches :: [String] -> [Int]
doAllMatches matches =
    map (totalMatchScore . interpretLine) matches

main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let linesOfFile = lines content
    putStrLn $ show (sum $ doAllMatches linesOfFile)

-- Guillaume DEREX