import System.Environment

data Shape = Rock | Paper | Scissors deriving (Enum, Eq, Show)
data Outcome = Lose | Draw | Win deriving (Enum, Eq, Show)

shapeFromChar :: Char -> Shape
shapeFromChar 'A' = Rock
shapeFromChar 'B' = Paper
shapeFromChar 'C' = Scissors
shapeFromChar 'X' = Rock
shapeFromChar 'Y' = Paper
shapeFromChar 'Z' = Scissors
shapeFromChar other = error "Unexpected shape code"

outcomeFromChar :: Char -> Outcome
outcomeFromChar 'X' = Lose
outcomeFromChar 'Y' = Draw
outcomeFromChar 'Z' = Win
outcomeFromChar other = error "Unexpected outcome code"

selectWithOutcome :: Outcome -> Shape -> Shape
selectWithOutcome Draw shape = shape
selectWithOutcome Win shape = case (shape) of
    Rock -> Paper
    Paper -> Scissors
    Scissors -> Rock
selectWithOutcome Lose shape = case (shape) of
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper

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

interpretLine2 :: String -> (Shape, Shape)
interpretLine2 line = do
    let shape = shapeFromChar (line !! 0)
    (shape, selectWithOutcome (outcomeFromChar (line !! 2)) shape)

totalMatchScore :: (Shape, Shape) -> Int
totalMatchScore (him, you) =
    (matchScore him you) + (shapeScore you)

doAllMatches :: [String] -> [Int]
doAllMatches matches =
    map (totalMatchScore . interpretLine) matches

doAllMatches2 :: [String] -> [Int]
doAllMatches2 matches =
    map (totalMatchScore . interpretLine2) matches

test = unlines ["A Y", "B X", "C Z"] 

main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let linesOfFile = lines content
    --let linesOfFile = lines test
    putStrLn $ show (sum $ doAllMatches linesOfFile)
    putStrLn $ show (sum $ doAllMatches2 linesOfFile)

-- Guillaume DEREX