import System.Environment
import Data.List

data Op = NoOp | AddX Int

-- Parse a string into an Op
parseOp :: String -> Op
parseOp line =
    if isPrefixOf "noop" line
    then NoOp
    else AddX (read (drop 5 line))

-- Puts noops between ops that take several cycles
-- So that 1 op represents 1 cycle in the output
extendOps :: [Op] -> [Op]
extendOps [] = []
extendOps (x:xs) = case x of
    (NoOp) -> NoOp : extendOps xs
    (AddX i) -> NoOp : (AddX i) : extendOps xs

-- Applies an operations to regx
applyOp :: Int -> Op -> Int
applyOp regx (NoOp) = regx
applyOp regx (AddX val) = regx + val

-- Applies a series of Ops to regx
applyManyOps :: Int -> [Op] -> Int
applyManyOps regx ops =
    foldl (applyOp) regx ops

signalStrenghts :: Int -> [Int] -> [Op] -> [Int]
signalStrenghts regx stops [] = []
signalStrenghts regx stops ops =
    let n = (stops !! 1) - (stops !! 0)
        newregx = applyManyOps regx (take n ops)
    in (newregx * (stops !! 1)) : (signalStrenghts newregx (tail stops) (drop n ops))


main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let ops = NoOp : (extendOps $ map (parseOp) $ lines input)
    let stops = 0 : [20, 60, 100, 140, 180, 220, 221, length ops]
    let strenghts = take 6 $ signalStrenghts 1 stops ops
    putStrLn $ show $ strenghts
    putStrLn $ show $ sum strenghts