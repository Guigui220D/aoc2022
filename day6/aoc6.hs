hasRepeatingChar :: String -> Bool
hasRepeatingChar [] = False
hasRepeatingChar (x:xs) = 
  if (elem x xs)
  then True
  else hasRepeatingChar xs

findNonRepeatingSequence :: Int -> String -> Int
findNonRepeatingSequence count buffer =
  if (hasRepeatingChar $ take count buffer)
  then 1 + findNonRepeatingSequence count (tail buffer)
  else count

findSOP = findNonRepeatingSequence 4
findSOM = findNonRepeatingSequence 14

main = do
  args <- getArgs
  input <- readFile (args !! 0)
  putStrLn $ show $ findSOP input
  putStrLn $ show $ findSOM input

-- Guillaume DEREX
