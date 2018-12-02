getInput = do
  contents <- getContents
  return $ map parseLine (lines contents)

parseLine line = line

processInput input = input

main = do
  input <- getInput
  print $ processInput input
