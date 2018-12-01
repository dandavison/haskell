import System.IO (getLine, isEOF)


-- Read list of integers from stdin
getValues :: [Integer] -> IO [Integer]
getValues values = do
  eof <- isEOF
  if eof
  then return values
  else do
    line <- getLine
    values' <- getValues (values ++ [parseLine line])
    return values'


getValues' :: IO [Integer]
getValues' = do
  contents <- getContents
  return $ map parseLine (lines contents)


parseLine :: String -> Integer
parseLine ('+':xs) = read xs :: Integer
parseLine xs = read xs :: Integer


main :: IO ()
main = do
  -- values <- getValues []
  values <- getValues'
  print $ foldr (+) 0 values
