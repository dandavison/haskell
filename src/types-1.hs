-- declare the type: the set of all lists containing ints
x :: [Int]
x = [1, 1]

data IntList = IntList [Int]
  deriving Show

y :: IntList
y = IntList [2, 2]

IntList myList = y
IntList [i, j] = y

main :: IO ()
main = do
  print x
  print y
  print myList
  print i
  print j
