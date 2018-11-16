import Control.Applicative (liftA2)
import Data.Char (isAlpha, isDigit, isSpace)


(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)


ignoreUnknown = filter (isAlpha <||> isDigit <||> isSpace)

main :: IO ()
main = do
  let input = "1*2&aZ"
  print $ "\"" ++ input ++ "\"" ++ " -> " ++ "\"" ++ (ignoreUnknown input) ++ "\""
