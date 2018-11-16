module Bob (
  responseFor
) where

import Data.Char (isLower, isSpace, isUpper)
import Data.List (isSuffixOf)


data Utterance  = ShoutedQuestion
  | Question
  | Shout
  | Silence
  | OtherUtterance


responseFor :: String -> String
responseFor = response . parseUtterance
  where
    response ShoutedQuestion = "Calm down, I know what I'm doing!"
    response Question = "Sure."
    response Shout = "Whoa, chill out!"
    response Silence = "Fine. Be that way!"
    response OtherUtterance = "Whatever."


parseUtterance :: String -> Utterance
parseUtterance rawString
  | isSilence s = Silence
  | isShout s && isQuestion s = ShoutedQuestion
  | isQuestion s = Question
  | isShout s = Shout
  | otherwise = OtherUtterance
  where
    s = preprocess rawString
    preprocess = filter (not . isSpace)
    isSilence = all isSpace
    isShout cs = any isUpper cs && all (not . isLower) cs
    isQuestion = isSuffixOf "?"
