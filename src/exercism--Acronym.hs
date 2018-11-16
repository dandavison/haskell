module Acronym (abbreviate) where

import  Data.Char (toUpper)
import Text.Regex (mkRegex, subRegex)


replacePunctuationWithSpace :: String -> String
replacePunctuationWithSpace s = map substitute s where
  substitute c = if c `elem` ",-" then ' ' else c


breakCamelCase :: String -> String
breakCamelCase s = subRegex (mkRegex "(.+)([a-z])([A-Z])(.+)") s "\\1\\2 \\3\\4"


abbreviate :: String -> String
abbreviate = map (toUpper . head) . words . (replacePunctuationWithSpace . breakCamelCase)
