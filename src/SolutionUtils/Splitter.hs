module SolutionUtils.Splitter where

split :: (Char -> Bool) -> String -> [String]
split p s =
  case dropWhile p s of
    "" -> []
    s' -> w : split p s''
      where (w, s'') = break p s'

splitByDash = split (=='-')
splitByColon = split (==':')
splitBySpace = split (==' ')
splitByColonIntoPair str =
  let splitString = splitByColon str
  in (splitString !! 0, splitString !! 1)
