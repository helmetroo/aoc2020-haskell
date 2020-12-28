module P2.PasswordFileReader(
  readPasswordsAndPolicies
  ) where

import System.IO

import P2.PasswordTypes(
  Password,
  TokenizedPolicy,
  PasswordAndTokenizedPolicy,
  tokenizePolicy
  )
import SolutionUtils.Splitter(splitByDash)

readPasswordsAndPolicies :: String -> IO ([Password], [TokenizedPolicy])
readPasswordsAndPolicies fileName = do
  readLines <- fmap lines $ readFile fileName
  return $ unzip $ map lineToPasswordAndPolicy readLines

lineToPasswordAndPolicy :: String -> PasswordAndTokenizedPolicy
lineToPasswordAndPolicy line =
  let splitLine = words line
      policy = tokenizePolicy splitLine
      password = extractPassword splitLine
  in (password, policy)

extractPassword :: [String] -> Password
extractPassword splitLine = splitLine !! 2
