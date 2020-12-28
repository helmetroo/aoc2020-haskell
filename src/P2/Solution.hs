module P2.Solution(countValidPasswords) where

import P2.PasswordTypes(Policy(..))

countValidPasswords passwords policies =
  foldl accumulateValidPassCount 0 (zip passwords policies)

accumulateValidPassCount count (password, policy) =
  if validate policy password then count + 1 else count
